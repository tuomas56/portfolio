from min.utils.functional import AllExtensions
from min.utils.extend import activate
from min.utils.adt import Enum
from funcparserlib.lexer import make_tokenizer
from funcparserlib.parser import *
import re

activate(*AllExtensions)


def spec(name, regex, flags=None):
    return (name, (regex, flags)) if flags else (name, (regex,))

token_specs = [
    spec(
        'keyword',
        r'((if)|(then)|(def)|(do)|(else)|(end)|(rec)|\{|\}|\[|\]|\(|\)|(\-\>)|\,|(\:\=)|\;)'),
    spec(
        'ws',
        r'\s+'),
    spec(
        'nl',
        r'\r|\n|\r\n'),
    spec(
        'numl',
        r'-?[0-9]+(\.[0-9]+)?'),
    spec(
        'strl',
        r'\"[^\"]*\"'),
    spec(
        'ident',
        r'[A-Za-z\$\.\*\@\~\<\>\^\+\-\/\=\:\`\!]'
        r'[A-Za-z_0-9\$\.\*\@\!\~\<\>\^\+\-\/\=\:\`]*')
]


token_types = [spec[0] for spec in token_specs]

ignore_tokens = ['ws', 'nl']

tokenizer = make_tokenizer(token_specs)

def remove_comments(s):
    return re.sub('#.*$', '', s, flags=re.MULTILINE)


def tokenize(s):
    s = remove_comments(s)
    return tokenizer(s).filter(lambda x: x.type not in ignore_tokens).list()

def tokval(t):
    return t.value

def tok(type, value=None):
    if value is None:
        return some(lambda t: t.type == type)
    else:
        return some(lambda t: t.type == type and t.value == value)

for type in token_types:
    globals()[type] = tok.partial(type)

fwd = forward_decl

class ASTNode(Enum):
    Block(list)
    Call(ASTNode, list)
    Name(str)
    Number(float)
    String(str)
    Function(list, ASTNode)
    If(ASTNode, ASTNode, ASTNode)
    BuiltinConst(str)
    Assign(str, ASTNode)
    Rec(list)

expr = fwd()

block = (skip(keyword('do')) + many(expr + skip(keyword(';'))) + skip(keyword('end'))
      |  skip(keyword('{') ) + many(expr + skip(keyword(';'))) + skip(keyword('}') )) >> ASTNode.Block

def parse_arglist(val):
    if val is None:
        return []
    else:
        return [val[0]] + val[1]

name = ident() >> tokval >> ASTNode.Name

number = numl() >> tokval >> float >> ASTNode.Number

string = strl() >> tokval >> (lambda s: s[1:-1]) >> ASTNode.String

brackets = skip(keyword('(')) + expr + skip(keyword(')'))

def parse_namedfunction(val):
    func = ASTNode.Function(val[1], val[2])
    func.name = val[0]
    return ASTNode.Assign(val[0], func)

funarglist = maybe((ident() >> tokval) + many(skip(keyword(',')) + (ident() >> tokval))) >> parse_arglist
anonfunction = skip(keyword('def')) + skip(keyword('(')) + funarglist + skip(keyword(')')) + expr 
anonfunction >>= tuple.spread.partial(func=ASTNode.Function)
namedfunction = skip(keyword('def')) + (ident() >> tokval) + skip(keyword('(')) + funarglist + skip(keyword(')')) + expr
namedfunction >>= parse_namedfunction
function = namedfunction | anonfunction

def parse_call(val):
    stem = val[0]
    for seg in val[1]:
        stem = ASTNode.Call(stem, seg)
    return stem

callarglist = maybe(expr + many(skip(keyword(',')) + expr)) >> parse_arglist
call_ = skip(keyword('(')) + callarglist + skip(keyword(')'))
rec = skip(keyword('rec')) + call_ >> ASTNode.Rec
noncall = name | number | string | function | block | brackets | rec
call = noncall + many(call_) >> parse_call

def parse_binop(val):
    lhs = val[0]
    for op, rhs in val[1]:
        opname, = op
        if re.match(r'[\$\.\*\@\~\<\>\^\+\-\/\=\:\!]+', opname):
            lhs = ASTNode.Call(op, [lhs, rhs])
        elif opname.startswith('`') and opname.endswith('`'):
            lhs = ASTNode.Call(ASTNode.Name(opname[1:-1]), [lhs, rhs])
        else:
            raise SyntaxError("Operators must be all symbols, not '%s'!" % opname)
    return lhs
    
nonbinop =  call | name | number | string | function | block | brackets
binop = nonbinop + many(name + nonbinop) >> parse_binop

def parse_if(val):
    val = list(val)
    if val[2] == None:
        val[2] = ASTNode.BuiltinConst('None')
    return ASTNode.If(*val)

if_ = skip(keyword('if')) + expr + skip(keyword('then')) +  expr + maybe(skip(keyword('else')) + expr) 
if_ >>= parse_if

assign = (ident() >> tokval) + skip(keyword(':=')) + expr >> tuple.spread.partial(func=ASTNode.Assign)

expr.define(assign | binop | name | number | string | function | block | if_)

toplevel = expr + skip(finished)

def parse(s):
    return toplevel.parse(tokenize(s))