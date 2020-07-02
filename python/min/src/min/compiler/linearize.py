from min.utils import Transformer, gensym
from min.utils.adt import Enum, instanceattribute
from min.compiler.anftransformer import ANFNode
from min.utils.extend import activate
from min.utils.functional import IterExtensions
from collections import namedtuple
import logging

logger = logging.getLogger(__name__)
activate(IterExtensions)

Module = namedtuple('Module', 'functions')
Function = namedtuple('Function', 'free_vars argnames blocks')

class Block:
    def __init__(self, name):
        self.name = name
        self.body = []
    
    def __str__(self):
        return 'Label "%s"' % self.name
    
    def __repr__(self):
        return '%s: %s' % (self.name, '\n  '.join(map(str, self.body)))

class LinearNode(Enum):
    Call(LinearNode, list)
    Name(str)
    Argument(str)
    Number(float)
    String(str)
    Branch(LinearNode, Block, Block)
    BuiltinConst(str)
    Assign(str, LinearNode)
    FunctionRef(str, LinearNode)
    EnvironmentRef(str)
    MakeEnvironment(list)
    LetVar(str, LinearNode)
    LetRef(str)
    Return(LinearNode)
    Rec(list)

    @instanceattribute
    def is_simple(self):
        return isinstance(self, (LinearNode.Name,
                                 LinearNode.Argument,
                                 LinearNode.Number,
                                 LinearNode.String,
                                 LinearNode.LetRef,
                                 LinearNode.EnvironmentRef,
                                 LinearNode.BuiltinConst))

class LinearTransformer(Transformer(ANFNode, LinearNode)):
    def __init__(self, func):
        self.func = func
        self.blocks = {'entry': Block('entry')}
        self.block = self.blocks['entry']
        self.term = True
        self.block.body.append(
            self.process_ret(self.visit(func.body)))

    def process_ret(self, val):
        if isinstance(val, LinearNode.Branch) or not self.term:
            logger.debug('Path continues, no return.')
            return val
        else:
            logger.debug('Path end detected; return introduced.')
            if not val.is_simple():
                valname = gensym('return')
                logger.debug('Let-expr introduced in return.')
                letval = LinearNode.LetVar(valname, val)
                self.block.body.append(letval)
                val = LinearNode.LetRef(valname)
            return LinearNode.Return(val)

    def visit_Block(self, body):
        old_term = self.term
        self.term = False
        vals = body.map(self.visit).list()
        self.block.body.extend(vals[:-1])
        val = self.process_ret(vals[-1])
        self.term = old_term
        return val
    
    def visit_If(self, cond, ifbody, elbody):
        ifblock = Block(gensym('if_block'))
        elblock = Block(gensym('el_block'))
        old_block = self.block
        logger.debug('Introducing block for if-expr-if.')
        self.block = ifblock
        val = self.process_ret(self.visit(ifbody))
        self.block.body.append(val)
        logger.debug('Introducing block for if-expr-else.')
        self.block = elblock
        val = self.process_ret(self.visit(elbody))
        self.block.body.append(val)
        logger.debug('Returning to original block after if-expr.')
        self.block = old_block
        self.blocks[ifblock.name] = ifblock
        self.blocks[elblock.name] = elblock
        cond = self.visit(cond)
        return LinearNode.Branch(cond, ifblock, elblock)
    
    def visit_LetVar(self, name, value, expr):
        logger.debug('Linearizing let-expr.')
        def recur(name, value, expr):
            print(name, value, expr)
            if isinstance(value, ANFNode.LetVar):
                name2, value2, value = value
                value_new = self.visit(value)
                value2 = recur(name2, value2, value)
                #self.block.body.append(LinearNode.LetVar(name2, value2))
                self.block.body.append(LinearNode.LetVar(name, value_new))
                return self.visit(expr)
            else:
                value = self.visit(value)
                self.block.body.append(LinearNode.LetVar(name, value))
                return self.visit(expr)
        return recur(name, value, expr)

def transform(mod):
    logger.info('Linearizing module.')
    logger.debug('Module functions:')
    for name, func in mod.functions.items():
        free_vars, argnames, body = func
        logger.debug('%s:\n    Function(\n        %s,%s)', name, argnames, body)
    new_mod = Module({})
    for name, func in mod.functions.items():
        logger.debug('Transforming function %s.', name)
        trans = LinearTransformer(func)
        new_func = Function(
            func.free_vars,
            func.argnames,
            trans.blocks
        )
        new_mod.functions[name] = new_func
    return new_mod