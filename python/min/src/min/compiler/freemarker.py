from min.utils import Transformer, EnumVisitor
from min.utils.adt import Enum
from min.utils.extend import activate
from min.utils.functional import IterExtensions
from min.parser import ASTNode
import logging

logger = logging.getLogger(__name__)
activate(IterExtensions)

class MarkedNode(Enum):
    Block(list)
    Call(MarkedNode, list)
    Name(str)
    Number(float)
    String(str)
    Function(list, list, MarkedNode)
    If(MarkedNode, MarkedNode, MarkedNode)
    BuiltinConst(str)
    Assign(str, MarkedNode)
    Rec(list)
    
class MarkedTransformer(Transformer(ASTNode, MarkedNode)):
    def __init__(self):
        self.env = {}
    
    def visit_Function(self, argnames, body):
        body = self.visit(body)
        trans = FunctionTransformer(argnames)
        trans.visit(body)
        func = MarkedNode.Function(trans.free_vars(), argnames, body)
        return func
        
class FunctionTransformer(EnumVisitor):
    def __init__(self, argnames, env=None):
        if env is None:
            env = {}
        self.env = env
        self.env.update(dict(zip(argnames, [True]*len(argnames))))
    
    def free_vars(self):
        return [key for key, value in self.env.items() if not value]
    
    def visit_default(self, value):
        pass
    
    def visit_Name(self, name):
        if name not in self.env:
            logger.debug('Found free variable: %s.', name)
            self.env[name] = False
        
    def visit_Assign(self, name, value):
        self.visit(value)
        if name not in self.env:
            self.env[name] = True
    
    def visit_Block(self, values):  
        values.map(self.visit).list()
    
    def visit_Call(self, func, args):
        self.visit(func)
        args.map(self.visit).list()
        
    def visit_Rec(self, args):
        args.map(self.visit).list()
            
    def visit_If(self, cond, ifbody, elsebody):
        self.visit(cond)
        self.visit(ifbody)
        self.visit(elsebody)
    
    def visit_Function(self, freevars, argnames, body):
        for var in freevars:
            if var not in self.env:
                self.env[var] = False
                
def transform(val):
    logger.info('Marking free variables in AST.')
    logger.debug('AST: %s', val)
    return MarkedTransformer().visit(val)