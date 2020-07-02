from min.utils import Transformer, gensym
from min.utils.adt import Enum
from min.compiler.freemarker import MarkedNode
from min.utils.extend import activate
from min.utils.functional import IterExtensions
from collections import namedtuple
import logging

logger = logging.getLogger(__name__)
activate(IterExtensions)

Module = namedtuple('Module', 'functions')
Function = namedtuple('Function', 'free_vars argnames body')

class ModuleNode(Enum):
    Block(list)
    Call(ModuleNode, list)
    Name(str)
    Number(float)
    String(str)
    If(ModuleNode, ModuleNode, ModuleNode)
    BuiltinConst(str)
    Assign(str, ModuleNode)
    FunctionRef(str, list)
    Rec(list)
        
class ModuleTransformer(Transformer(MarkedNode, ModuleNode)):
    def __init__(self, mod=None):
        if mod is None:
            mod = Module({})
        self.mod = mod
    
    def visit_Function(self, free_vars, argnames, body):
        body = self.visit(body)
        name = gensym('function')
        logger.debug('Function lifted, id: %s, free variables: %s.', name, free_vars)
        self.mod.functions[name] = Function(free_vars, argnames, body)
        return ModuleNode.FunctionRef(name, free_vars)
        
def transform(node, mod=None):
    logger.info('Transforming marked AST to module.')
    logger.debug('Marked AST: %s %s', node, type(node).__name__)
    assert(isinstance(node, MarkedNode))
    trans = ModuleTransformer(mod)
    new_node = trans.visit(node)
    trans.mod.functions['main'] = Function([], [], new_node)
    logger.info('Transformed module, %s functions total.', len(trans.mod.functions))
    return trans.mod