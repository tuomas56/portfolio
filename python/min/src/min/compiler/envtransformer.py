from min.utils import Transformer
from min.utils.adt import Enum
from min.utils.extend import activate
from min.utils.functional import IterExtensions
from min.compiler.makemodule import ModuleNode, Module
from collections import namedtuple
import logging

logger = logging.getLogger(__name__)
activate(IterExtensions)

class LiftedNode(Enum):
    Block(list)
    Call(LiftedNode, list)
    Name(str)
    Argument(str)
    Number(float)
    String(str)
    If(LiftedNode, LiftedNode, LiftedNode)
    BuiltinConst(str)
    Assign(str, LiftedNode)
    FunctionRef(str, LiftedNode)
    EnvironmentRef(str)
    MakeEnvironment(list)
    Rec(list)

class EnvTransformer(Transformer(ModuleNode, LiftedNode)):
    def __init__(self, func):
        self.free_vars, self.argnames, _ = func    
        self.env = {}
        self.env.update(dict(zip(self.free_vars, ["fv"]*len(self.free_vars))))
        self.env.update(dict(zip(self.argnames, ["arg"]*len(self.argnames))))
        
    def visit_Assign(self, name, value):
        value = self.visit(value)
        self.env[name] = "var"
        return LiftedNode.Assign(name, value)

    def visit_Name(self, name):
        if name in self.env and self.env[name] == "fv":
            logger.debug('Found environment reference, %s.', name)
            return LiftedNode.EnvironmentRef(name)
        elif name in self.env and self.env[name] == "arg":
            return LiftedNode.Argument(name)
        elif name in self.env and self.env[name] == "var":
            return LiftedNode.Name(name)
        elif name not in self.env:
            return LiftedNode.Name(name)
    
    def visit_FunctionRef(self, name, free_vars):
        logger.debug('Making environment with variables: %s', free_vars)
        env = LiftedNode.MakeEnvironment(free_vars
            .map(self.visit_Name)
            .list())
        return LiftedNode.FunctionRef(name, env)

Module = namedtuple('Module', 'functions')
Function = namedtuple('Function', 'free_vars argnames body')

def transform(mod):
    logger.info('Lifting environment in module.')
    logger.debug('Module functions:')
    for name, func in mod.functions.items():
        free_vars, argnames, body = func
        logger.debug('%s:\n    Function(\n        %s,\n        %s,%s)', name, free_vars, argnames, body)
    new_mod = Module({})
    for name, func in mod.functions.items():
        logger.debug('Lifting function %s.', name)
        trans = EnvTransformer(func)
        new_func = Function(
            func.free_vars,
            func.argnames,
            trans.visit(func.body)
        )
        new_mod.functions[name] = new_func
    return new_mod