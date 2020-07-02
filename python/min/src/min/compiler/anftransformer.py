from min.utils import Transformer, gensym
from min.utils.adt import Enum, instanceattribute
from min.compiler.envtransformer import LiftedNode
from min.utils.extend import activate
from min.utils.functional import IterExtensions
from collections import namedtuple
import logging

logger = logging.getLogger(__name__)
activate(IterExtensions)

Module = namedtuple('Module', 'functions')
Function = namedtuple('Function', 'free_vars argnames body')

class ANFNode(Enum):
    Block(list)
    Call(ANFNode, list)
    Name(str)
    Argument(str)
    Number(float)
    String(str)
    If(ANFNode, ANFNode, ANFNode)
    BuiltinConst(str)
    Assign(str, ANFNode)
    FunctionRef(str, ANFNode)
    EnvironmentRef(str)
    MakeEnvironment(list)
    LetVar(str, ANFNode, ANFNode)
    LetRef(str)
    Rec(list)
    
    @instanceattribute
    def is_simple(self):
        return isinstance(self, (ANFNode.Name,
                                 ANFNode.Argument,
                                 ANFNode.Number,
                                 ANFNode.String,
                                 ANFNode.LetRef,
                                 ANFNode.EnvironmentRef,
                                 ANFNode.BuiltinConst))

class ANFTransformer(Transformer(LiftedNode, ANFNode)):
    def visit_Call(self, func, args):
        func = self.visit(func)
        args = args.map(self.visit).list()
        if func.is_simple():
            call = ANFNode.Call(func, args)
        else:
            logger.debug('Introducing let-expr in call-func for %s.', func)
            funcname = gensym("func")
            call = ANFNode.LetVar(
                    funcname, func,
                    ANFNode.Call(
                        ANFNode.LetRef(funcname), args
                    ))
        
        for i, arg in list(enumerate(args))[::-1]:
            if arg.is_simple():
                continue
            else:
                logger.debug('Introducing let-expr in call-arg for %s.', arg)
                argname = gensym("arg")
                call = ANFNode.LetVar(
                    argname, arg, call
                )
                args[i] = ANFNode.LetRef(argname)
        return call
    
    def visit_If(self, cond, ifbody, elsebody):
        cond = self.visit(cond)
        ifbody = self.visit(ifbody)
        elsebody = self.visit(elsebody)
        if cond.is_simple():
            if_ = ANFNode.If(cond, ifbody, elsebody)
        else:
            logger.debug('Introducing let-expr in if for %s.', cond)
            condname = gensym("cond")
            if_ = ANFNode.LetVar(
                condname, cond, ANFNode.If(
                    ANFNode.LetRef(condname), ifbody, elsebody
                )
            )
        return if_
    
    def visit_Assign(self, name, value):
        value = self.visit(value)
        if value.is_simple():
            assign = ANFNode.Assign(name, value)
        else:
            logger.debug('Introducing let-expr in assign-val for %s.', value)
            valuename = gensym("value")
            assign = ANFNode.LetVar(
                valuename, value, ANFNode.Assign(
                    name, ANFNode.LetRef(valuename)
                )
            )
        return assign
    
    def visit_FunctionRef(self, name, env):
        env = self.visit(env)
        envname = gensym("env")
        return ANFNode.LetVar(
            envname, env, ANFNode.FunctionRef(
                name, ANFNode.LetRef(envname)
            )
        )

Module = namedtuple('Module', 'functions')
Function = namedtuple('Function', 'free_vars argnames body')

def transform(mod):
    logger.info('Transforming module to A-Normal Form.')
    logger.debug('Module functions:')
    for name, func in mod.functions.items():
        free_vars, argnames, body = func
        logger.debug('%s:\n    Function(\n        %s,%s)', name, argnames, body)
    new_mod = Module({})
    for name, func in mod.functions.items():
        logger.debug('Transforming function %s.', name)
        trans = ANFTransformer()
        new_func = Function(
            func.free_vars,
            func.argnames,
            trans.visit(func.body)
        )
        new_mod.functions[name] = new_func
    return new_mod