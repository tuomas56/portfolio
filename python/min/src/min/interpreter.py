from min.utils.functional import AllExtensions
from min.utils.extend import activate
from min.utils import EnumVisitor, expect
from min.objspace import MinValue
import logging

logger = logging.getLogger(__name__)
activate(*AllExtensions)

class FuncInterpreter(EnumVisitor):
    def __init__(self, func, module, builtins, env, args):
        self.func = func
        self.module = module
        self.env = env
        self.args = args
        self.builtins = builtins.copy()
        self.vars = builtins.copy()
        self.lets = {}
        self.ret_val = None
    
    def visit_Name(self, name):
        if name not in self.vars:
            raise NameError("Variable %s does not exist!" % name)
        else:
            return self.vars[name]
    
    def visit_Assign(self, name, value):
        self.vars[name] = self.visit(value)
        return self.vars[name]
    
    def visit_Argument(self, name):
        if name not in self.args:
            raise NameError("Argument %s does not exist!" % name)
        else:
            return self.args[name]
    
    
    def visit_EnvironmentRef(self, name):
        if name not in self.env:
            raise NameError("Variable %s does not exist!" % name)
        else:
            return self.env[name]
    
    def visit_LetRef(self, name):
        if name not in self.lets:
            raise NameError("Internal Error: let-ref %s does not exist!" % name)
        else:
            return self.lets[name]
    
    def visit_LetVar(self, name, value):
        self.lets[name] = self.visit(value)
        return self.lets[name]
        
    def visit_Number(self, value):
        return MinValue.Number(value)
    
    def visit_String(self, value):
        return MinValue.String(value)
    
    def visit_BuiltinConst(self, value):
        return MinValue.get_builtin_const(value)
    
    def visit_Return(self, value):
        self.ret_val = self.visit(value)
        return self.ret_val
   
    def visit_MakeEnvironment(self, names):
        ac_names = [name for name, in names]
        names = dict(zip(ac_names, names.map(self.visit).list()))
        return MinValue.Environment(names)
    
    def visit_FunctionRef(self, name, env):
        return MinValue.Function(
            self.module.functions[name], 
            self.visit(env)
        )
    
    def visit_Branch(self, cond, first, other):
        if self.visit(cond).truthy():
            return self.visit_block(first)
        else:
            return self.visit_block(other)
    
    def visit_block(self, block):
        for expr in block.body[:-1]:
            self.visit(expr)
        return self.visit(block.body[-1])
    
    def visit_Call(self, func, args):
        args = args.map(self.visit).list()
        func = self.visit(func)
        if not isinstance(func, (MinValue.Function, MinValue.PythonFunction)):
            raise TypeError("Can't call a non function: %s." % func)
        elif isinstance(func, MinValue.Function):
            func_actual, (env,) = func
            return run_function(func_actual, self.module, {}, env, args)
        else:
            func_actual, = func
            return func_actual(self, *args)
     
    def visit_Rec(self, args):
        args = args.map(self.visit).list()
        run_function(self.func, self.module, self.builtins, self.env, args)
    
def run_function(func, module, builtins, env, args):
    expect(all(k in func.free_vars for k in env) and
           all(k in env for k in func.free_vars),
        "Got free wrong free vars in function!")
    args = dict(zip(func.argnames, args))
    expect(len(args) == len(func.argnames),
        "Got wrong number of args in function!")
    interp = FuncInterpreter(func, module, builtins, env, args)
    interp.visit_block(func.blocks['entry'])
    return interp.ret_val
    
def run(module, builtins):
    return run_function(module.functions['main'], module, builtins, {}, [])