from min.utils.functional import AllExtensions
from min.utils.extend import activate
from min.objspace import MinValue
from min.compiler import compile
from min.compiler.linearize import Function
from min.interpreter import run_function
import operator
import logging

logger = logging.getLogger(__name__)
activate(*AllExtensions)


@MinValue.PythonFunction
def print_function(interp, *args):
    print(*args.map(str))
    return MinValue.Nothing()

def numeric_function(name, func):
    @MinValue.PythonFunction
    def _func(interp, arg1, arg2):
        print(arg1, arg2)
        if not isinstance(arg1, MinValue.Number) or \
           not isinstance(arg2, MinValue.Number):
            raise TypeError("Numeric function %s requires two numeric arguments!" % name)
        (val1,), (val2,) = arg1, arg2
        return MinValue.Number(func(val1, val2))
    return _func

@MinValue.PythonFunction
def eq_function(interp, arg1, arg2):
    if arg1 == arg2:
        return MinValue.get_builtin_const("True")
    else:
        return MinValue.get_builtin_const("False")
    
@MinValue.PythonFunction
def neq_function(interp, arg1, arg2):
    if arg1 != arg2:
        return MinValue.get_builtin_const("True")
    else:
        return MinValue.get_builtin_const("False")

@MinValue.PythonFunction
def str_function(interp, arg):
    return MinValue.String(str(arg))

prelude = {
    'print': print_function,
    '+': numeric_function('+', operator.add),
    '-': numeric_function('-', operator.sub),
    '*': numeric_function('*', operator.mul),
    '/': numeric_function('/', operator.truediv),
    '>': numeric_function('>', operator.gt),
    '<': numeric_function('<', operator.lt),
    '>=': numeric_function('>=', operator.ge),
    '<=': numeric_function('<=', operator.le),
    '==': eq_function,
    '!=': neq_function,
    'True': MinValue.get_builtin_const("True"),
    'False': MinValue.get_builtin_const("False"),
    'str': str_function
}