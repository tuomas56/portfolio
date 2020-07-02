from min.utils.adt import Enum, instanceattribute
from min.utils.functional import AllExtensions
from min.utils.extend import activate
import logging

logger = logging.getLogger(__name__)
activate(*AllExtensions)

class MinValue(Enum):
    Number(float)
    String(str)
    Nothing()
    Environment(dict)
    Function(object, MinValue)
    PythonFunction(object)
    
    @instanceattribute
    def truthy(self):
        if isinstance(self, MinValue.Number):
            val, = self
            if val == 0:
                return False
        return True
    
    @instanceattribute
    def __str__(self):
        if isinstance(self, MinValue.Nothing):
            return "None"
        elif isinstance(self, (MinValue.Number, MinValue.String)):
            val, = self
            return str(val)
        elif isinstance(self, MinValue.Environment):
            return "#env"
        elif isinstance(self, MinValue.Function):
            return "#function"
        elif isinstance(self, MinValue.PythonFunction):
            return "#primitive"
    
    @staticmethod
    def get_builtin_const(name):
        if name == 'None':
            return MinValue.Nothing()
        elif name == 'True':
            return MinValue.Number(1.0)
        elif name == 'False':
            return MinValue.Number(0.0)
        else:
            raise NameError("No such constant %s!" % name)
            