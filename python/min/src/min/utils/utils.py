import inspect
import string
from collections import defaultdict

class Visitor:
    def transform(self, value):
        return (value,)

    def visit(self, value):
        name = type(value).__name__
        try:
            return getattr(self, 'visit_%s' % name)(*self.transform(value))
        except AttributeError:
            return self.visit_default(value)
    
    def visit_default(self, value):
        raise TypeError("Wrong type provided to visitor! %s" % (value,))
        
class EnumVisitor(Visitor):
    def transform(self, value):
        return iter(value)

def Transformer(from_, to_, *, auto_visit_compounds=True):
    class _transform(EnumVisitor):
        __name__ = 'Transformer[%s -> %s]' % (from_.__name__, to_.__name__)
        def transform(self, value):
            expect(isinstance(value, from_),
                "Value '%s' provided to %s should be an instance of %s, not %s!" %
                   (value, self.__name__, from_.__name__, type(value).__name__))
            return iter(value)
    
        def visit_default(self, value):
            expect(isinstance(value, from_),
                "Value '%s' provided to %s should be an instance of %s, not %s!" %
                   (value, self.__name__, from_.__name__, type(value).__name__))
            vrt_name = type(value).__name__
            expect(hasattr(to_, vrt_name),
                "Enum %s has no variant %s! Consider adding a specific case to %s." %
                   (to_.__name__, vrt_name, self.__name__))
            variant = getattr(to_, vrt_name)
            if auto_visit_compounds:
                new_vals = []
                for val in value:
                    if isinstance(val, from_):
                        new_vals.append(self.visit(val))
                    elif isinstance(val, (list, tuple)):
                        if all(isinstance(x, from_) for x in val):
                            new_vals.append([self.visit(x) for x in val])
                    else:
                        new_vals.append(val)
                return variant(*new_vals)
            else:
                return variant(*value)
    return _transform
            
        
def gensym(name='', *, counter=defaultdict(int)):
    counter[name] += 1
    return '%s_%s' % (name, counter[name])
    
def expect(condition, message):
    try:
        assert(condition)
    except AssertionError:
        raise AssertionError(message)
        
def interpolate(interp_str):
    caller_frame = inspect.currentframe().f_back
    caller_globals = caller_frame.f_globals
    caller_locals = caller_frame.f_locals
    caller_vars = dict(caller_globals, **caller_locals)
    return string.Template(interp_str).substitute(caller_vars)
    