from min.utils.extend import Extension, extension, method_on
import types
import functools

@extension
class IterExtensions(Extension):
    @method_on(list, tuple, map, filter, range, types.GeneratorType, name='map')
    def map_(self, func):
        return map(func, self)
    
    @method_on(list, tuple, map, filter, range, types.GeneratorType, name='filter')
    def filter_(self, func):
        return filter(func, self)
    
    @method_on(list, tuple, map, filter, range, types.GeneratorType, name='list')
    def list_(self):
        return list(self)
        
    @method_on(list, tuple, map, filter, range, types.GeneratorType, name='reduce')
    def reduce_(self, func, initializer=None):
        return functools.reduce(func, self, initializer)
    
    @method_on(list, tuple, map, filter, range, types.GeneratorType, name='spread')
    def spread(self, func):
        return func(*self)

@extension    
class ObjectExtensions(Extension):
    @method_on(object, name='apply')
    def apply(self, func):
        return func(self)
    
    @method_on(object, name='const')
    def const(self):
        return lambda: self
        
    @method_on(object, name='getattr')
    def getattr_(self, name):
        return getattr(self, name)
        
    @method_on(object, name='setattr')
    def setattr_(self, name, value):
        return setattr(self, name, value)
        
@extension      
class FunctionExtensions(Extension):
    @method_on(types.FunctionType, types.LambdaType, types.MethodType, 
               type(print), functools.partial, name='partial')
    def partial(self, *args, **kwargs):
        return functools.partial(self, *args, **kwargs)
    
    @method_on(types.FunctionType, types.LambdaType, types.MethodType, 
               type(print), functools.partial, name='curry')
    def curry(self, unique=True):
        return _curry_helper(self, unique=unique)
    
    @method_on(types.FunctionType, types.LambdaType, types.MethodType, 
               type(print), functools.partial, name='compose')
    def compose(self, other):
        return _compose_helper(other, self)

@extension  
class NumberExtensions(Extension):
    @method_on(int, name='times')
    def times(self, func):
        return (func() for _ in range(self))

AllExtensions = [
    IterExtensions,
    FunctionExtensions,
    ObjectExtensions,
    NumberExtensions
]

def _compose_helper(f, g):
    def _inner(*args, **kwargs):
        return f(g(*args, **kwargs))
    return _inner

def _curry_helper(func, unique=True, minArgs=None):
    def g(*myArgs, **myKwArgs):
        def f(*args, **kwArgs):
            if args or kwArgs:                  
                newArgs = myArgs + args
                newKwArgs = dict.copy(myKwArgs)
 
                if unique and not kwArgs.keys().isdisjoint(newKwArgs):
                    raise ValueError("Repeated keyword argument in curried function while unique is set to True!")
 
                newKwArgs.update(kwArgs)
 
                # Checks whether it's time to evaluate func.
                if minArgs is not None and minArgs <= len(newArgs) + len(newKwArgs):
                    return func(*newArgs, **newKwArgs)
                else:
                    return g(*newArgs, **newKwArgs)
            else:
                return func(*myArgs, **myKwArgs)
        return f
    return g
 
    