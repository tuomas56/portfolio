import abc
import inspect
import builtins
import contextlib

#EXAMPLES AT THE BOTTOM
#WARNING: This is extremely buggy.
#I apologise in advance for the sheer number of
#coding guidelines I have broken in writing this.

def makevariant(name, tys, dct):
    class _variant:
        __name__ = name
        _name = name
        def __init__(self, *args):
            self.__class__.__name__ = name
            if len(args) != len(tys):
                raise TypeError("Not the right amount of args for %s! Should be: %s" % (name,len(tys)))
            for arg, ty in zip(args, tys):
                try:
                    isinstance(None, ty)
                except TypeError:
                    if ty is not _self:
                        raise TypeError("%s is not a type and cannot be used in a Enum definition." % ty)
                if ty is _self:
                    if arg.__par != self.__par:
                        raise TypeError("Wrong type for argument, got %s, should be %s." % (type(arg), ty))
                else:
                    if not isinstance(arg, ty):
                        raise TypeError("Wrong type for argument '%r', got type %s, should be %s." % (arg, type(arg), ty))
            self.items = args

        @classmethod
        def _EnumMeta__set_par(cls, par):
            cls.__par = par
            par.register(cls)

        def __iter__(self):
            return iter(self.items)

        def __repr__(self):
            return "%s(%s)" % (self._name, ', '.join(map(repr, self.items)))

        def __str__(self):
            def pprint(val, indent):
                s = []
                if isinstance(val, Enum):
                    s.append('\n' + indent + type(val).__name__ + '(')
                    for child in val.items:
                        s.append(pprint(child, indent + '    ') + ', ')
                    s[-1] = s[-1][:-2]
                    s.append(')')
                elif isinstance(val, list):
                    s.append('\n' + indent + '[')
                    for child in val:
                        s.append(pprint(child, indent + '    ') + ', ')
                    s[-1] = s[-1][:-2]
                    s.append(']')
                else:
                    s.append(str(val))
                return ''.join(s)
            return pprint(self, '    ')

        def __eq__(self, other):
            if hasattr(other, 'items'):
                return self.__class__ == other.__class__ and self.items == other.items
            else:
                return False
    dct[name] = _variant
    dct._vrts.append(name)

_self = object()

class EnumMeta(abc.ABCMeta):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def __prepare__(name, bases):
        prev_frame = inspect.currentframe().f_back
        globs = prev_frame.f_globals
        locs = prev_frame.f_locals
        class dct(dict):
            def __init__(self, *args, **kwargs):
                self._vrts = []
                super().__init__(*args, **kwargs)

            def __missing__(self, key):
                if key == name:
                    return _self
                if key.startswith('_'):
                    return lambda *args: makevariant(key[1:], args, self)
                if key in locs:
                    return locs[key]
                if key in globs:
                    return globs[key]
                if key in dir(builtins):
                    return getattr(builtins, key)
                return lambda *args: makevariant(key, args, self)
        return dct()

    def __new__(cls, name, bases, dct):
        obj = super().__new__(cls, name, bases, dct)
        methods = {k: v for k, v in dct.items() if hasattr(v, '__inst')}
        for vn in dct._vrts:
            v = getattr(obj, vn)
            v.__set_par(obj)
            for name, met in methods.items():
                setattr(v, name, met)
        setattr(obj, '_vrts', dct._vrts)
        return obj


#Sum Types, Tagged Unions, Enums, etc. Allows declaring of variants like this:
#class Test(Enum):
#  Variant(int)
#  Variant2(str)
#When creating use Test.Variant or:
#with use(Test):
#  v = Variant(10)
#Values in variant constructors _are_ type-checked so, since generics are not yet implemented, use `object` for type vars.
class Enum(metaclass=EnumMeta):
    pass

#wrap any methods or attributes that you want the enum variants to have with this
def instanceattribute(f):
    setattr(f, '__inst', None)
    return f

#Take the variants of an enum and make them global during this context manager.
@contextlib.contextmanager
def use(*names):
    for var in names:
        globals().update({vn: getattr(var, vn) for vn in var._vrts})
    yield
    for var in names:
        for vn in var._vrts:
            del globals()[vn]

#basic pattern matching
#options is a dict of the form
#class: lambda
#if the value is an instance of class, the value will be provided to lambda as *args (it should be an iterator)
#eg)
#class Test(Enum):
#  Thing(int)
#  Other(int, str)
#match(Test.Thing(2), {
#    Test.Thing: lambda ival: print("Got:", ival),
#    Test.Other: lambda ival, sval: print("Got:", ival, "and", sval)
#})
def match(value, options):
    for opt, f in options.items():
        if isinstance(value, opt):
            return f(*value)

#basic example
#class Maybe(Enum):
#	Nothing()
#	Some(object)

#n = Maybe.Nothing()
#s = Maybe.Some(10)

#v, = s
#unpack using iter
#print(v)
#prints 10

#match(n, {
#	Maybe.Nothing: lambda: print("nothing"),
#	   Maybe.Some: lambda val: print("got: ", val)
#})
#basic matching
#the values of the enum are provided as arguments to the function.

#multiple fields example
#class Test(Enum):
#	Variant(int, str)
#	Variant2(bool)

#ival, sval = Test.Variant(10, 'hi')
#bval, = Test.Variant2(True)

#print(ival, sval, bval)

#rust's option example
class Option(Enum):
    Nothing()
    Some(object)

    @instanceattribute
    def expect(self, reason):
        if isinstance(self, Option.Nothing):
            raise RuntimeError(reason)
        else:
            val, = self
            return val

#use is a context manager which temporarily 
#puts all the variants of the chosen enum into scope.
#you can use more than one like so:
#with use(Option, Result):
#with use(Option):
#	Some(10).expect("AAHAH!")
#	#returns 10
    #Nothing().expect("AAHAH!")
    #raises RuntimeError("AAHAH!")
#	v = Some(10)
#	match(v, {
#		   Some: lambda x: print(x),
#		Nothing: lambda: print("Oh noes!")
#	})
    #prints 10
    
#rust's result example
class Result(Enum):
    Ok(object)
    Err(object)

    @instanceattribute
    def unwrap(self):
        if isinstance(self, Result.Err):
            err, = self
            raise err
        else:
            val, = self
            return val

#@use(Result)
#def test(x) -> Result:
 # if 0 < x < 10:
  #  return Ok(x)
  #else:
   # return Err(ValueError("Value out of range!"))

#print(test(5).unwrap())
#prints 5
#test(11).unwrap()
#raises ValueError("Value out of range!")
