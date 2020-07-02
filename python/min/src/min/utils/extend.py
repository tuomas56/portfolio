import contextlib
import ctypes
import functools
from types import MappingProxyType

@contextlib.contextmanager
def using(*exts):
    for ext in exts:
        ext.buildup()
    yield
    for ext in exts:
        ext.breakdown()

def activate(*exts):
    for ext in exts:
        ext.buildup()
    
def deactivate(*exts):
    for ext in exts:
        ext.breakdown()

# figure out side of _Py_ssize_t
if hasattr(ctypes.pythonapi, 'Py_InitModule4_64'):
    _Py_ssize_t = ctypes.c_int64
else:
    _Py_ssize_t = ctypes.c_int

# regular python
class _PyObject(ctypes.Structure):
    pass
_PyObject._fields_ = [
    ('ob_refcnt', _Py_ssize_t),
    ('ob_type', ctypes.POINTER(_PyObject))
]

# python with trace
if object.__basicsize__ != ctypes.sizeof(_PyObject):
    class _PyObject(ctypes.Structure):
        pass
    _PyObject._fields_ = [
        ('_ob_next', ctypes.POINTER(_PyObject)),
        ('_ob_prev', ctypes.POINTER(_PyObject)),
        ('ob_refcnt', _Py_ssize_t),
        ('ob_type', ctypes.POINTER(_PyObject))
    ]


class _DictProxy(_PyObject):
    _fields_ = [('dict', ctypes.POINTER(_PyObject))]


def reveal_dict(proxy):
    if not isinstance(proxy, MappingProxyType):
        raise TypeError('dictproxy expected')
    dp = _DictProxy.from_address(id(proxy))
    ns = {}
    ctypes.pythonapi.PyDict_SetItem(ctypes.py_object(ns),
                                    ctypes.py_object(None),
                                    dp.dict)
    return ns[None]


def get_class_dict(cls):
    d = getattr(cls, '__dict__', None)
    if d is None:
        raise TypeError('given class does not have a dictionary')
    if isinstance(d, MappingProxyType):
        return reveal_dict(d)
    return d
    
def extension(cls):
    for _, method in cls.__dict__.items():
        if hasattr(method, "use_class"):
            method(cls)
    return cls

    
def method_on(*classes, name=None):
    def _method(func):
        nonlocal name
        if name is None:
            name = func.__name__
        def make_ext_method(cls):
            x = cls
            def get_active():
                return x.is_active
            
            @functools.wraps(func)
            def _extension_method(self, *args, **kwargs):
                if get_active():
                    return func(self, *args, **kwargs)
                else:
                    raise AttributeError("'%s' object has no attribute '%s'" % (type(self).__name__, name))
            
            for cls in classes:
                get_class_dict(cls)[name] = _extension_method
        make_ext_method.use_class = True
        return make_ext_method
    return _method
    
class Extension:
    is_active = False
    
    @classmethod
    def buildup(cls):
        cls.is_active = True
        
    
    @classmethod 
    def breakdown(cls):
        cls.is_active = False