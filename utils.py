'''
utils.py
Utility functions.
'''

import sys

def warn(w):
  sys.stderr.write("WARNING--" + w + "\n")

def err(e):
  sys.stderr.write("ERROR--" + e + "\n")

class SymbolInstantiationError(Exception):
  pass

def symbol(string):
  """
  Overwrites the decorated class' __init__ and __str__ functions: the new
  __init__ always raises a SymbolInstantiationError, while the new __str__
  returns the string given to this decorator as an argument.
  """
  def wrap(cls):
    def __init__(self):
      raise SymbolInstantiationError(
        "Class {} is a symbol and can't be instantiated.".format(cls.__name__)
      )
    def __str__(self):
      return string
    cls.__init__ = __init__
    cls.__str__ = __str__
    return cls
  return wrap

class SingletonInstantiationError(Exception):
  pass

def singleton(cls):
  """
  Overwrites the decorated class' __init__ function with a version that raises
  a SingletonInstantiationError if called more than once. Arguments to the init
  function are unaffected (i.e. passed through to the old __init__).
  """
  instantiated = False
  baseinit = cls.__init__
  def __init__(self, *args, **kwargs):
    nonlocal instantiated
    if instantiated:
      raise SingletonInstantiationError(
        "Can't instantiate a second instance of singleton Class {}.".format(
          cls.__name__
        )
      )
    else:
      baseinit(self, *args, **kwargs)
      instantiated = True

  cls.__init__ = __init__
  return cls

@singleton
class NoRepr:
  """
  An object that returns '' for both str() and repr().
  """
  def __str__(self): return ""
  def __repr__(self): return ""

norepr = NoRepr()
