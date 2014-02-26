'''
utils.py
Utility functions.
'''

import re
import os
import itertools

import sys

# 'symbol' and 'singleton' class decorators:

class SymbolInstantiationError(Exception):
  pass

class Symbol:
  def __init__(self, name, as_bool, **kwargs):
    self._name = name
    if as_bool in (False, True):
      self._as_bool = as_bool
    else:
      self._as_bool = True
    self.__dict__.update(kwargs)

  def __call__(self):
    raise SymbolInstantiationError(
      "{} is a Symbol and can't be instantiated.".format(self._name)
    )

  def __str__(self):
    return self._name

  def __bool__(self):
    return self._as_bool

def symbol(name, as_bool=True):
  """
  Replaces the decorated class with an instance of the Symbol class. If as_bool
  is given and either True or False then the __bool__ function of the created
  symbol will return the given value.
  """
  def wrap(cls):
    nonlocal name, as_bool
    return Symbol(name, as_bool, **cls.__dict__)
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

# Decorators for easy class construction:

def uniquely_defined_by(*attributes):
  """
  If a class is uniquely defined by some listing of its attributes, we can
  automatically create __hash__, __eq__, and __ne__ functions for it.
  """
  def decorate(cls):
    nonlocal attributes
    hash_expr = str(hash(cls.__name__))
    for a in attributes:
      hash_expr = '47 * ({}) + hash(self.{}) '.format(hash_expr, a)
    eq_expr = ' and '.join(
      "(self.{var} == other.{var})".format(var=a) for a in attributes
    )
    code = """\
def __hash__(self):
  return {hash_expr}

def __eq__(self, other):
  return type(self) == type(other) and {eq_expr}

def __ne__(self, other):
  return not self == other
""".format(
  hash_expr=hash_expr,
  eq_expr=eq_expr,
)
    exec(code, locals(), globals())
    cls.__hash__ = __hash__
    cls.__eq__ = __eq__
    cls.__ne__ = __ne__
    return cls
  return decorate

def attr_object(*args):
  """
  A "attr_object" is a class with the listed discrete instance attributes
  (each of which is optional) that is uniquely defined by those attributes.
  """
  if not args:
    raise ValueError("Tried to create attr_object class without any contents.")
  def decorate(cls):
    init_args = ', '.join("{}=None".format(a) for a in args)
    init_body = '\n  '.join("self.{var} = {var}".format(var=a) for a in args)
    code = """\
def __init__(self, {init_args}):
  {init_body}
""".format(
  init_args=init_args,
  init_body=init_body,
)
    exec(code, locals(), globals())
    cls.__init__ = __init__
    return uniquely_defined_by(*args)(cls)
  return decorate

# Miscellaneous classes:

@singleton
class NoRepr:
  """
  An object that returns '' for both str() and repr().
  """
  def __str__(self): return ""
  def __repr__(self): return ""

norepr = NoRepr()

@symbol("NotGiven", as_bool=False)
class NotGiven:
  """
  A symbol for use in default arguments where None is a valid argument.
  """
  pass

# Quoting and unquoting:

def quote(string):
  return '"' + string.replace("\\", "\\\\").replace('"', r'\"') + '"'

escape_sequence = re.compile(r'\\(["\\])')
def unquote(string):
  if string[0] == '"' and string[-1] == '"':
    return escape_sequence.sub(r'\1', string)[1:-1]
  else:
    raise ValueError("'unquote' called on non-quoted string.")

def walk_files(dir, include=None, exclude=None):
  """
  Walks the directory tree rooted at dir and generates the filenames of each
  file which passes the include function and fails the exclude function
  (default is include-all exclude-none).
  """
  for (root, dirs, files) in os.walk(dir):
    for f in files:
      filename = os.path.join(root, f)

      safe = include == None or include(filename)

      if exclude != None and exclude(filename):
        safe = False

      if safe:
        yield filename

def load_logic(dir):
  """
  Loads every .lp file in the given directory (and all subdirectories) together
  into a big string and parses it into a set of rules.
  """
  raw = ""
  rules = set()
  for f in walk_files(dir, include=lambda f: f.endswith(".lp")):
    with open(f, 'r') as fin:
      raw += fin.read()
      raw += "\n"
  return rules

_test_cases = [
  (quote, r"test", r'"test"'),
  (quote, r'comp"lex', r'"comp\"lex"'),
  (quote, r'ha\rd"er', r'"ha\\rd\"er"'),
  (quote, r'tou\"gh', r'"tou\\\"gh"'),
  (unquote, r'"test"', r"test"),
  (unquote, r'"comp\"lex"', r'comp"lex'),
  (unquote, r'"ha\\rd\"er"', r'ha\rd"er'),
  (unquote, r'"tou\\\"gh"', r'tou\"gh'),
  (unquote, r'"mis\\\\"match"ed"', r'mis\\"match"ed'),
]
