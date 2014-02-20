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

# A class whose repr() is the empty string:

@singleton
class NoRepr:
  """
  An object that returns '' for both str() and repr().
  """
  def __str__(self): return ""
  def __repr__(self): return ""

norepr = NoRepr()


## Iterator peeking:
#
#def unpeekable(iter):
#  """
#  Returns an unpeekable version of the given iterator which can be unpeeked
#  by sending it the string 'unpeek' (see the unpeek function below).
#  """
#  def iterate():
#    current = next(iter)
#    while True:
#      r = yield current
#      if r == "unpeek":
#        yield current # goes to the send() call
#      else:
#        break
#  return iterate
#
#def unpeek(unpeekable):
#  """
#  Unpeeks the given unpeekable generator.
#  """
#  unpeekable.send("unpeek")

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
