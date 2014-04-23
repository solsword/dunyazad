'''
utils.py
Utility functions.
'''

import re
import os
import sys
import itertools
import traceback

# Sorting by str(x):
def strsorted(x):
  return sorted(x, key=lambda x: str(x))

# Formatting an error as Python would:
def format_exception(e):
  return ''.join(
    traceback.format_exception(type(e), e, e.__traceback__)
  )

# Appending to a tuple:

def tuple_with(src, add):
  return tuple(list(src) + [add])

# Quoting and unquoting:

def is_quoted(string):
  return string[0] == '"' and string[-1] == '"'

def quote(string):
  return '"' + string.replace("\\", "\\\\").replace('"', r'\"') + '"'

escape_sequence = re.compile(r'\\(["\\])')
def unquote(string):
  if is_quoted(string):
    return escape_sequence.sub(r'\1', string)[1:-1]
  else:
    raise ValueError("'unquote' called on non-quoted string.")

def dequote(string):
  if is_quoted(string):
    return unquote(string)
  else:
    return string

# Walking object attributes:
def walk_attributes(o, seen=set()):
  """
  A generator that walks all of the non-internal attributes of the given
  object recursively in a depth-first order. An attribute is considered
  internal (and thus skipped along with its children) if it begins and ends
  with '__'. For each attribute touched this function yields a tuple of the
  attribute name and the attribute value. Recursive objects will be yielded as
  the value for whatever attribute they are but their children will be ignored.
  """
  if id(o) in seen:
    return
  seen.add(id(o))
  if hasattr(o, "__dict__"):
    for attr in o.__dict__:
      if not (attr.startswith("__") and attr.endswith("__")):
        yield attr, getattr(o, attr)
        for sub in walk_attributes(getattr(o, attr), seen):
          yield sub

# Class decorators:

def strrepr(cls):
  """
  Copies the __str__ function as the __repr__ function.
  """
  if "__repr__" in cls.__dict__:
    raise ValueError(
      "Tried to overwrite existing __repr__ of class '{}'".format(cls.__name__)
    )
  cls.__repr__ = cls.__str__
  return cls

def instance(cls):
  """
  Replaces the decorated class with an instance of itself, constructed with
  zero arguments. Attributes from the class (except those that start and begin
  with "__") are copied onto the instance, so they behave as if they were
  instance attributes rather than class attributes.
  """
  result = cls()
  for attr in cls.__dict__:
    if not (attr.startswith("__") and attr.endswith("__")):
      setattr(result, attr, cls.__dict__[attr])
  return result

def abstract(cls):
  """
  Creates an __init__ method for the given class that raises a
  NotImplementedError indicating that the class is abstract. Classes inheriting
  from an abstract base class that don't override __init__ will also be
  effectively abstract.
  """
  def __init__(self):
    raise NotImplementedError(
      "Can't instantiate abstract class '{}'.".format(
        type(self).__name__
      )
    )
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
    rec_hash = str(hash(cls.__name__) + 17)
    hash_expr = str(hash(cls.__name__))
    for a in attributes:
      hash_expr = '47 * ({}) + hash(self.{}) '.format(hash_expr, a)
    eq_expr = ' and '.join(
      "(self.{var} == other.{var})".format(var=a) for a in attributes
    )
    code = """\
@prevent_recursion({rec_hash})
def __hash__(self):
  return {hash_expr}

def eq_smart_base_case(initial_args, recursive_args):
  initial_self = initial_args[0]
  initial_other = initial_args[1][0]
  recursive_self = recursive_args[0]
  recursive_other = recursive_args[1][0]
  # If I contain a copy of myself, other should contain a copy of themselves:
  return (
    id(initial_self) == id(recursive_self)
  and
    id(initial_other) == id(recursive_other)
  )

@prevent_recursion(smart_base_case=eq_smart_base_case)
def __eq__(self, other):
  '''
  Note: won't work for recursive objects!
  '''
  return type(self) == type(other) and {eq_expr}

def __ne__(self, other):
  return not self == other
""".format(
  rec_hash=rec_hash,
  hash_expr=hash_expr,
  eq_expr=eq_expr,
)
    exec(code, locals(), globals())
    cls.__hash__ = __hash__
    cls.__eq__ = __eq__
    cls.__ne__ = __ne__
    cls._being_hashed = False
    return cls
  return decorate

def attr_object(*args):
  """
  A "attr_object" is a class with the listed discrete instance attributes
  (each of which is optional) that is uniquely defined by those attributes. The
  following methods are automatically defined:

    __init__
    __repr__

  The following additional methods are automatically defined by
  uniquely_defined_by:

    __hash__
    __eq__
    __ne__
  """
  if not args:
    raise ValueError("Tried to create attr_object class without any contents.")
  def decorate(cls):
    nonlocal args
    init_args = ', '.join("{}=None".format(a) for a in args)
    init_body = '\n  '.join("self.{var} = {var}".format(var=a) for a in args)
    repr_str = "{}({})".format(
      cls.__name__,
      ', '.join('{}' for a in args)
    )
    repr_fmt_args = ', '.join("repr(self.{})".format(a) for a in args)
    code = """\
def __init__(self, {init_args}):
  {init_body}

def __repr__(self):
  return "{repr_str}".format({repr_fmt_args})
""".format(
  init_args=init_args,
  init_body=init_body,
  repr_str=repr_str,
  repr_fmt_args=repr_fmt_args,
)
    exec(code, locals(), globals())
    cls.__init__ = __init__
    cls.__repr__ = __repr__
    return uniquely_defined_by(*args)(cls)
  return decorate

# Miscellaneous classes:

@instance
class NoRepr:
  """
  An object that returns '' for both str() and repr().
  """
  def __str__(self): return ""
  def __repr__(self): return ""

@instance
class NotGiven:
  """
  A object for use in default arguments where None is a valid argument.
  """
  def __bool__(self):
    return False

# Function decorators:

def prevent_recursion(
  default_value=NotGiven,
  base_case=NotGiven,
  smart_base_case=NotGiven,
):
  """
  A decorator that returns the given default value instead of whatever would
  usually be returned when the decorated method (it must be a method) is called
  recursively. Definitely not thread-safe. If no default value is given, it
  returns '<cls:id>' where 'cls' is the class name of the object the method is
  being called on and 'id' is its id. The three kinds of defaults override each
  other: smart_base_case is used first, otherwise base_case is used and only if
  neither smart_base_case nor base_case is given will default_value be used.
  Each determines a default value differently:

    default_value - Used directly as the default value.

    base_case - called with the method arguments (including self) to return a
      default value.

    smart_base_case - called with two tuples to determine a default value. The
      first tuple is (self, args, kwargs) as passed to the original
      non-recursive call to the method, while the second tuple is (self, args,
      kwargs) as passed to the current recursive call (for which the computed
      default value will be used).
  """
  def decorate(method):
    def recursion_guard(self, *args, **kwargs):
      nonlocal default_value, base_case
      if not hasattr(self, "_is_being_called"):
        self._is_being_called = False
      if not hasattr(self, "_initially_called_with"):
        self._initially_called_with = None
      if self._is_being_called:
        if smart_base_case != NotGiven:
          return smart_base_case(
            self._initially_called_with,
            (self, args, kwargs)
          )
        elif base_case != NotGiven:
          return base_case(self, *args, **kwargs)
        elif default_value != NotGiven:
          return default_value
        else:
          return "<{}:{}>".format(type(self).__name__, id(self))
      else:
        self._is_being_called = True
        self._initially_called_with = (self, args, kwargs)
        result = method(self, *args, **kwargs)
        self._initially_called_with = None
        self._is_being_called = False
        return result
    return recursion_guard
  return decorate

# Filesystem functions:

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

def process_file_contents(dir, process, include=None, exclude=None):
  """
  Loads every file that matches the given filters in the given directory (and
  all subdirectories) and calls the given function on its text, returning a
  flat list of the results.
  """
  results = []
  for f in walk_files(dir, include, exclude):
    with open(f, 'r') as fin:
      try:
        results.append(process(fin.read()))
      except Exception as e:
        sys.stderr.write(
          "Error while processing file '{}':\n".format(
            f
          )
        )
        raise e
  return results

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
