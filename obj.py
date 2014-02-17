"""
obj.py
A hacked-up "Obj" class which represents an entire tree of attributes,
returning an object Obj.Empty (which evaluates to False using bool()) when a
nonexistent attribute is accessed, even for nested attributes:

  a = Obj()
  if not a.b.c.foo:
    print("a.b.c.foo not found.") # this executes; no exception is raised

Arbitrarily nested attributes can also be automagically created:

  a = Obj()
  a.b.c.foo = "bar"
  print(a.b.c.foo) # prints "bar"
"""

from utils import *

import copy

class EmptyObj:
  """
  An empty object which evaluates to False and which returns another EmptyObj
  on any attribute access. Because this is used as a fallback for missing
  attributes of Obj objects, you can write code like:
    if a.b.c.d:
      # do something
    else:
      # do something else
  instead of:
    try:
      v = a.b.c.d
      # do something
    except AttributeError:
      # do something else
  Of course this doesn't work if you want to store False or None in your
  object tree and distinguish between that case and a missing-object case,
  but you can always test equivalence against None or False, and you can test
  whether type(a.b.c.d) == EmptyObj.

  Additionally, EmptyObj can be used to create entire object trees when
  returned via accessing a missing attribute of an Obj. For example:
    a = Obj()
    a.b.c.d = 3
  Rather than returning an error, this code will create the objects a.b and
  a.b.c and assign a.b.c.d = 3. Even something like the following should work:
    a = Obj()
    tmp = a.b.c              # an EmptyObj
    a.b.c.f = "I'm a.b.c.f"  # creates a.b, a.b.c, and assigns a.b.c.f
    tmp.d = "I'm a.b.c.d"    # assigns a.b.c.d
  But note that something like the following will result in an AttributeError:
    a = Obj()
    tmp = a.b.c
    a.b.c = 4
    tmp.x = 5   # attempt to assign 4.x = 5 fails
  In general, assigning EmptyObj's to variables is not a good idea, because
  they are just placeholders. For example, the following prints "<empty>"
  instead of printing "full":
    a = Obj()
    tmp = a.b.c
    a.b.c.d = "full"
    print(tmp.d)  # prints "<empty>"
  """
  def __init__(self, parent=None, name=None):
    object.__setattr__(self, "_parent", parent)
    object.__setattr__(self, "_name", name)

  def __bool__(self):
    return False

  def __str__(self):
    return "<empty>"

  def __getattribute__(self, name):
    #print("ega: '{}'".format(name))
    if name.startswith('__') and name.endswith('__'):
      return object.__getattribute__(self, name)
    return EmptyObj(self, name)

  def __setattr__(self, attr, val):
    asr = object.__getattribute__(self, "_attr_set_request")
    asr([attr], val)

  def __getitem__(self, addr):
    raise KeyError("EmptyObj objects have no contents.")

  def __contains__(self, addr):
    return False

  def __setitem__(self, addr, value):
    asr = object.__getattribute__(self, "_attr_set_request")
    asr(addr.split('.'), value)

  def _attr_set_request(self, attr_chain, val):
    parent = object.__getattribute__(self, "_parent")
    name = object.__getattribute__(self, "_name")
    attr_chain.insert(0, name)
    if type(parent) == EmptyObj:
      pasr = object.__getattribute__(parent, "_attr_set_request")
      pasr(attr_chain, val)
    else:
      assign_to = parent
      while len(attr_chain) > 1:
        attr = attr_chain[0]
        attr_chain = attr_chain[1:]
        o = getattr(assign_to, attr)
        if type(o) == EmptyObj:
          o = Obj()
          setattr(assign_to, attr, o)
        assign_to = o
      setattr(assign_to, attr_chain[0], val)

class Obj:
  """
  A generic object which returns Empty instead of throwing an exception if you
  try to access nonexistent fields. For convenience, field names that start and
  end with '__' will revert to normal attribute lookup, so that Python internal
  code can access methods like __deepcopy__ without issues.
  """
  def __init__(self, **kwargs):
    object.__setattr__(self, "_dict", kwargs)

  def __getattribute__(self, name):
    #print("ga: '{}'".format(name))
    if name.startswith('__') and name.endswith('__'):
      return object.__getattribute__(self, name)
    _dict = object.__getattribute__(self, "_dict")
    if name in _dict:
      return _dict[name]
    else:
      return EmptyObj(self, name)

  def __setattr__(self, name, val):
    _dict = object.__getattribute__(self, "_dict")
    _dict[name] = val

  def __copy__(obj):
    raise NotImplementedError("Shallow copying of Obj objects is forbidden.")

  def __deepcopy__(obj, memo={}):
    result = Obj()
    memo[id(obj)] = result
    _dict = object.__getattribute__(obj, "_dict")
    for key in _dict.keys():
      val = _dict[key]
      if id(val) in memo:
        setattr(result, key, memo[id(val)])
      else:
        setattr(result, key, copy.deepcopy(val, memo=memo))
    return result

  def __getitem__(self, addr):
    """
    Takes a string containing a dotted address and returns the value at that
    address from the given object. Mechanically equivalent to:

      eval("<varname of obj>" + addr)

    but obviously safer. Note that unlike attribute access, __getitem__ will
    return a KeyError if the given address doesn't exist.
    """
    if type(addr) != str:
      raise KeyError(
        "Key '{}' not found (objects only contain strings).".format(addr)
      )
    if '.' in addr:
      i = addr.index('.')
      key = addr[:i]
      tail = addr[i+1:]
      try:
        return getattr(self, key)[tail]
      except TypeError:
        return KeyError(
          "Address '{}' contained non-mapping object.".format(addr)
        )
    else:
      result = getattr(self, addr)
      if type(result) == EmptyObj:
        raise KeyError("Address '{}' is empty.".format(addr))
      return result

  def __contains__(self, addr):
    """
    Works like __getitem__ but just returns True/False.
    """
    try:
      gi = object.__getattribute__(self, "__getitem__")
      gi(addr)
      return True
    except KeyError:
      return False

  def __setitem__(self, addr, value):
    """
    Takes a string containing a dotted address and writes the given value to
    that address. Creates structure as necessary to place the value at the
    given address, but raises an AttributeError if doing so would overwrite
    existing data (for example assigning "a.b.c" to "foo" when "a.b" is 3).
    """
    if '.' in addr:
      i = addr.index('.')
      key = addr[:i]
      tail = addr[i+1:]
      getattr(self, key)[tail] = value
    else:
      setattr(self, addr, value)

def obj_contents(obj, prefix=""):
  """
  Iterates recursively over all of this object's keys in depth-first order,
  yielding (key, value) tuples where the key is a dotted string specifying
  the value's path. For example, the following code:

    a = Obj()
    a.n = 5
    a.b.c = "foo"
    a.b.x = 3
    
    for (key, value) in obj_contents(a):
      print(key + ":", value)

  produces this output:

    b.c: foo
    b.x: 3
    n: 5

  Note that the traversal is ordered according to a string sort() on the keys
  at each level.
  """
  _dict = object.__getattribute__(obj, "_dict")
  for key in sorted(_dict.keys()):
    full_key = key

    if prefix:
      full_key = prefix + '.' + full_key

    if type(_dict[key]) == Obj:
      yield from obj_contents(_dict[key], prefix=full_key)
    else:
      yield (full_key, _dict[key])

def _test_obj_basic():
  a = Obj()
  if type(a.xyzzx) != EmptyObj:
    return "Basic EmptyObj construction failed."
  if a.b.c.d:
    return "No soft failure (bool(a.b.c.d) wasn't False)."
  return True

def _test_obj_gen():
  a = Obj()
  a.foo.bar.baz = 3
  if a.foo.bar.baz != 3:
    return \
      "Generative insertion failed (a.foo.bar.baz was {} instead of 3).".format(
        a.foo.bar.baz
      )
  try:
    a.foo.bar.baz.q = 1
    return "Overwrote defined property (a.foo.bar.baz)!"
  except AttributeError:
    pass
  a.foo.q = "test"
  if a.foo.q != "test":
    return "Simple branched generation failed (a.foo.q wasn't 'test')."
  return True

def _test_obj_tmp_empty():
  a = Obj()
  tmp = a.b.c              # an EmptyObj
  a.b.c.f = "a.b.c.f"  # creates a.b, a.b.c, and assigns a.b.c.f
  tmp.d = "a.b.c.d"    # assigns a.b.c.d
  return (a.b.c.f, a.b.c.d)

def _test_obj_tmp_shadow_attribute_error():
  a = Obj()
  tmp = a.b.c
  a.b.c = 4
  try:
    tmp.x = 5   # attempt to assign 4.x = 5 fails
    return False
  except AttributeError:
    return True

def _test_obj_no_shadow_values():
  a = Obj()
  tmp = a.b.c
  a.b.c.d = "full"
  return (str(tmp.d), str(a.b.c.d))

def _test_obj_get():
  a = Obj()
  a.b.c.d = "success"
  return ("b.c.d" in a, a["b.c.d"])

def _test_obj_set():
  a = Obj()
  a["b.c.d"] = "success"
  return ("c.d" in a.b, a.b.c.d)

def _test_obj_get_missing():
  a = Obj()
  a.b.c.d = "success"
  return (
    "b.c.d" in a,
    "d" in a.b.c,
    "x" not in a,
    "b.c.x" not in a,
    "b.x.y" not in a,
    "x.y.z.q" not in a.b.c,
    "x.y.z.q" not in a.b.c.d,
  )

def _test_obj_contents():
  a = Obj()
  a.b.c = 3
  a.b.x = "foo"
  a.z = "bar"
  a.a = "baz"
  return list(obj_contents(a))

def _test_obj_copy():
  a = Obj()
  a.b.c = 3
  a.b.x = "foo"
  a.z = "bar"
  a.a = "baz"
  b = copy.deepcopy(a)
  return list(obj_contents(a)) == list(obj_contents(b))

_test_cases = [
  (_test_obj_basic, True),
  (_test_obj_gen, True),
  (_test_obj_tmp_empty, ("a.b.c.f", "a.b.c.d")),
  (_test_obj_tmp_shadow_attribute_error, True),
  (_test_obj_no_shadow_values, ("<empty>", "full")),
  (_test_obj_get, (True, "success")),
  (_test_obj_set, (True, "success")),
  (_test_obj_get_missing, (True,)*7),
  (
    _test_obj_contents,
    [
      ("a", "baz"),
      ("b.c", 3),
      ("b.x", "foo"),
      ("z", "bar")
    ]
  ),
  ( _test_obj_copy, True),
]
