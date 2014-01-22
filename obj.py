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
    #print("ga ({}, {})".format(self, name))
    return EmptyObj(self, name)

  def __setattr__(self, attr, val):
    #print("sa ({}, {}, {})".format(self, attr, val))
    asr = object.__getattribute__(self, "_attr_set_request")
    asr([attr], val)

  def _attr_set_request(self, attr_chain, val):
    #print("asr({}, {}, {})".format(self, attr_chain, val))
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
  try to access nonexistent fields. This object pretends not to have any of the
  normal standard builtin fields, so only assigned fields should show up as
  non-empty.
  """
  def __init__(self, **kwargs):
    object.__setattr__(self, "_dict", kwargs)

  def __getattribute__(self, name):
    #print("oga ({}, {})".format(self, name))
    _dict = object.__getattribute__(self, "_dict")
    if name in _dict:
      return _dict[name]
    else:
      return EmptyObj(self, name)

  def __setattr__(self, name, val):
    _dict = object.__getattribute__(self, "_dict")
    _dict[name] = val

# Note that with the way Obj objects are set up, they can't usefully provide
# methods. The following obj_* functions are thus pseudo-methods of Obj.

def obj_readaddr(obj, addr):
  """
  Takes a string containing a dotted address and returns the value at that
  address from the given object. Mechanically equivalent to:

    eval("<varname of obj>" + addr)

  but obviously safer.
  """
  if '.' in addr:
    i = addr.index('.')
    key = addr[:i]
    tail = addr[i+1:]
    return obj_readaddr(getattr(obj, key), tail)
  else:
    return getattr(obj, addr)

def obj_writeaddr(obj, addr, value):
  """
  Takes a string containing a dotted address and writes the given value to that
  address. Creates structure as necessary to place the value at the given
  address, but raises an AttributeError if doing so would overwrite existing
  data (for example assigning "a.b.c" to "foo" when "a.b" is 3).
  """
  if '.' in addr:
    i = addr.index('.')
    key = addr[:i]
    tail = addr[i+1:]
    obj_writeaddr(getattr(obj, key), tail, value)
  else:
    setattr(obj, addr, value)

def obj_contents(obj, prefix=""):
  """
  Iterates recursively over all of this object's keys in depth-first order,
  yielding (key, value) tuples where the key is a dotted string specifying
  the value's path. For example, the following code:

    a = Obj()
    a.n = 5
    a.b.c = "foo"
    a.b.x = 3
    
    for (key, value) in a.contents():
      print(key + ":", value)

  produces this output:

    a.b.c: foo
    a.b.x: 3
    a.n: 5

  Note that the traversal is ordered according to a string sort() on the keys
  at each level.
  """
  _dict = object.__getattribute__(obj, "_dict")
  for key in sorted(_dict.keys()):
    full_key = key

    if prefix:
      full_key = prefix + '.' + full_key

    if type(key) == Obj:
      yield from _dict[key].contents(prefix=full_key)
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

_test_cases = [
  (_test_obj_basic, True),
  (_test_obj_gen, True),
  (_test_obj_tmp_empty, ("a.b.c.f", "a.b.c.d")),
  (_test_obj_tmp_shadow_attribute_error, True),
  (_test_obj_no_shadow_values, ("<empty>", "full")),
]
