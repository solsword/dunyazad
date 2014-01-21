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
