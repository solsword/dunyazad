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

class Obj:
  """
  A generic object which returns Empty instead of throwing an exception if you
  try to access nonexistent fields. This object pretends not to have any of the
  normal standard builtin fields, so only assigned fields should show up as
  non-empty.
  """
  @singleton
  class EmptyObj:
    """
    An empty object which evaluates to False and which returns itself on any
    attribute access. Because this is used as a fallback for missing attributes
    of Obj objects, you can write code like:
      if a.b.c.d:
        # do something
      else:
        # do something else
    instead of:
      try:
        v = a.b.c.d
        # do something
      except:
        # do something else
    Of course this doesn't work if you want to store False or None in your
    object tree and distinguish between that case and a missing-object case,
    but in that case you can test equivalence against None, False, or
    Obj.Empty (which should be the only instance of an EmptyObj anywhere).

    Additionally, by exploiting symbiosis with the Obj class via _base,
    EmptyObj can be used to create entire object trees. For example:
      a = Obj()
      a.b.c.d = 3
    Rather than returning an error, this code will create the objects a.b and
    a.b.c and assign a.b.c.d = 3.
    """
    def __bool__(self):
      return False

    def __str__(self):
      return "Obj.Empty"

    def _base(self, obj, name):
      """
      Sets the current base object which is using the EmptyObj to return an
      empty result. This makes it possible for __setattr__ to hallucinate
      entire object trees (see description for EmptyObj).
      """
      #print("base ({}, {}, {})".format(self, obj, name))
      o = Obj()
      object.__setattr__(self, "base_obj", obj)
      object.__setattr__(self, "hallucinated_name", name)
      object.__setattr__(self, "hallucinated_root", o)
      object.__setattr__(self, "hallucinated", o)

    def __getattribute__(self, name):
      #print("ga ({}, {})".format(self, name))
      newobj = Obj()
      setattr(object.__getattribute__(self, "hallucinated"), name, newobj)
      object.__setattr__(self, "hallucinated", newobj)
      return self

    def __setattr__(self, attr, val):
      #print("sa ({}, {}, {})".format(self, attr, val))
      setattr(object.__getattribute__(self, "hallucinated"), attr, val)
      #print("sa-out ({}, {}, {})".format(
      #  object.__getattribute__(self, "base_obj"),
      #  object.__getattribute__(self, "hallucinated_name"),
      #  object.__getattribute__(self, "hallucinated_root")
      #))
      setattr(
        object.__getattribute__(self, "base_obj"),
        object.__getattribute__(self, "hallucinated_name"),
        object.__getattribute__(self, "hallucinated_root")
      )
      #print("sa-check {}".format(
      #  #object.__getattribute__(
      #  getattr(
      #    object.__getattribute__(self, "base_obj"), 
      #    object.__getattribute__(self, "hallucinated_name")
      #  )
      #))

  Empty = EmptyObj()

  def __init__(self, **kwargs):
    object.__setattr__(self, "_dict", kwargs)

  def __getattribute__(self, name):
    #print("oga ({}, {})".format(self, name))
    _dict = object.__getattribute__(self, "_dict")
    if name in _dict:
      return _dict[name]
    else:
      object.__getattribute__(Obj.Empty, "_base")(self, name)
      return Obj.Empty

  def __setattr__(self, name, val):
    _dict = object.__getattribute__(self, "_dict")
    _dict[name] = val

def _test_obj_basic():
  a = Obj()
  assert (not a.b.c.d), "No soft failure (a.b.c.d wasn't False)."
  a.foo.bar.baz = 3
  assert (a.foo.bar.baz == 3), \
    "Hallucinogenic insertion failed (a.foo.bar.baz wasn't 3)."
  try:
    a.foo.bar.baz.q = 1
    raise AssertionError("Overwrote defined property (a.foo.bar.baz)!")
  except AttributeError:
    pass
  a.foo.q = "test"
  assert (a.foo.q == "test"), \
    "Shallow hallucination failed (a.foo.q wasn't 'test')."
  return True

_test_cases = [
  (_test_obj_basic, True)
]
