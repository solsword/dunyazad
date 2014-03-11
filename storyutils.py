"""
storyutils.py
Convenience functions for dealing with stories encoded as a bunch of
predicates (some specific to the Dunyazad encoding format).
"""

import ans

Pr = ans.Pr
Vr = ans.Vr
PVr = ans.PVr
SbT = ans.SbT

# A pattern for matching id(Type, ID) predicates:
ID_PATTERN = Pr("id", Vr("Type"), Vr("ID"))

def ensure_pr(thing):
  """
  Makes sure the given thing is a Predicate and makes a Predicate out of it if
  it isn't one yet.
  """
  if isinstance(thing, Pr):
    return thing
  return Pr(thing)

def id_pr(type, id):
  """
  Returns a Predicate specifying the item of the given type with the given id.
  """
  return Pr("id", Pr(type), Pr(id))

def intrinsic_pr(item, property, value):
  """
  Returns a Predicate denoting an intrinsic property of the given item (which
  should be a Predicate with the same structure as the return value from a call
  to id_pr). Both the property and value may be either Predicate objects in
  which case they are incorporated directly in the result or some other object
  in which case they are turned into predicates by calling Predicate() on them
  before being incorporated in the result.
  """
  return Pr(
    "intrinsic",
    item,
    Pr( "prop", ensure_pr(property), ensure_pr(value) ),
  )

def link_pr(frm, to, type):
  """
  Takes id predicate constructions for source and destination story objects and
  returns a predicate denoting a link of the given type between them.
  """
  return Pr(
    "link",
    frm,
    to,
    type
  )

def next_id(story, type):
  """
  Scans through the given story and returns the next ID number for the given
  object type.
  """
  highest = -1
  for (schema, binding) in ans.bindings({"id": ID_PATTERN}, story):
    id_type = dequote(str(binding["id.Type"].name))
    id_int = int(binding["id.ID"].name)
    if id_type == type and id_int > highest:
      highest = id_int
  return highest + 1

def add_character(story, name="Merquivest Monogarymbalid", role="stranger"):
  """
  Adds a new character to the story with the given name and role.
  """
  idp = id_pr("chr", next_id(story, "chr"))
  story.add(idp)
  story.add(intrinsic_pr(idp, "name", name))
  story.add(intrinsic_pr(idp, "role", role))
