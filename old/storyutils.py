"""
storyutils.py
Convenience functions for dealing with stories encoded as a bunch of
predicates (mostly specific to the Dunyazad encoding format).
"""

import ans

Pr = ans.Pr
Vr = ans.Vr
PVr = ans.PVr
SbT = ans.SbT

# A pattern for matching id(Type, ID) predicates:
ID_PATTERN = Pr("id", Vr("Type"), Vr("ID"))

##############################
# General utility functions: #
##############################

def ensure_predicate(thing):
  """
  Makes sure the given thing is a Predicate and makes a Predicate out of it if
  it isn't one yet.
  """
  if isinstance(thing, Pr):
    return thing
  return Pr(thing)

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

#####################################
# Predicate construction functions: #
#####################################

def id_pr(type, id):
  """
  Returns a Predicate specifying the item of the given type with the given id.
  """
  return Pr("id", Pr(type), Pr(id))

def next_id_prs(story, type, n=1):
  """
  Returns N Predicates specifying new item(s) of the given type, based on the
  IDs present in the given story. Shouldn't be called more than once without
  adding the result(s) of each call to the story before subsequent calls.
  """
  nxt = next_id(story, type)
  return [ id_pr(type, i) for i in range(nxt, nxt+n) ]

def link_pr(story, frm, to, ltype):
  """
  Takes id predicate constructions for source and destination story objects and
  returns a Predicates describing a link of the given type between them.
  """
  return Pr(
    "link",
    ensure_predicate(ltype),
    ensure_predicate(frm),
    ensure_predicate(to)
  )

def intrinsic_pr(story, item, property, value):
  """
  Returns a list of Predicates denoting an intrinsic property of the given item
  (which should be a Predicate with the same structure as the return value from
  a call to id_pr). The returned list consists of a new property plus content
  for that property and a link of type "intrinsic" from the given item to that
  property. The new property id Predicate will always be the first element of
  the result list.
  """
  return Pr(
    "intrinsic",
    ensure_predicate(item),
    ensure_predicate(property),
    ensure_predicate(value),
  )

#############################
# Story mutation functions: #
#############################

def add_link(story, frm, to, ltype):
  """
  Adds a link of the given type from the given source to the given destination.
  """
  story.add(link_pr(story, frm, to, ltype))

def add_character(story, name='"Merquivest Monogarymbalid"', role="stranger"):
  """
  Adds a new character to the story with the given name and role. Returns the
  id Predicate for the added character.
  """
  character = next_id_prs(story, "chr", 1)[0]
  story.add(character)
  story.add( intrinsic_pr(story, character, "name", name) )
  story.add( intrinsic_pr(story, character, "role", role) )
  return character

def add_item(story, name='"polka-dot umbrella"', typ="key"):
  """
  Adds a new item to the story with the given name and type. Returns the id
  Predicate for the added item.
  """
  item = next_id_prs(story, "itm", 1)[0]
  story.add(item)
  story.add( intrinsic_pr(story, item, "name", name) )
  story.add( intrinsic_pr(story, item, "type", typ) )
  return item

def add_location(story, name='"center of the sun"', typ="geographic_region"):
  """
  Adds a new location to the story with the given name and type. Returns the id
  Predicate for the added location.
  """
  location = next_id_prs(story, "loc", 1)[0]
  story.add(location)
  story.add( intrinsic_pr(story, location, "name", name) )
  story.add( intrinsic_pr(story, location, "type", typ) )
  return location
