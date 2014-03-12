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

def content_pr(item, *content):
  """
  Returns a Predicate specifying the content of the given item.
  """
  return Pr(
    "content",
    item,
    *[ ensure_predicate(p) for p in content ]
  )

def link_prs(story, frm, to, ltype):
  """
  Takes id predicate constructions for source and destination story objects and
  returns a list of Predicates describing a link of the given type between
  them. The first item in the result list is always the id predicate for the
  new link object.
  """
  link = next_id_prs(story, "lnk", 1)[0]
  return [
    link,
    content_pr(lnk, frm, to, ensure_predicate(ltype))
  ]

def linked_node_prs(story, item, ltype, ntype, *contents):
  """
  Returns a list of Predicates denoting a linked node with the given link type,
  node type, and node contents.
  """
  node = next_id_prs(story, ntype, 1)[0]
  result = [ node ]
  result.append( content_pr(node, *contents) )
  result.extend( link_prs(story, item, node, ltype) )
  return result

def intrinsic_prs(story, item, property, value):
  """
  Returns a list of Predicates denoting an intrinsic property of the given item
  (which should be a Predicate with the same structure as the return value from
  a call to id_pr). The returned list consists of a new property plus content
  for that property and a link of type "intrinsic" from the given item to that
  property. The new property id Predicate will always be the first element of
  the result list.
  """
  return linked_node_prs(
    story,
    item,
    "intrinsic",
    "prop",
    property,
    value
  )

#############################
# Story mutation functions: #
#############################

def add_link(story, frm, to, ltype):
  """
  Adds a link of the given type from the given source (should be an id_pr
  result) to the given destination (same). Returns the id predicate for the
  newly-created link.
  """
  predicates = link_prs(story, frm, to, ltype)
  story.update(predicates)
  return predicates[0]

def add_character(story, name="Merquivest Monogarymbalid", role="stranger"):
  """
  Adds a new character to the story with the given name and role. Returns the
  id Predicate for the added character.
  """
  character = next_id_prs(story, "chr", 1)[0]
  story.add(character)
  story.update( intrinsic_prs(story, character, "name", name) )
  story.update( intrinsic_prs(story, character, "role", role) )
  return character
