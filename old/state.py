"""
state.py
Defines story state and knows how to represent it as an answer set.
"""

import ans
import english

from utils import *

class StoryState:
  def __init__(
    self,
    instances=None,
    states=None,
    properties=None,
    relations=None
  ):
    self.instances = instances or []
    self.states = states or []
    self.properties = properties or []
    self.relations = relations or []

  def code(self):
    result = ""
    for typ, tag in self.instances:
      result += "inst({}, {}).\n".format(typ, tag)
    for st, (typ, tag) in self.states:
      result += "state({}, inst({}, {})).\n".format(st, typ, tag)
    for pr, (typ, tag), val in self.properties:
      result += "property({}, inst({}, {}), {}).\n".format(pr, typ, tag, val)
    for rel, (styp, stag), (otyp, otag) in self.relations:
      result += "relation({}, inst({}, {}), inst({}, {})).\n".format(
        rel,
        styp, stag,
        otyp, otag
      )
    return result

  def add_instance(self, (typ, tag)):
    self.instances.append((typ, tag))

  def remove_instance(self, (typ, tag)):
    self.instances.remove((typ, tag))

  def add_state(self, state, (typ, tag)):
    self.states.append((state, (typ, tag)))

  def remove_state(self, state, (typ, tag)):
    self.states.remove((state, (typ, tag)))

  def add_property(self, prop, (typ, tag), val):
    self.states.append((prop, (typ, tag), val))

  def remove_property(self, prop, (typ, tag), val):
    self.states.remove((prop, (typ, tag), val))

  def add_relation(self, relation, (styp, stag), (otyp, otag)):
    self.relations.append((relation, (styp, stag), (otyp, otag)))

  def remove_relation(self, relation, (styp, stag), (otyp, otag)):
    self.relations.remove((relation, (styp, stag), (otyp, otag)))

  def states_of(self, inst):
    return [
      (st, ins)
        for (st, ins) in self.states
        if ins == inst
    ]

  def properties_of(self, inst):
    return [
      (prop, ins, val)
        for (prop, ins, val) in self.states
        if ins == inst
    ]

  def relations_from(self, inst):
    return [
      (rel, subj, obj)
        for (rel, subj, obj) in self.relations
        if subj == inst
    ]

  def relations_to(self, typ, tag):
    return [
      (rel, subj, obj)
        for (rel, subj, obj) in self.relations
        if obj == inst
    ]
