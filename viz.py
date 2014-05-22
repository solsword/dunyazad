#!/usr/bin/env python3
"""
viz.py
For visualizing the global rules.
"""

import re

from graphviz import Digraph

from ans import *

from utils import *

# Defines where to search for global rules:
GLOBAL_RULES_DIR = "rules"

def objects(thing):
  result = set()
  if (
    isinstance(thing, Disjunction)
   or isinstance(thing, Choice)
   or isinstance(thing, Aggregate)
  ):
    for e in thing.elements:
      result |= objects(e)
  elif isinstance(thing, ChoiceElement):
    result |= objects(thing.literal)
  elif isinstance(thing, AggregateElement):
    result |= objects(thing.terms)
  elif isinstance(thing, list):
    for t in thing:
      result |= objects(t)
  elif isinstance(thing, Interval) or isinstance(thing, BuiltinAtom):
    pass # we don't bother tracking these
  elif isinstance(thing, Expression):
    result |= objects(thing.lhs)
    result |= objects(thing.rhs)
  elif isinstance(thing, NafLiteral):
    result |= objects(thing.contents)
  elif isinstance(thing, ClassicalLiteral) or isinstance(thing, SimpleTerm):
    result.add("{}{}/{}".format(
      '-' if (hasattr(thing, "negated") and thing.negated) else '',
      slug(thing.id),
      len(thing.terms if hasattr(thing, "terms") else 0)
    ))
  elif isinstance(thing, ScriptCall):
    result.add("@{}/{}".format(slug(thing.function), len(thing.args)))
  return result

if __name__ == "__main__":
  print("Loading rules...")
  ruleset = load_logic(GLOBAL_RULES_DIR)
  print("  ...done.")

  nodes = set(["#FAIL", "#AVOID", "#OPT"])
  edges = set()

  print("Creating dependency graph...")
  # create a dependency graph:
  for r in ruleset:
    # Note: Directives and Scripts are ignored.
    if isinstance(r, Rule):
      atoms = set()
      dependencies = objects(r.body)
      if hasattr(r, "head") and r.head != None:
        atoms |= objects(r.head)
      else:
        atoms.add("#FAIL") # headless rules will have #FAIL as their head
      nodes |= atoms
      nodes |= dependencies
      for a in atoms:
        for d in dependencies:
          edges.add((a, d))
    elif isinstance(r, WeakConstraint):
      for d in objects(r.body):
        nodes.add(d)
        edges.add(("#AVOID", d))
    elif isinstance(r, Optimization):
      for e in r.elements:
        for l in e.literals:
          atoms = objects(l)
          nodes |= atoms
          for a in atoms:
            edges.add(("#OPT", a))

  node_ids = {}
  i = 0
  for n in nodes:
    node_ids[n] = 'a' + str(i)
    print("Got node: {} :: {}".format(repr(n), repr(node_ids[n])))
    i += 1

  # now build it in graphviz format:
  graph = Digraph(comment="Auto-generated logic visualization.")

  for n in nodes:
    graph.node(node_ids[n], n)

  for frm, to in edges:
    if frm in ["#FAIL", "#AVOID", "#OPT"]:
      graph.edge(node_ids[frm], node_ids[to], constraint=False)
    else:
      graph.edge(node_ids[frm], node_ids[to])

  graph.render("viz-test")
