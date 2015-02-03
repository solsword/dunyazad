#!/usr/bin/env python3
"""
aspviz.py
An ASP program visualizer. It loads rules from the DEFAULT_RULES_DIR directory
by default, or takes a list of files to load rules from.
"""

import re
import sys

from graphviz import Digraph

from ans import *

from utils import *

# Defines where to search for rules if no argument is given:
DEFAULT_RULES_DIR = "rules"

OUTPUT_DIR = "viz-output"

def objects(thing):
  result = set()
  if (
    thing == None
   or isinstance(thing, Interval)
   or isinstance(thing, BuiltinAtom)
  ):
    pass # we don't bother tracking these
  elif (
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
  elif isinstance(thing, list) or isinstance(thing, tuple):
    for t in thing:
      result |= objects(t)
  elif isinstance(thing, Expression):
    result |= objects(thing.lhs)
    result |= objects(thing.rhs)
  elif isinstance(thing, NafLiteral):
    result |= objects(thing.contents)
  elif isinstance(thing, ClassicalLiteral) or isinstance(thing, SimpleTerm):
    result.add("{}{}/{}".format(
      '-' if (hasattr(thing, "negated") and thing.negated) else '',
      slug(thing.id),
      len(thing.terms) if hasattr(thing, "terms") and thing.terms != None else 0
    ))
  elif isinstance(thing, ScriptCall):
    result.add(
      "@{}/{}".format(
        slug(thing.function),
        len(thing.args) if hasattr(thing, "args") and thing.args != None else 0
      )
    )
  else:
    raise NotImplementedError(
      "No method for getting objects out of thing: {}".format(thing)
    )
  return result

def misspellings(word):
  word_letters = "abcdefghijklmnopqrstuvwxyz_"
  result = set()
  # omissions
  for i in range(len(word)):
    result.add(word[:i] + word[i+1:])
  # inversions
  for i in range(len(word) - 1):
    result.add(word[:i] + word[i+1] + word[i] + word[i+2:])
  # substitutions
  for i in range(len(word)):
    for l in word_letters:
      result.add(word[:i] + l + word[i+1:])
  # additions
  for i in range(len(word)+1):
    for l in word_letters:
      result.add(word[:i] + l + word[i:])
  return result

if __name__ == "__main__":
  print("Loading rules...")
  if sys.argv[1:]:
    ruleset = load_logic_files(sys.argv[1:])
  else:
    ruleset = load_logic_dir(DEFAULT_RULES_DIR)
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
  print("  ...done.")

  print("Constructing GraphViz graph...")
  node_ids = {}
  i = 0
  for n in nodes:
    node_ids[n] = 'node_{}'.format(i)
    i += 1

  print("Checking for potential misspellings...")
  msp = {}
  nl = list(nodes)
  for i, n in enumerate(nl):
    msp = misspellings(n)
    for o in nl[i:]:
      if n == o:
        continue
      if o in msp:
        print("Possible misspelling: '{}' <-> '{}'".format(n, o))

  # now build it in graphviz format:
  graph = Digraph(comment="Auto-generated logic visualization.")

  for n in nodes:
    graph.node(node_ids[n], n)

  for frm, to in edges:
    if frm in ["#FAIL", "#AVOID", "#OPT"]:
      graph.edge(node_ids[frm], node_ids[to], constraint="false")
    else:
      graph.edge(node_ids[frm], node_ids[to])
  print("  ...done.")

  outfile = os.path.join(OUTPUT_DIR, "rules.gv")
  if os.path.exists(OUTPUT_DIR):
    if not os.path.isdir(OUTPUT_DIR):
      outfile = "viz-output-rules.gv"
  else:
    os.mkdir(OUTPUT_DIR)
  print("Rendering GraphViz graph to '{}.pdf'...".format(outfile))
  graph.render(outfile)
  print("  ...done.")
