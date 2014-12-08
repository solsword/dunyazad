"""
tasks.py
"""

import os
import random

import asp
import ans

from ans import Pr, Vr, PVr, SbT

# Defines where to search for global rules:
GLOBAL_RULES_DIR = "rules"

# shortcut:
opj = os.path.join

# Base source files:
BASE_SOURCES = [
  "utils.lp",
  opj("story", "core.lp"),
  opj("story", "setup.lp"),
  opj("story", "potential.lp"),
  opj("story", "grow.lp"),
  opj("story", "eval.lp"),
  opj("story", "vignettes.lp"),
  opj("story", "choice_structure.lp"),
  opj("story", "surface.lp"),
]

CONTENT_DIR = opj("story", "content")

CONTENT_SOURCES = [
  opj(GLOBAL_RULES_DIR, CONTENT_DIR, f)
    for f in os.listdir(opj(GLOBAL_RULES_DIR, CONTENT_DIR))
    if f.endswith(".lp")
]

ALL_SOURCES = [
  opj(GLOBAL_RULES_DIR, path) for path in BASE_SOURCES
] + CONTENT_SOURCES

SEP = '\n' + '%'*80 + '\n'

BASE_SRC = SEP + "% Base source code start." + SEP
for s in ALL_SOURCES:
  with open(s) as fin:
    BASE_SRC += fin.read() + SEP
BASE_SRC += "% Base source code end." + SEP

FRAGMENTS_DIR = "fragments"

FR_CACHE = {}
def fr(name):
  if name in FR_CACHE:
    return FR_CACHE[name]
  else:
    code = ""
    fl = opj(*(name.split('.'))) + ".lp"
    with open(opj(FRAGMENTS_DIR, fl)) as fin:
      code = fin.read()
    FR_CACHE[name] = code
    return code

SC = {
  "story_node": Pr("story_node", Vr("Node")),
  "option": Pr("at", Vr("Node"), Pr("option", Vr("Opt"))),
  "successor": Pr("successor", Vr("From"), Pr("option", Vr("Opt")), Vr("To")),
  "setup": PVr("setup", "setup", Vr("Node"), Vr("Which")),
}

KEEP = {
  "at": PVr("at", "at", Vr("Node"), SbT("Fluent")),
  "st": PVr("st", "st", Vr("Node"), SbT("Fluent")),

  "story_node": PVr("story_node", "story_node", Vr("Node")),
  "node_type": PVr("node_type", "node_type", Vr("Node"), Vr("Type")),
  "node_status_reached":
    PVr("node_status_reached", "node_status_reached", Vr("Node"), Vr("Status")),
  "successor":
    PVr(
      "successor", "successor",
      Vr("From"),
      Pr("option", Vr("Opt")),
      Vr("To"),
    ),
  "path_length": PVr("path_length", "path_length", Vr("Node"), Vr("Count")),

  "vignette": PVr("vignette", "vignette", Vr("Node"), Vr("Root")),

  "setup": PVr("setup", "setup", Vr("Node"), Vr("Which")),
  "spontaneous":
    PVr("spontaneous", "spontaneous", Pr("st", Vr("Node"), SbT("State"))),
  "unresolved_potential":
    PVr(
      "unresolved_potential", "unresolved_potential",
      Vr("Node"),
      Pr("option", Vr("Opt")),
      SbT("Potential"),
    ),
  "resolves_vignette":
    PVr(
      "resolves_vignette", "resolves_vignette", 
      Vr("Node"),
      Pr("option", Vr("Opt")),
    ),

  "intro_text":
    PVr("intro_text", "intro_text", Vr("Node"), Vr("Text")),
  "potential_text":
    PVr("potential_text", "potential_text", Vr("Node"), Vr("Text")),
  "option_text":
    PVr(
      "option_text", "option_text",
      Vr("Node"),
      Pr("option", Vr("Opt")),
      Vr("Text"),
    ),
  "action_text":
    PVr(
      "action_text", "action_text",
      Vr("Node"),
      Pr("option", Vr("Opt")),
      Vr("Text"),
    ),
}

def runfr(story, name, extra = ""):
  return asp.solve(
    SEP.join(
      [
        BASE_SRC,
        '\n'.join(str(pr) + '.' for pr in story),
        fr(name),
        extra
      ]
    )
  )

def filter_keep(story):
  result = []
  for sch, bnd in ans.bindings(KEEP, story):
    result.append(bnd[sch])
  return result

def all_nodes(story):
  for pr in story:
    b = ans.bind(SC["story_node"], pr)
    if b:
      yield b["story_node.Node"]

def all_vignette_beginnings(story):
  # TODO: Fix me!
  for pr in story:
    b = ans.bind(SC["setup"], pr)
    if b:
      yield b["setup.Node"]

def all_uninitialized_nodes(story):
  ai = list(all_initialized_nodes(story))
  for n in all_nodes(story):
    if n not in ai:
      yield n

def all_options(story):
  for pr in story:
    b = ans.bind(SC["option"], pr)
    if b:
      yield (b["at.Node"], b["at.option.Opt"])

def all_successors(story):
  for pr in story:
    b = ans.bind(SC["successor"], pr)
    if b:
      yield (b["successor.From"], b["successor.option.Opt"], b["successor.To"])

def all_uninstantiated_nodes(story):
  an = all_nodes(story)
  ao = list(all_options(story))
  for n in an:
    hit = False
    for node, opt in ao:
      if n == node:
        hit = True
        break
    if not hit:
      yield n

def all_uninstantiated_initialized_nodes(story):
  uin = list(all_uninstantiated_nodes(story))
  return [n for n in all_initialized_nodes(story) if n in uin]

def all_unfinished_options(story):
  ao = all_options(story)
  an = list(all_nodes(story))
  asc = list(all_successors(story))
  for n, opt in ao:
    hit = False
    for node, option, nxt in asc:
      if n == node and opt == option:
        hit = True
        break
    if not hit:
      yield (n, opt)

# Tasks:

def setup_story(story):
  return filter_keep(runfr(story, "setup"))

def instantiate_node(story, n):
  return filter_keep(runfr(story, "instantiate", "target_node({}).".format(n)))

def branch_node(story, n):
  return filter_keep(runfr(story, "branch", "target_node({}).".format(n)))

def instantiate_random(story):
  l = list(all_uninstantiated_nodes(story))
  if l:
    return instantiate_node(story, random.choice(l))
  else:
    print("Instantiate random: no node to instantiate!")
    for pr in story:
      print(str(pr) + ".")

def branch_random(story):
  l = list(all_unfinished_options(story))
  if l:
    return branch_node(story, random.choice(l)[0])
  else:
    print("Branch random: no node to instantiate!")
    for pr in story:
      print(str(pr) + ".")
