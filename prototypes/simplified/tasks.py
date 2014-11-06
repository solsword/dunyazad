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
  opj("story", "choice_structure.lp"),
  opj("story", "core.lp"),
  opj("story", "grow.lp"),
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

def all_nodes(story):
  for pr in story:
    b = ans.bind(SC["story_node"], pr)
    if b:
      yield b["story_node.Node"]

def all_options(story):
  for pr in story:
    b = ans.bind(SC["option"], pr)
    if b:
      yield (b["at.Node"], b["at.option.Opt"])

def all_successors(story):
  for pr in story:
    b = ans.bind(SC["successor"], pr)
    if b:
      yield (b["successor.From"], b["successor.Opt"], b["successor.To"])

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
  return runfr(story, "setup")

def instantiate_node(story, n):
  return runfr(story, "instantiate", "target_node({}).".format(n))

def branch_node(story, n):
  return runfr(story, "branch", "target_node({}).".format(n))

def instantiate_random(story):
  return instantiate_node(
    story,
    random.choice(list(all_uninstantiated_nodes(story)))
  )

def branch_random(story):
  return branch_node(
    story,
    random.choice(list(all_unfinished_options(story)))[0]
  )
