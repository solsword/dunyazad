#!/usr/bin/env python3
"""
main.py
The main file for story generation.
"""

import os
import os.path

import sys

import asp
import ans
import parser
import tasks
import viz
import english

from utils import *

CRASHFILE = os.path.join("out", "crash.lp")

def main(storyfile = None, scaffoldfile = None, nodelimit = 12):
  story = []
  sofar = []
  scaffolding = ""
  error = False
  if scaffoldfile:
    with open(scaffoldfile, 'r') as fin:
      scaffolding = fin.read()

  if storyfile:
    with open(storyfile, 'r') as fin:
      sofar = list(ans.parse_ans(fin.read()))
  else:
    try:
      story.extend(tasks.setup_story([], scaffolding))
    except asp.ASPError as e:
      print(
        "Error during setup: {}. Dumping source to '{}'".format(
          e.message,
          CRASHFILE
        )
      )
      with open(CRASHFILE, 'w') as fout:
        fout.write('% stdout:\n%' + '\n%'.join(e.stdout.split('\n')))
        fout.write('% stderr:\n%' + '\n%'.join(e.stderr.split('\n')))
        fout.write(e.program)
      return False
 
    n = -1
    if not os.path.isdir("out"):
      os.mkdir("out")
    if not os.path.isdir(os.path.join("out", "snapshots")):
      os.mkdir(os.path.join("out", "snapshots"))
    sofar = story
    while len(list(tasks.all_nodes(story))) < nodelimit:
      n += 1
      try:
        story = tasks.instantiate_random(story, scaffolding)
        with open(
          os.path.join("out", "snapshots", "story-{}.lp".format(n)),
          'w'
        ) as fout:
          for pr in story:
            fout.write(str(pr) + '.\n')
      except asp.ASPError as e:
        print(
          "Error during instantiation: {}. Dumping source to '{}'".format(
            e.message,
            CRASHFILE
          )
        )
        with open(CRASHFILE, 'w') as fout:
          fout.write('% stdout:\n%' + '\n%'.join(e.stdout.split('\n')))
          fout.write('% stderr:\n%' + '\n%'.join(e.stderr.split('\n')))
          fout.write(e.program)
        error = True
        break
 
      try:
        story = tasks.branch_random(story, scaffolding)
      except asp.ASPError as e:
        print(
          "Error during branching: {}. Dumping source to '{}'".format(
            e.message,
            CRASHFILE
          )
        )
        with open(CRASHFILE, 'w') as fout:
          fout.write('% stdout:\n%' + '\n%'.join(e.stdout.split('\n')))
          fout.write('% stderr:\n%' + '\n%'.join(e.stderr.split('\n')))
          fout.write(e.program)
        error = True
        break
      sofar = story

#  for pr in sofar:
#    print(str(pr) + '.')
#  nouns = english.glean_nouns(sofar)
#  for k in nouns:
#    print(nouns[k])
  print("Story nodes:")
  for pr in sofar:
    if (pr.name == "story_node"):
      print("  " + pr.args[0].name)

  print("Drawing story graph...")
  viz.viz(sofar)

  print("Building story text...")
  with open(os.path.join("out", "story.txt"), 'w') as fout:
    fout.write(english.build_story_text(sofar, timeshift=None))
  with open(os.path.join("out", "facts.lp"), 'w') as fout:
    for pr in sofar:
      fout.write(str(pr) + '.\n')
  return not error

if __name__ == "__main__":
  success = False
  storyfile = None
  scaffolding = None
  nodelimit = 12

  if '-s' in sys.argv:
    idx = sys.argv.index('-s')
    storyfile = sys.argv[idx+1]

  if '-f' in sys.argv:
    idx = sys.argv.index('-f')
    scaffolding = sys.argv[idx+1]

  if '-n' in sys.argv:
    idx = sys.argv.index('-n')
    nodelimit = int(sys.argv[idx+1])

  success = main(storyfile, scaffolding, nodelimit)

  if not success:
    exit(1)
