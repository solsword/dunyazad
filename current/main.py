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

def write_crashfile(e):
  with open(CRASHFILE, 'w') as fout:
    fout.write('% stdout:\n%' + '\n%'.join(e.stdout.split('\n')))
    fout.write('% stderr:\n%' + '\n%'.join(e.stderr.split('\n')))
    fout.write(e.program)

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
      write_crashfile(e)
      return False
 
    n = -1
    if not os.path.isdir("out"):
      os.mkdir("out")
    if not os.path.isdir(os.path.join("out", "snapshots")):
      os.mkdir(os.path.join("out", "snapshots"))
    sofar = story
    keepgoing = True
    target = "unknown"
    while len(list(tasks.all_nodes(story))) < nodelimit and keepgoing:
      keepgoing = False
      n += 1
      target = tasks.random_uninstantiated(story)
      if not target:
        print("No nodes to instantiate this cycle.")
      else:
        keepgoing = True
        print("Instantiating node '{}'...".format(target))
        try:
          story = tasks.instantiate_node(story, target, scaffolding)
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
          write_crashfile(e)
          error = True
          break
 
      target = tasks.random_unbranched(story)
      if not target:
        print("No nodes to branch this cycle.")
      else:
        keepgoing = True
        print("Branching node '{}'...".format(target))
        try:
          story = tasks.branch_node(story, target, scaffolding)
        except asp.ASPError as e:
          print(
            "Error during branching: {}. Dumping source to '{}'".format(
              e.message,
              CRASHFILE
            )
          )
          write_crashfile(e)
          error = True
          break

      target = tasks.random_ending(story)
      if not target:
        print("No endings to polish this cycle.")
      else:
        keepgoing = True
        print("Polishing ending node '{}'...".format(target))
        try:
          story = tasks.polish_ending(story, target, scaffolding)
        except asp.ASPError as e:
          print(
            "Error while polishing ending: {}. Dumping source to '{}'".format(
              e.message,
              CRASHFILE
            )
          )
          write_crashfile(e)
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
  viz.viz(sofar, error, str(target))

  print("Writing out story facts...")
  with open(os.path.join("out", "facts.lp"), 'w') as fout:
    for pr in sofar:
      fout.write(str(pr) + '.\n')

  print("Building story text...")
  with open(os.path.join("out", "story.txt"), 'w') as fout:
    fout.write(english.build_story_text(sofar, timeshift=None))
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
