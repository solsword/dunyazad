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
import english

from utils import *

CRASHFILE = os.path.join("out", "crash.lp")

NODES_TO_GENERATE = 12

def main(storyfile = None):
  sofar = []
  if storyfile:
    with open(storyfile, 'r') as fin:
      sofar = list(ans.parse_ans(fin.read()))
  else:
    try:
      story = tasks.setup_story("")
    except asp.ASPError as e:
      print("Error during setup. Dumping source to '{}'".format(CRASHFILE))
      with open(CRASHFILE, 'w') as fout:
        fout.write(e.message)
      exit(1)
 
    n = -1
    if not os.path.isdir("out"):
      os.mkdir("out")
    if not os.path.isdir(os.path.join("out", "snapshots")):
      os.mkdir(os.path.join("out", "snapshots"))
    sofar = story
    while len(list(tasks.all_nodes(story))) < NODES_TO_GENERATE:
      n += 1
      try:
        story = tasks.instantiate_random(story)
        with open(
          os.path.join("out", "snapshots", "story-{}.lp".format(n)),
          'w'
        ) as fout:
          for pr in story:
            fout.write(str(pr) + '.\n')
      except asp.ASPError as e:
        print(
          "Error during instantiation. Dumping source to '{}'".format(CRASHFILE)
        )
        with open(CRASHFILE, 'w') as fout:
          fout.write(e.message)
        break
 
      try:
        story = tasks.branch_random(story)
      except asp.ASPError as e:
        print("Error during branching. Dumping source to '{}'".format(CRASHFILE))
        with open(CRASHFILE, 'w') as fout:
          fout.write(e.message)
        break
      sofar = story

#  for pr in story:
#    print(str(pr) + '.')
#  nouns = english.glean_nouns(story)
#  for k in nouns:
#    print(nouns[k])
  with open(os.path.join("out", "story.txt"), 'w') as fout:
    fout.write(english.build_story_text(sofar, timeshift="past"))
  with open(os.path.join("out", "facts.lp"), 'w') as fout:
    for pr in sofar:
      fout.write(str(pr) + '.\n')

if __name__ == "__main__":
  if '-s' in sys.argv:
    idx = sys.argv.index('-s')
    storyfile = sys.argv[idx+1]
    main(storyfile)
  else:
    main()
