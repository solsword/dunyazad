#!/usr/bin/env python3
"""
main.py
The main file for story generation.
"""

import os
import os.path

import asp
import tasks
import english

from utils import *

def main():
  story = tasks.setup_story("")
  n = -1
  if not os.path.isdir("out"):
    os.mkdir("out")
  if not os.path.isdir(os.path.join("out", "snapshots")):
    os.mkdir(os.path.join("out", "snapshots"))
  while len(list(tasks.all_nodes(story))) < 6:
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
      print("Error during instantiation. Dumping source to 'crash.lp'")
      with open("crash.lp", 'w') as fout:
        fout.write(e.message)
      exit(1)

    try:
      story = tasks.branch_random(story)
    except asp.ASPError as e:
      print("Error during branching. Dumping source to 'crash.lp'")
      with open("crash.lp", 'w') as fout:
        fout.write(e.message)
      exit(1)
#  for pr in story:
#    print(str(pr) + '.')
#  nouns = english.glean_nouns(story)
#  for k in nouns:
#    print(nouns[k])
  with open(os.path.join("out", "story.txt"), 'w') as fout:
    fout.write(english.build_story_text(story))
  with open(os.path.join("out", "facts.lp"), 'w') as fout:
    for pr in story:
      fout.write(str(pr) + '.\n')

if __name__ == "__main__":
  main()
