#!/usr/bin/env python3
"""
main.py
The main file for story generation.
"""

import asp
import tasks
import english

from utils import *

def main():
  story = tasks.setup_story("")
  while len(list(tasks.all_nodes(story))) < 10:
    try:
      story = tasks.instantiate_random(story)
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
  print(english.build_story_text(story))

if __name__ == "__main__":
  main()
