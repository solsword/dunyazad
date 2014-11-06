#!/usr/bin/env python3
"""
main.py
The main file for story generation.
"""

import asp
import tasks

from utils import *

def main():
  story = tasks.setup_story("")
  story = tasks.instantiate_random(story);
  story = tasks.branch_random(story);
  story = tasks.instantiate_random(story);
  story = tasks.branch_random(story);
  for pr in story:
    print(str(pr) + '.')

if __name__ == "__main__":
  main()
