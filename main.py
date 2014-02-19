#!/usr/bin/env python3
"""
main.py
The main file for story generation.
"""

import tasks
import tasknet as tn

from utils import *

# Defines where to search for global rules:
GLOBAL_RULES_DIR = "global"

def main():
  """
  Sets up a task network and queues up the 'tell_story' task before asking the
  network to run until finished. It then prints out the contents of
  net.mem.code.story.
  """
  net = tn.TaskNet()
  net.mem.code.universal = load_logic(GLOBAL_RULES_DIR)
  net.mem.code.story = set()
  tasks.spawn_task(net, "tell_story")
  net.run()
  print(net.mem.code.story)

if __name__ == "__main__":
  main()
