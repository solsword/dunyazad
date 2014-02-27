#!/usr/bin/env python3
"""
main.py
The main file for story generation.
"""

import traceback

import tasks
import tasknet as tn
import ans

from utils import *

# Defines where to search for global rules:
GLOBAL_RULES_DIR = "global"

def trace_story(net, task, result):
  print('\n'.join(sorted(str(p) + '.' for p in net.mem.code.story)))
  print('-'*40)

def main():
  """
  Sets up a task network and queues up the 'tell_story' task before asking the
  network to run until finished. It then prints out the contents of
  net.mem.code.story.
  """
  net = tn.TaskNet()
  net.mem.code.universal = ans.load_logic(GLOBAL_RULES_DIR)
  net.mem.code.story = set()
  tasks.spawn_task(net, "tell_story")
  print('-'*80)
  print("Trace:")
  print('-'*80)
  leftovers = net.run(maintenance=tn.trace_net)
  #leftovers = net.run(maintenance=trace_story)
  unfinished = [t for t in leftovers if t.status != tn.TaskStatus.Final.Crashed]
  crashed = [t for t in leftovers if t.status == tn.TaskStatus.Final.Crashed]
  print('-'*80)
  print("Unfinished:")
  print('-'*80)
  print('\n'.join(str(t) for t in unfinished))
  print('-'*80)
  print("Crashed:")
  print('-'*80)
  print(
    '\n'.join(
      "{}\n{}".format(
        t,
        format_exception(t.error) if t.error else '<no error?!>'
      ) for t in crashed
    )
  )
  print('-'*80)
  print("Story:")
  print('-'*80)
  print(
    '\n'.join(
      sorted(
        str(predicate) + '.' for predicate in net.mem.code.story
      )
    )
  )
  print('-'*80)

if __name__ == "__main__":
  main()
