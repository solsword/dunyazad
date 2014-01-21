#!/usr/bin/env python
"""
test.py
A stupid-simple test harness.
"""

from utils import *

import sys

default_tests = [
  "parser",
  "tasknet",
  "obj",
]

def test(module):
  m = __import__(module)
  print("Starting tests for module '{}'...".format(module))
  passed = 0
  failed = 0
  crashed = 0
  for tc in m._test_cases:
    try:
      if len(tc) == 3:
        f, i, o = tc
        r = f(i)
      elif len(tc) == 2:
        f, o = tc
        i = norepr
        r = f()
      if r == o:
        passed += 1
        print("Test passed! {}({})".format(f.__name__, repr(i)))
      else:
        failed += 1
        err("""\
Test case FAILED: {}({}) produced:
{}
instead of:
{}
""".format(f.__name__, i, r, o)
        )
    except Exception as e:
      crashed += 1
      err("Test case CRASHED: {}".format(e))
      sys.excepthook(e.__class__, e, e.__traceback__)
  print(
    "Stats: {} passed, {} failed, {} crashed.".format(passed, failed, crashed)
  )

if __name__ == "__main__":
  for m in sys.argv[1:] or default_tests:
    test(m)
