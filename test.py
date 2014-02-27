#!/usr/bin/env python
"""
test.py
A stupid-simple test harness.
"""

from utils import *

import sys

import types

default_tests = [
  "utils",
  "parser",
  "tasknet",
  "obj",
  "ans",
]

stop_on_failure = False

def test(module):
  print("Starting tests for module '{}'...".format(module))
  m = __import__(module)
  passed = 0
  failed = 0
  crashed = 0
  for tc in m._test_cases:
    print('-'*40)
    try:
      if len(tc) == 3:
        f, i, o = tc
        if type(i) == tuple:
          r = f(*i)
        else:
          r = f(i)
      elif len(tc) == 2:
        f, o = tc
        i = NoRepr
        r = f()

      if type(r) == types.GeneratorType:
        r = list(r)

      if r == o:
        passed += 1
        print("Test passed! {}({})".format(f.__name__, repr(i)))
      else:
        failed += 1
        print("""\
Test case FAILED: {}({}) produced:
{}
instead of:
{}""".format(f.__name__, repr(i), repr(r), repr(o)),
          file=sys.stderr
        )
        if stop_on_failure:
          break
    except Exception as e:
      crashed += 1
      print("Test case CRASHED:\n{}".format(e), file=sys.stderr)
      sys.excepthook(e.__class__, e, e.__traceback__)
      if stop_on_failure:
        break
  print("""
Stats: {} passed, {} failed, {} crashed.""".format(passed, failed, crashed)
       )
  if failed == 0 and crashed == 0 and passed > 0:
    print ("===all tests passed===")
    return True
  elif failed == 0 and crashed == 0 and passsed == 0:
    print ("???no tests for module '{}'???".format(module))
    return None
  else:
    print ("!!!SOME TESTS FAILED!!!")
    return False

if __name__ == "__main__":
  failed = []
  if "-s" in sys.argv:
    stop_on_failure = True
    sys.argv.remove("-s")
  if "--stop-on-failure" in sys.argv:
    stop_on_failure = True
    sys.argv.remove("--stop-on-failure")
  for m in sys.argv[1:] or default_tests:
    print('-'*80)
    if not test(m):
      failed.append(m)
    if stop_on_failure:
      break
    print('-'*80)
  print('='*80)
  if failed:
    print("[FAIL]: Some tests failed.")
    print(" Defective (see individual results above):")
    print('   ' + '\n   '.join(f for f in failed))
  else:
    print("[pass]: All tests succeeded!")
  print('='*80)
