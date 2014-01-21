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
        print("""\
Test case FAILED: {}({}) produced:
{}
instead of:
{}""".format(f.__name__, i, r, o),
          file=sys.stderr
        )
    except Exception as e:
      crashed += 1
      print("Test case CRASHED: {}".format(e), file=sys.stderr)
      sys.excepthook(e.__class__, e, e.__traceback__)
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
  for m in sys.argv[1:] or default_tests:
    print('-'*80)
    if not test(m):
      failed.append(m)
    print('-'*80)
  print('='*80)
  if failed:
    print("[FAIL]: Some tests failed.")
    print(" Defective (see individual results above):")
    print('   ' + '\n   '.join(f for f in failed))
  else:
    print("[pass]: All tests succeeded!")
  print('='*80)
