#!/usr/bin/env python
"""
wholeunit.py

Unit tests for whole-system functionality.
"""

import os
import sys
import re

import main

TEST_DIR = "unit"

TEST_FILES = [
  os.path.join(TEST_DIR, f)
    for f in os.listdir(TEST_DIR)
    if f.endswith(".lp")
]

NODELIMIT = re.compile(r"nodelimit\(([0-9]+)\)")
SHOULDFAIL = re.compile(r"testshouldfail")
SHOULDCRASH = re.compile(r"testshouldcrash")

def wholetest(sf):
  nl = 12
  shouldfail = False
  shouldcrash = False
  with open(sf) as fin:
    fl = fin.readline()
    nlmatch = re.search(NODELIMIT, fl)
    tsfmatch = re.search(SHOULDFAIL, fl)
    tscmatch = re.search(SHOULDCRASH, fl)
    if nlmatch:
      nl = int(nlmatch.group(1))
    if tsfmatch:
      shouldfail = True
    if tscmatch:
      shouldcrash = True
  try:
    success = main.main(scaffoldfiles = [sf], nodelimit = nl)
    return shouldfail ^ success
  except Exception as e:
    return shouldcrash

if __name__ == "__main__":
  for f in sys.argv[1:]:
    if wholetest(f):
      print("Test succeeded.")
    else:
      print("Test failed.")

# Testing:

_test_cases = [
  (
    wholetest,
    f,
    True
  ) for f in TEST_FILES
]
