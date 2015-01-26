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

def wholetest(sf):
  with open(sf) as fin:
    fl = fin.readline()
    nl = 12
    nlmatch = re.search(NODELIMIT, fl)
    if nlmatch:
      nl = int(nlmatch.group(1))
  try:
    success = main.main(scaffoldfile = sf, nodelimit = nl)
    return success
  except:
    return False

if __name__ == "__main__":
  for f in sys.argv[1:]:
    wholetest(f)

# Testing:

_test_cases = [
  (
    wholetest,
    f,
    True
  ) for f in TEST_FILES
]
