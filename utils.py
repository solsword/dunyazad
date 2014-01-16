'''
utils.py
Utility functions.
'''

import sys

def warn(w):
  sys.stderr.write("WARNING--" + w + "\n")

def err(e):
  sys.stderr.write("ERROR--" + e + "\n")
