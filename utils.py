'''
utils.py
Utility functions.
'''

import sys

def warn(w):
  sys.stderr.write("WARNING--" + w + "\n")

def err(e):
  sys.stderr.write("ERROR--" + e + "\n")

def singleton(string):
  def wrap(cls):
    def __init__(self):
      raise NotImplementedError("Class {} is a singleton.".format(cls.__name__))
    def __str__(self):
      return string
    cls.__init__ = __init__
    cls.__str__ = __str__
    return cls
  return wrap
