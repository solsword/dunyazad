"""
asp.py
Python interface to clingo.
"""

import subprocess
import io
import re

#TODO: Seed clingo!

class ASPError(Exception):
  def __init__(self, code="unknown", message=''):
    self.message = message
    self.code = code

  def __str__(self):
    return "[error code {}] {}".format(self.code, self.message)

def solve(code):
  """
  Takes a string containing some answer set code and runs clingo on it,
  returning a list of lines from clingo's output. Raises an ASPError if clingo
  returns an error code.
  """
  clingo = subprocess.Popen(
    ["clingo", "--verbose=0", "--quiet=1,1", "--seed=0", "--rand-freq=0.05"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
  )
  stdout, stderr = clingo.communicate(code.encode())
  ret = clingo.returncode
  # clingo returns either 10 or 30 on success (not sure what the difference is)
  if ret not in [10, 30]:
    raise ASPError(
      code=ret,
      message="""
Clingo returned error code {}
--------
-stdin:-
--------
{}
---------
-stdout:-
---------
{}
---------
-stderr:-
---------
{}
""".format(ret, code, stdout.decode(), stderr.decode())
    )
  predicates = (stdout.decode().split('\n'))[0]
  #return ')\n'.join(predicates.split(') '))
  return predicates.split(') ')
