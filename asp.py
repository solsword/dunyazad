"""
asp.py
Python interface to clingo.
"""

import subprocess
import io

class ASPError(Exception):
  pass

def solve(code):
  """
  Takes a string containing some answer set code and runs clingo on it,
  returning clingo's complete output as a string. Raises a ASPError if clingo
  returns an error code.
  """
  clingo = subprocess.Popen(
    ["clingo"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
  )
  stdout, stderr = clingo.communicate(code.encode())
  ret = clingo.returncode
  if ret != 0:
    raise ASPError(
      """
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
  return stdout
