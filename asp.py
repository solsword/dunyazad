"""
asp.py
Python interface to clingo.
"""

import subprocess
import io

def solve(code):
  """
  Takes a string containing some answer set code and runs clingo on it,
  returning clingo's complete output as a string. Raises a CalledProcessError
  (from subprocess.check_call) if clingo returns an error code.
  """
  inp = io.StringIO(code)
  outp = io.StringIO()
  errp = io.StringIO()
  subprocess.check_call(["clingo"], stdin=inp, stdout=outp, stderr=errp)
  return outp.read()
