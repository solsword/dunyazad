"""
asp.py
Python interface to clingo.
"""

import subprocess
import io

import ans
import parser

#TODO: Seed clingo!

class ASPError(Exception):
  def __init__(self, code="unknown", message=''):
    self.message = message
    self.code = code

  def __str__(self):
    return "[error code {}] {}".format(self.code, self.message)

# A grammar for clingo output:

def clean_clingo_parse(r, b, l):
  """
  Converts a parse of clingo output to a set of predicates.
  """
  return set(r[0]), b, l

ClingoOutput = parser.Hook(
  clean_clingo_parse,
  parser.Seq(
    parser.Rep(
      ans.Predicate
    ),
    "SATISFIABLE"
  )
)

def solve(code):
  """
  Takes a string containing some answer set code and runs clingo on it,
  returning a set of Predicate objects parsed from clingo's output. Raises an
  ASPError if clingo returns an error code.
  """
  clingo = subprocess.Popen(
    ["clingo", "--verbose=0"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
  )
  stdout, stderr = clingo.communicate(code.encode())
  ret = clingo.returncode
  if ret != 10: # clingo returns 10 on success for some reason
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
  return parser.parse_completely(
    stdout.decode(),
    ClingoOutput,
    devour=ans.devour_asp
  )
