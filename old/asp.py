"""
asp.py
Python interface to clingo.
"""

import subprocess
import io
import re

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

def clean_clingo_opt_parse(r, b, l):
  """
  Converts a parse of clingo optimization output to a set of predicates.
  """
  return set(r[0][-2]), b, l

ClingoOutput = parser.Hook(
  clean_clingo_parse,
  parser.Seq(
    parser.Rep(
      ans.Predicate
    ),
    "SATISFIABLE",
  )
)

ClingoOptOutput = parser.Hook(
  clean_clingo_opt_parse,
  parser.Seq(
    parser.SepList(
      parser.Rep(
        ans.Predicate
      ),
      parser.Seq(
        "Optimization:",
        re.compile("-?[0-9]+"),
      )
    ),
    "OPTIMUM FOUND",
  )
)

def solve(code, is_opt=False):
  """
  Takes a string containing some answer set code and runs clingo on it,
  returning a set of Predicate objects parsed from clingo's output. Raises an
  ASPError if clingo returns an error code. If is_opt is given and not False,
  then the output will be assumed to be the result of an optimization problem
  rather than a simple satisfiability problem.
  """
  clingo = subprocess.Popen(
    ["clingo", "--verbose=0", "--quiet=1,1", "--seed=0"],
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
  if is_opt:
    return parser.parse_completely(
      stdout.decode(),
      ClingoOptOutput,
      devour=ans.devour_asp
    )
  else:
    return parser.parse_completely(
      stdout.decode(),
      ClingoOutput,
      devour=ans.devour_asp
    )
