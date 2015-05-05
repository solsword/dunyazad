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

FASTPARSE = True

class ASPError(Exception):
  def __init__(
    self,
    message='',
    retcode="unknown",
    program='',
    stdout='',
    stderr=''
  ):
    self.message = message
    self.retcode = retcode
    self.program = program
    self.stdout = stdout
    self.stderr = stderr

  def __str__(self):
    return "[error code {}] {}".format(self.retcode, self.message)

# A grammar for clingo output:

def clean_clingo_parse(r, b, l):
  """
  Converts a parse of clingo output to a list of predicates.
  """
  return list(r[0]), b, l

def clean_clingo_opt_parse(r, b, l):
  """
  Converts a parse of clingo optimization output to a list of predicates.
  """
  return list(r[0][-2]), b, l

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

def solve(code, seed=0, rand=0.0):
  """
  Takes a string containing some answer set code and runs clingo on it,
  returning a set of Predicate objects parsed from clingo's output. Raises an
  ASPError if clingo returns an error code.
  """
  clingo = subprocess.Popen(
    [
      "clingo",
      "--verbose=0",
      "--quiet=1,1",
      "--seed={}".format(seed),
      "--rand-freq={:.3f}".format(rand)
    ],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
  )
  stdout, stderr = clingo.communicate(code.encode())
  ret = clingo.returncode
  # clingo returns either 10 or 30 on success (not sure what the difference is)
  if ret not in [10, 30]:
    raise ASPError(
      message="Clingo returned error code {}".format(ret),
      retcode=ret,
      program=code,
      stdout = stdout.decode(),
      stderr = stderr.decode()
    )
  lines = stdout.decode().split('\n')
  return ans.parse_ans_fast(lines[0])
