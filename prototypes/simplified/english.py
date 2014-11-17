"""
english.py
Tools for NLG.
"""

from utils import *

pronouns = [
  ("first",   "singular",  "any",  "subjective",  "any",        "I"),
  ("first",   "singular",  "any",  "objective",   "any",        "me"),
  ("first",   "singular",  "any",  "possessive",   "modifier",  "my"),
  ("first",   "singular",  "any",  "possessive",   "object",    "mine"),
             
  ("first",   "plural",    "any",  "subjective",   "any",       "we"),
  ("first",   "plural",    "any",  "objective",    "any",       "us"),
  ("first",   "plural",    "any",  "possessive",   "modifier",  "our"),
  ("first",   "plural",    "any",  "possessive",   "object",    "ours"),

  ("second",  "singular",  "any",  "subjective",  "any",        "you"),
  ("second",  "singular",  "any",  "objective",   "any",        "you"),
  ("second",  "singular",  "any",  "possessive",   "modifier",  "your"),
  ("second",  "singular",  "any",  "possessive",   "object",    "yours"),

  ("second",  "plural",    "any",  "subjective",   "any",       "you"),
  ("second",  "plural",    "any",  "objective",    "any",       "you"),
  ("second",  "plural",    "any",  "possessive",   "modifier",  "your"),
  ("second",  "plural",    "any",  "possessive",   "object",    "yours"),

]

@instance
class number:
  @instance
  class singular: pass
  @instance
  class plural: pass

@instance
class gender:
  @instance
  class masculine: pass
  @instance
  class feminine: pass
  @instance
  class neuter: pass
