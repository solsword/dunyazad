"""
verbs.py
Code for handling verbs and conjugation.
"""

from utils import *
from eng_base import *

def base(verb):
  return verb

def add_s(verb):
  if (
    any(verb.endswith(e) for e in ["s", "z", "ch", "sh"])
  or
    verb[-2] in consonants and verb[-1] == "o"
  ):
    return verb + "es"
  elif verb[-2] in consonants and verb[-1] == "y":
    return verb[:-1] + "ies"
  else:
    return verb + "s"

def add_ed(verb):
  # TODO: Final consonant doubling?
  if verb.endswith("e"):
    return verb + "d"
  elif verb[-2] in consonants and verb[-1] == "y":
    return verb[:-1] + "ied"
  else:
    return verb + "ed"

def add_ing(verb):
  # TODO: Consonant doubling here as well?
  if verb.endswith("ie"):
    return verb[:-2] + "ying"
  elif verb.endswith("e") and len(verb) > 2:
    return verb[:-1] + "ing"
  else:
    return verb + "ing"

CONJ_DEFAULTS = [
  ("present", "singular", "first", base),
  ("present", "singular", "second", base),
  ("present", "singular", "third", add_s),

  ("present", "plural", "any", base),

  ("past", "any", "any", add_ed),

  ("infinitive", "any", "any", base),

  ("imperative", "any", "any", base),

  ("present participle", "any", "any", add_ing),

  ("past participle", "any", "any", add_ed),
]

IRREGULAR = {
  "be": [
    ("present", "singular", "first", "am"),
    ("present", "singular", "second", "are"),
    ("present", "singular", "third", "is"),

    ("present", "plural", "any", "are"),

    ("past", "singular", "first", "was"),
    ("past", "singular", "second", "were"),
    ("past", "singular", "third", "was"),

    ("past", "plural", "any", "were"),

    ("past participle", "any", "any", "been"),
  ],
  "do": [
    ("past", "any", "any", "did"),
    ("past participle", "any", "any", "done"),
  ],
  "have": [
    ("present", "singular", "third", "has"),
    ("past", "any", "any", "had"),
    ("past participle", "any", "any", "had"),
  ]
}

def conjugation(verb, tns="present", nmbr="any", per="any"):
  """
  Figures out the conjugation of the given verb and returns it.
  """
  lookup = (tns, nmbr, per)
  if verb in IRREGULAR:
    irr = table_match(IRREGULAR[verb], lookup)
    if irr:
      return irr
    # else fall out:
  return table_match(CONJ_DEFAULTS, lookup)(verb)

def conj_ref(thing, verb, tns):
  """
  Uses the given noun to help conjugate the given verb (but still needs a tense
  of course).
  """
  return conjugation(verb, tns, thing.number, thing.person)
