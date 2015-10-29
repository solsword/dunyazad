"""
nouns.py
Code for handling nouns and pronouns.
"""

from utils import *
from eng_base import *

PRONOUNS = [
  ("first",   "singular",  "any",       "subjective", "any",      "I"),
  ("first",   "singular",  "any",       "objective",  "any",      "me"),
  ("first",   "singular",  "any",       "possessive", "modifier", "my"),
  ("first",   "singular",  "any",       "possessive", "object",   "mine"),
  ("first",   "singular",  "any",       "reflexive",  "any",      "myself"),
                                        
  ("first",   "plural",    "any",       "subjective", "any",      "we"),
  ("first",   "plural",    "any",       "objective",  "any",      "us"),
  ("first",   "plural",    "any",       "possessive", "modifier", "our"),
  ("first",   "plural",    "any",       "possessive", "object",   "ours"),
  ("first",   "plural",    "any",       "reflexive",  "any",      "ourselves"),
                                        
  ("second",  "singular",  "any",       "subjective", "any",      "you"),
  ("second",  "singular",  "any",       "objective",  "any",      "you"),
  ("second",  "singular",  "any",       "possessive", "modifier", "your"),
  ("second",  "singular",  "any",       "possessive", "object",   "yours"),
  ("second",  "singular",  "any",       "reflexive",  "any",      "yourself"),
                                        
  ("second",  "plural",    "any",       "subjective", "any",      "you"),
  ("second",  "plural",    "any",       "objective",  "any",      "you"),
  ("second",  "plural",    "any",       "possessive", "modifier", "your"),
  ("second",  "plural",    "any",       "possessive", "object",   "yours"),
  ("second",  "plural",    "any",       "reflexive",  "any",      "yourselves"),

  ("third",   "singular",  "masculine", "subjective", "any",      "he"),
  ("third",   "singular",  "masculine", "objective",  "any",      "him"),
  ("third",   "singular",  "masculine", "possessive", "modifier", "his"),
  ("third",   "singular",  "masculine", "possessive", "object",   "his"),
  ("third",   "singular",  "masculine", "reflexive",  "any",      "himself"),

  ("third",   "singular",  "feminine",  "subjective", "any",      "she"),
  ("third",   "singular",  "feminine",  "objective",  "any",      "her"),
  ("third",   "singular",  "feminine",  "possessive", "modifier", "her"),
  ("third",   "singular",  "feminine",  "possessive", "object",   "hers"),
  ("third",   "singular",  "feminine",  "reflexive",  "any",      "herself"),

  ("third",   "singular",  "neuter",    "subjective", "any",      "it"),
  ("third",   "singular",  "neuter",    "objective",  "any",      "it"),
  ("third",   "singular",  "neuter",    "possessive", "modifier", "its"),
  ("third",   "singular",  "neuter",    "possessive", "object",   "its"),
  ("third",   "singular",  "neuter",    "reflexive",  "any",      "itself"),

  ("third",   "plural",    "any",       "subjective", "any",      "they"),
  ("third",   "plural",    "any",       "objective",  "any",      "them"),
  ("third",   "plural",    "any",       "possessive", "modifier", "their"),
  ("third",   "plural",    "any",       "possessive", "object",   "theirs"),
  ("third",   "plural",    "any",       "reflexive",  "any",      "themselves"),
]

class Noun:
  def __init__(
    self,
    tag,
    types=None,
    name="nameless",
    person="third",
    number="singular",
    gender="neuter",
    determined=True,
    owner=None,
    is_party_member=False,
  ):
    self.tag = tag
    self.types = types or {}
    self.name = name
    self.person = person
    self.number = number
    self.gender = gender
    self.determined = determined
    self.owner = owner
    self.is_party_member = is_party_member

  def __str__(self):
    return "Noun[{}.{}/{}][{}/{}]".format(
      self.tag,
      self.root_type(),
      self.name,
      definite(self),
      pronoun(self)
    )

  def __repr__(self):
    return "Noun({}, {}, {}, {}, {}, {}, {}, {})".format(
      self.tag,
      self.root_type(),
      self.name,
      self.person,
      self.number,
      self.gender,
      self.determined,
      self.is_party_member,
    )

  def type_at(self, node):
    if node in self.types:
      return self.types[node]
    else:
      return "<unknown>"

  def root_type(self):
    return self.type_at("root")

def pnslot(thing):
  """
  Returns the pronouns slot that the given thing occupies.
  """
  lookup = (thing.person, thing.number, thing.gender, "any", "any")
  result = table_match(PRONOUNS, lookup)
  if result:
    if result == "he" or result == "she":
      return "he/she"
    else:
      return result
  else:
    raise KeyError(
      "Pronoun lookup failed for '{}'. No match for conditions: {}".format(
        str(thing),
        lookup
      )
    )

def pronoun(thing, case = "subjective", position = "object"):
  """
  Computes and returns the desired pronoun. The given 'thing' should have
  'person', 'number' and 'gender' attributes. Note that "any" is a special
  case: it will match any other value. If this would result in multiple
  possible matches, the first valid match in the PRONOUNS table is used.
  """
  lookup = (thing.person, thing.number, thing.gender, case, position)
  return table_match(PRONOUNS, lookup)

def pr_ref(thing, ref):
  """
  Finds a pronoun that has the same person, case and position as the given
  reference pronoun, but which has the number and gender of the given thing.
  Note that where multiple pronouns matching the reference exist (e.g., "you"),
  the first one in the PRONOUNS table is used (for "you", this would result in
  "second," "subjective," and "any" for person, case, and position
  respectively).
  """
  person = "third"
  case = "subjective"
  position = "object"
  for pr in PRONOUNS:
    if pr[-1] == ref:
      person, _n, _g, case, position, _p = pr
      break
  return pronoun(thing, person, case, position)

def casepos(pronoun):
  """
  A reverse lookup for case and position given a pronoun (the third person
  plural forms are ideal as they're unambiguous). If the pronoun is ambiguous,
  the first hit in the PRONOUNS table will be returned. If 
  """
  for person, number, gender, case, position, pro in PRONOUNS:
    if pro == pronoun:
      return (case, position)
  raise ValueError("Unknown reference pronoun '{}'.".format(pronoun))

def definite(thing, case="subjective"):
  """
  Returns a definite reference to the given noun, e.g. "the monster" or "Sam"
  """
  result = ""
  if thing.owner:
    result = "N#{}/their {}".format(thing.owner.tag, thing.name)
  elif thing.determined:
    result = "the {}".format(thing.name)
  else:
    result = thing.name
  if case == "possessive":
    if result[-1] in "sz":
      return result + "’"
    else:
      return result + "’s"
  else:
    return result

def indefinite(thing, case="subjective"):
  result = ""
  if not thing.determined:
    result = "a {} named {}".format(thing.root_type(), thing.name)
  else:
    if thing.number == "singular":
      if thing.name[0] in vowels: # TODO: better than this T_T
        result = "an {}".format(thing.name)
      else:
        result = "a {}".format(thing.name)
    else:
      result = "some {}".format(thing.name)
  if case == "possessive":
    if result[-1] in "sz":
      return result + "'"
    else:
      return result + "'s"
  else:
    return result
