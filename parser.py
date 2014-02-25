"""
Custom packrat parser implementation.
"""

from utils import *

MAX_ERROR_CONTEXT = 40

# A big dictionary for storing intermediate parsing results:
ratnest = {}

class ParseError(Exception):
  pass

@uniquely_defined_by("elements")
class Sequence:
  """
  Parse a fixed sequence of things.
  """
  def __init__(self, *args):
    self.elements = tuple(args)

  def __str__(self):
    return 'Seq({})'.format(', '.join(str(e) for e in self.elements))

  def parse(self, text):
    leftovers = text
    results = []
    bubble_cloud = []
    for elem in self.elements:
      result, bubbles, leftovers = packrat_parse(leftovers, elem)
      if isinstance(leftovers, ParseError):
        return None, (), leftovers
      results.append(result)
      bubble_cloud.extend(bubbles)
    return tuple(results), tuple(bubble_cloud), leftovers

Seq = Sequence

@uniquely_defined_by("elements")
class Alternatives:
  """
  Parse a set of alternatives by trying them in order. Note that this class'
  parse method is a generator that can generate multiple valid results (it will
  generate a ParseError as part of its output if there are no valid results).
  """
  def __init__(self, *args):
    self.elements = tuple(args)

  def __str__(self):
    return 'Any({})'.format(', '.join(str(e) for e in self.elements))

  def parse(self, text):
    for alt in self.elements:
      result, bubbles, leftovers = packrat_parse(text, alt)
      if not isinstance(leftovers, ParseError):
        yield result, bubbles, leftovers
    yield (
      None,
      (),
      ParseError(
        "No matches found for {} at '{}'.".format(
          self, 
          text
            if len(text) <= MAX_ERROR_CONTEXT
            else text[:MAX_ERROR_CONTEXT] + '...'
        )
      )
    )

@uniquely_defined_by("thing", "require_match")
class Repeated:
  """
  Parse a variable number of repetitions of the given thing. If require_match
  is set to True, at least one match will be required, and a ParseError will be
  generated if the thing doesn't match. In the default case (require_match is
  False) an empty list will be returned as a result when the thing doesn't
  match.
  """
  def __init__(self, thing, require_match=False):
    self.thing = thing

  def parse(self, text):
    results = []
    bubble_cloud = []
    last_good_leftovers = text
    result, bubbles, leftovers = packrat_parse(text, self.thing)
    if self.require_match and isinstance(leftovers, ParseError):
      return None, (), leftovers
    while not isinstance(leftovers, ParseError):
      last_good_leftovers = leftovers
      results.append(result)
      bubble_cloud.extend(bubbles)
      result, bubbles, leftovers = packrat_parse(leftovers, self.thing)
    return tuple(results), tuple(bubble_cloud), last_good_leftovers

Rep = Repeated

@uniquely_defined_by("thing")
class Optional:
  """
  Parse the given thing but if it doesn't match just return None as a result.
  """
  def __init__(self, thing):
    self.thing = thing

  def __str__(self):
    return 'Opt({})'.format(self.thing)

  def parse(self, text):
    result, bubbles, leftovers = packrat_parse(text, alt)
    if isinstance(leftovers, ParseError):
      return None, (), text
    else:
      return result, bubbles, leftovers

Opt = Optional

@attr_object("name", "value")
class AttributeBubble:
  """
  A bubble indicating that a named attribute should be added to an object being
  constructed.
  """
  pass

@attr_object("name", "thing")
class Attribute:
  """
  Parse the given thing and return None as the result, while adding an
  Attribute bubble to the bubble cloud.
  """

  def __str__(self):
    return "Attr({}, {})".format(self.name, self.thing)

  def parse(self, text):
    result, bubbles, leftovers = packrat_parse(text, self.thing)
    bubbles = tuple(list(bubbles) + [AttributeBubble(self.name, result)])
    return None, bubbles, leftovers

Attr = Attribute

@attr_object("name", "thing")
class Flag:
  """
  Parse the given thing. If it parses, return None as a result and add an
  AttributeBubble with the given name and a value of True to the bubble cloud.
  If it doesn't parse, return None as a result and add an AttributeBubble to
  the bubble cloud with the given name and False as a value.
  """

  def __str__(self):
    return "Flag({}, {})".format(self.name, self.thing)

  def parse(self, text):
    result, bubbles, leftovers = packrat_parse(text, self.thing)
    if isinstance(leftovers, ParseError):
      bubbles = tuple(list(bubbles) + [AttributeBubble(self.name, False)])
      return None, bubbles, text
    else:
      bubbles = tuple(list(bubbles) + [AttributeBubble(self.name, True)])
      return None, bubbles, leftovers

def packrat_parse(text, thing):
  if (text, thing) in ratnest:
    return ratnest[(text, thing)]
  r = None
  if hasattr(thing, parse):
    result = thing.parse(text)
    if isinstance(result, types.GeneratorType):
      r = next(result)
    else:
      r = result
  elif hasattr(thing, grammar):
    r = parse_with_grammar(text, thing, grammar)
  ratnest[(text, thing)] = r
  return r

def parse_with_grammar(text, thing, grammar):
  result, bubbles, leftovers = packrat_parse(text, grammar)
  pass
