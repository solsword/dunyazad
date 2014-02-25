"""
Custom packrat parser implementation.
"""

MAX_CONTEXT = 40

# A big dictionary for storing intermediate parsing results:
ratnest = {}

class ParseError(Exception):
  pass

class Sequence:
  def __init__(self, *args):
    self.elements = tuple(args)

  def __hash__(self):
    return 123 + hash(self.elements)

  def __eq__(self, other):
    return type(self) == type(other) and self.elements == other.elements

  def __ne__(self, other):
    return not (self == other)

  def __str__(self):
    return 'Seq({})'.format(', '.join(str(e) for e in self.elements))

Seq = Sequence

class Alternatives:
  def __init__(self, *args):
    self.elements = tuple(args)

  def __hash__(self):
    return 97 + hash(self.elements)

  def __eq__(self, other):
    return type(self) == type(other) and self.elements == other.elements

  def __ne__(self, other):
    return not (self == other)

  def __str__(self):
    return 'Any({})'.format(', '.join(str(e) for e in self.elements))

  def parse(self, text):
    for alt in self.elements:
      result, bubbles, leftovers = packrat_parse(text, alt)
      if not isinstance(leftovers, ParseError):
        return result, bubbles, leftovers
    return (
      None,
      [],
      ParseError(
        "No matches found for {} at '{}'.".format(
          self, 
          text if len(text) <= MAX_CONTEXT else text[:MAX_CONTEXT] + '...'
        )
      )
    )

def packrat_parse(text, thing):
  if (text, thing) in ratnest:
    return ratnest[(text, thing)]
  if hasattr(thing, parse):
    return thing.parse(text)
  elif hasattr(thing, grammar):
    return parse_with_grammar(text, thing, grammar)

def parse_with_grammar(text, thing, grammar):
