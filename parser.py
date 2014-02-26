"""
Custom packrat parser implementation.
"""

from utils import *

import string
import re
import types

############
# Globals: #
############

MAX_ERROR_CONTEXT = 20

RegExpType = re._pattern_type

# A dictionary for storing intermediate parsing results:
_ratnest = {}

######################
# Utility functions: #
######################

def error_context(text):
  if len(text) <= MAX_ERROR_CONTEXT:
    return text
  else:
    return text[:MAX_ERROR_CONTEXT] + '...'

def _default_devour(text):
  """
  The default devour function (strips out whitespace).
  """
  i = 0
  while i < len(text) and text[i] in string.whitespace:
    i += 1
  return text[i:]

##########################
# Miscellaneous classes: #
##########################

class ParseError(Exception):
  """
  An error encountered during parsing.
  """
  pass

@instance
class NoResult:
  """
  An object for use as a parser return value for items that have no result.
  """
  def __bool__(self):
    return False

@abstract
class GrammarElement:
  """
  A class denoting a grammar element. Not used to determine parsing behavior
  (instead the functional property of having a _parse attribute is used).
  """
  pass

@abstract
class Bubble:
  """
  An abstract class denoting a bubble, which is designed to be passed upwards
  during parsing until captured.
  """
  pass

##########################
# Core grammar elements: #
##########################

@attr_object("string", "preserve")
class StrToken(GrammarElement):
  """
  A token matches a string exactly, returning NoResult as a result unless
  "preserve" is given in which case it returns the matched string.
  """

  def __str__(self):
    return "StrToken({}{})".format(
      repr(self.string),
      ', ' + str(self.preserve) if self.preserve else ''
    )

  def _parse(self, text):
    if text.startswith(self.string):
      r = NoResult
      if self.preserve:
        r = self.string
      return r, (), text[len(self.string):]
    else:
      return (
        NoResult,
        (),
        ParseError(
          "StrToken {} failed to match on '{}'.".format(
            self.string,
            error_context(text)
          )
        )
      )

@uniquely_defined_by("expression", "omit")
class REToken(GrammarElement):
  """
  A term that's defined by a regular expression. Matches the given RE and
  returns the string that matched as a result, or NoResult if omit is set to
  True.
  """
  def __init__(self, expression, omit=False):
    self.omit = omit
    if isinstance(expression, str):
      self.expression = re.compile(expression)
    elif not isinstance(expression, RegExpType):
      raise ValueError(
        "REToken must be given either a string or a regular expression.\n" +\
        "Got: {}.".format(repr(expression))
      )
    else:
      self.expression = expression

  def __str__(self):
    return "REToken({})".format(repr(self.expression.pattern))

  def _parse(self, text):
    m = self.expression.match(text)
    if m:
      match = NoResult if self.omit else m.group(0)
      return match, (), text[len(m.group(0)):]
    else:
      return (
        NoResult,
        (),
        ParseError(
          "Expression {} failed to match on '{}'.".format(
            self.expression,
            error_context(text)
          )
        )
      )

def Token(expr, preserve=NotGiven):
  """
  Dynamically create either a StrToken (if given a string) or an REToken (if
  given a compiled regular expression). Whether the match is preserved can be
  specified or it can be left to default as for the type of token selected
  (True for RETokens and False for StrTokens).
  """
  if isinstance(expr, str):
    if preserve == NotGiven:
      return StrToken(expr)
    else:
      return StrToken(expr, preserve)
  elif isinstance(expr, RegExpType):
    if preserve == NotGiven:
      return REToken(expr)
    else:
      return REToken(expr, omit=not preserve)

@attr_object("thing")
class Omit(GrammarElement):
  """
  Parse something but return nothing (NoResult and no bubbles).
  """
  @prevent_recursion()
  def __str__(self):
    return "Omit({})".format(self.thing)

  def _parse(self, text):
    r, b, l = packrat_parse(text, self.thing)
    return NoResult, (), l

@uniquely_defined_by("elements")
class Sequence(GrammarElement):
  """
  Parse a fixed sequence of things.
  """
  def __init__(self, *args):
    self.elements = tuple(args)

  @prevent_recursion()
  def __str__(self):
    return 'Sequence({})'.format(', '.join(str(e) for e in self.elements))

  def _parse(self, text):
    leftovers = text
    results = []
    bubble_cloud = []
    for elem in self.elements:
      result, bubbles, leftovers = packrat_parse(leftovers, elem)
      if isinstance(leftovers, ParseError):
        return NoResult, (), leftovers
      results.append(result)
      bubble_cloud.extend(bubbles)
    return tuple(results), tuple(bubble_cloud), leftovers

@uniquely_defined_by("elements")
class Seq(GrammarElement):
  """
  Parse a fixed sequence of things, returning a condensed list of results that
  doesn't include any instances of NoResult.
  """
  def __init__(self, *args):
    self.elements = tuple(args)

  @prevent_recursion()
  def __str__(self):
    return 'Seq({})'.format(', '.join(str(e) for e in self.elements))

  def _parse(self, text):
    leftovers = text
    results = []
    bubble_cloud = []
    for elem in self.elements:
      result, bubbles, leftovers = packrat_parse(leftovers, elem)
      if isinstance(leftovers, ParseError):
        return NoResult, (), leftovers
      if result != NoResult:
        results.append(result)
      bubble_cloud.extend(bubbles)
    return tuple(results), tuple(bubble_cloud), leftovers

@uniquely_defined_by("elements")
class OneOf(GrammarElement):
  """
  Parse a set of alternatives by trying them in order. Note that this class'
  _parse method is a generator that can generate multiple valid results (it
  will generate a ParseError as part of its output if there are no valid
  results).
  """
  def __init__(self, *args):
    self.elements = tuple(args)

  @prevent_recursion()
  def __str__(self):
    return 'OneOf({})'.format(', '.join(str(e) for e in self.elements))

  def _parse(self, text):
    for alt in self.elements:
      result, bubbles, leftovers = packrat_parse(text, alt)
      if not isinstance(leftovers, ParseError):
        yield result, bubbles, leftovers
    yield (
      NoResult,
      (),
      ParseError(
        "No matches found for {} at '{}'.".format(
          self, 
          error_context(text)
        )
      )
    )

@attr_object("thing", "require_match")
class Rep(GrammarElement):
  """
  Parse a variable number of repetitions of the given thing. If require_match
  is set to True, at least one match will be required, and a ParseError will be
  generated if the thing doesn't match. In the default case (require_match is
  None) an empty list will be returned as a result when the thing doesn't
  match.
  """
  @prevent_recursion()
  def __str__(self):
    return "Rep({}{})".format(
      self.thing,
      (', ' + str(self.require_match)) if self.require_match else ''
    )
  def _parse(self, text):
    results = []
    bubble_cloud = []
    last_good_leftovers = text
    result, bubbles, leftovers = packrat_parse(text, self.thing)
    if self.require_match and isinstance(leftovers, ParseError):
      return NoResult, (), leftovers
    while not isinstance(leftovers, ParseError):
      last_good_leftovers = leftovers
      results.append(result)
      bubble_cloud.extend(bubbles)
      result, bubbles, leftovers = packrat_parse(leftovers, self.thing)
    return tuple(results), tuple(bubble_cloud), last_good_leftovers

@uniquely_defined_by("thing")
class Opt(GrammarElement):
  """
  Parse the given thing but if it doesn't match just return NoResult.
  """
  def __init__(self, thing):
    self.thing = thing

  @prevent_recursion()
  def __str__(self):
    return 'Opt({})'.format(self.thing)

  def _parse(self, text):
    result, bubbles, leftovers = packrat_parse(text, self.thing)
    if isinstance(leftovers, ParseError):
      return NoResult, (), text
    else:
      return result, bubbles, leftovers

@attr_object("name", "value")
class AttributeBubble(Bubble):
  """
  A bubble indicating that a named attribute should be added to an object being
  constructed.
  """
  pass

@attr_object("name", "thing")
class Attr(GrammarElement):
  """
  Parse the given thing and return NoResult as the result, while adding an
  AttributeBubble to the bubble cloud.
  """

  @prevent_recursion()
  def __str__(self):
    return "Attr({}, {})".format(repr(self.name), self.thing)

  def _parse(self, text):
    result, bubbles, leftovers = packrat_parse(text, self.thing)
    bubbles = tuple_with(bubbles, AttributeBubble(self.name, result))
    return NoResult, bubbles, leftovers

@attr_object("name", "thing")
class Flag(GrammarElement):
  """
  Parse the given thing. If it parses, return NoResult as a result and add an
  AttributeBubble with the given name and a value of True to the bubble cloud.
  If it doesn't parse, return NoResult as a result and add an AttributeBubble
  to the bubble cloud with the given name and False as a value.
  """

  @prevent_recursion()
  def __str__(self):
    return "Flag({}, {})".format(self.name, self.thing)

  def _parse(self, text):
    result, bubbles, leftovers = packrat_parse(text, self.thing)
    if isinstance(leftovers, ParseError):
      bubbles = tuple_with(bubbles, AttributeBubble(self.name, False))
      return NoResult, bubbles, text
    else:
      bubbles = tuple_with(bubbles, AttributeBubble(self.name, True))
      return NoResult, bubbles, leftovers

@attr_object("callback", "thing")
class Munge(GrammarElement):
  """
  A Munge grammar element works like a Hook but its callback only affects the
  result returned: it neither gets as arguments nor is expected to return the
  bubbles and leftovers.
  """
  @prevent_recursion()
  def __str__(self):
    return "Munge({}, {})".format(self.callback, self.thing)

  def _parse(self, text):
    r, b, l = packrat_parse(text, self.thing)
    return self.callback(r), b, l

@attr_object("callback", "thing")
class Hook(GrammarElement):
  """
  A Hook grammar element parses text as the given object and then calls the
  given callback function on the result, bubbles, and leftovers before
  returning the modified results.
  """
  @prevent_recursion()
  def __str__(self):
    return "Hook({}, {})".format(self.callback, self.thing)

  def _parse(self, text):
    r, b, l = packrat_parse(text, self.thing)
    r, b, l = self.callback(r, b, l)
    return r, b, l

@attr_object("cls", "thing")
class Package(GrammarElement):
  """
  A Package grammar element creates an instance of a specific class after
  parsing the given text as the given thing. It ignores any result from parsing
  the given text, but it catches AttributeBubble objects in the bubble cloud
  and assigns attributes on the newly created object accordingly. As part of
  this process it calls the constructor of the given class with zero arguments.
  """
  @prevent_recursion()
  def __str__(self):
    return "Package({}, {})".format(self.cls, self.thing)

  def _parse(self, text):
    r, b, l = packrat_parse(text, self.thing)
    obj = self.cls()
    filtered = []
    for bubble in b:
      if isinstance(bubble, AttributeBubble):
        setattr(obj, bubble.name, bubble.value)
      else:
        filtered.append(bubble)
    return obj, tuple(filtered), l

###########################
# Core parsing functions: #
###########################

def packrat_parse(text, thing, devour=_default_devour):
  """
  The main packrat parsing function, which looks up a result in the rat nest
  whenever possible and otherwise delegates to either the target object's
  _parse function or packrat_parse using the target object's grammar attribute.
  Returns a tuple of:

    result - The parsing result.

    bubbles - A tuple of bubble objects that should rise upwards. These can be
      captured by various intermediate parser objects and applied.

    leftovers - A suffix of the given text that remains unparsed. If there's a
      parse error, leftovers will be a ParseError object instead.

  If devour is given, it should be a function that takes text and returns the
  same text with discarded characters chopped off of the front; it will be
  called on the text before further processing.
  """
  text = devour(text)
  if (text, thing) in _ratnest:
    return _ratnest[(text, thing)]
  r, b, l = NoResult, (), ParseError("No result.")
  try:
    if hasattr(thing, "_parse"):
      result = thing._parse(text)
      if isinstance(result, types.GeneratorType):
        r, b, l = next(r)
      else:
        r, b, l = result
    elif hasattr(thing, "grammar"):
      r, b, l = packrat_parse(text, thing.grammar)
    elif isinstance(thing, str):
      r, b, l = packrat_parse(text, StrToken(thing))
    elif isinstance(thing, RegExpType):
      r, b, l = packrat_parse(text, REToken(thing))
    else:
      raise ParseError(
        "No known method for parsing text as a {}.".format(type(thing))
      )
  except Exception as e:
    return (
      NoResult,
      (),
      ParseError(
        "Failed to parse '{}' as {}.\nGot error: {}".format(
          error_context(text), thing, e
        )
      )
    )
  _ratnest[(text, thing)] = (r, b, l)
  return r, b, l

def parse(text, thing, devour=_default_devour):
  """
  Wrapper for packrat_parse that returns just a result object and leftover text
  or raises an error.
  """
  r, b, l = packrat_parse(text, thing)
  if isinstance(l, ParseError):
    raise l
  return r, l

def parse_completely(
  text,
  thing,
  devour=_default_devour,
  devour_leftovers=True
):
  """
  A wrapper for packrat_parse that ensures that the given text parses as the
  given thing without leftovers. Returns just the parse result. By default,
  the devour function will be called on the leftovers before raising an error,
  but this can be turned off by passing devour_leftovers=False.
  """
  r, b, l = packrat_parse(text, thing)
  if isinstance(l, ParseError):
    raise l
  if devour_leftovers:
    l = devour(l)
  if l:
    raise ParseError(
      "Parse successful but incomplete (remainder '{}').".format(
        error_context(l)
      )
    )
  return r

##########
# Sugar: #
##########

Integer = Munge( int, Token(re.compile('-?0|([1-9][0-9]*)')))
Word = Token(re.compile('\w+'))
def SepList(elem, sep=Token(',')):
  return Munge(
    lambda L: tuple([L[0]] + [e[0] for e in L[1]]),
    Seq(
      elem,
      Rep(
        Seq(
          Omit(sep),
          elem
        )
      )
    )
  )

######################
# Testing functions: #
######################

def _test_parse(thing, bubbles=NotGiven, leftovers=NotGiven):
  def _test_parser(text):
    r, b, l = packrat_parse(text, thing)
    if bubbles != NotGiven and b != bubbles:
      raise ParseError(
        "Bubbles didn't match.\nGot: {}\nExpected: {}".format(
          b,
          bubbles
        )
      )
    if leftovers != NotGiven and l != leftovers:
      raise ParseError(
        "Leftovers didn't match.\nGot: {}\nExpected: {}".format(
          l,
          leftovers
        )
      )
    if leftovers == NotGiven and isinstance(leftovers, ParseError):
      raise leftovers
    return r
  return _test_parser

@uniquely_defined_by("a", "b")
class _TestObject:
  def __init__(self, a=None, b=None):
    self.a = a or []
    self.b = b or 5

_test_grammar_1 = Seq(
  Token("to"),
  Token("("),
  Opt(
    Seq(
      Attr("a", Integer),
      Token(","),
    ),
  ),
  Attr("b", Integer),
  Token(")"),
)

_test_grammar_1.elements = tuple_with(
  _test_grammar_1.elements,
  Opt(Attr("recurisve", _test_grammar_1))
)

# the same as _test_grammar_1
_test_grammar_2 = Seq(
  Token("to"),
  Token("("),
  Opt(
    Seq(
      Attr("a", Integer),
      Token(","),
    ),
  ),
  Attr("b", Integer),
  Token(")"),
)

_test_grammar_2.elements = tuple_with(
  _test_grammar_2.elements,
  Opt(Attr("recurisve", _test_grammar_2))
)

###############
# Test cases: #
###############

_test_cases = [
  (
    _test_parse( Seq(Token("foo", True), Token("bar", True)) ),
    "foo bar",
    ("foo", "bar"),
  ),
  (
    _test_parse(
      Package(
        _TestObject,
        Seq(
          Token("to"),
          Token("("),
          Attr("a", Integer),
          Token(","),
          Attr("b", Word),
          Token(")"),
        )
      ),
    ),
    "to(4, 7)",
    _TestObject(a=4, b="7")
  ),
  (
    _test_parse(
      Package(
        _TestObject,
        Seq(
          Token("to"),
          Token("("),
          Opt(
            Seq(
              Attr("a", Integer),
              Token(","),
            ),
          ),
          Attr("b", Integer),
          Token(")"),
        )
      ),
    ),
    "to(7)",
    _TestObject(b=7)
  ),
  (
    _test_parse( Sequence( Word, StrToken("("), Integer, ",", Integer, ")" ) ),
    "test(3, 4)",
    ( "test", NoResult, 3, NoResult, 4, NoResult )
  ),
  (
    _test_parse( Seq( Word, "(", SepList( Integer ), ")" ) ),
    "test(3, 4, 5, 6)",
    ( "test", ( 3, 4, 5, 6 ) )
  ),
  (
    _test_parse( Seq( Word, "(", SepList( Integer, sep=";" ), ")" ) ),
    "test(3; 4; 5; 6)",
    ( "test", ( 3, 4, 5, 6 ) )
  ),
  (
    hash,
    Package(
      _TestObject,
      Seq(
        Token("to"),
        Token("("),
        Opt(
          Seq(
            Attr("a", Integer),
            Token(","),
          ),
        ),
        Attr("b", Integer),
        Token(")"),
      )
    ),
    hash(
      Package(
        _TestObject,
        Seq(
          Token("to"),
          Token("("),
          Opt(
            Seq(
              Attr("a", Integer),
              Token(","),
            ),
          ),
          Attr("b", Integer),
          Token(")"),
        )
      )
    )
  ),
  (
    lambda x: x, # test object equality
    Package(
      _TestObject,
      Seq(
        Token("to"),
        Token("("),
        Opt(
          Seq(
            Attr("a", Integer),
            Token(","),
          ),
        ),
        Attr("b", Integer),
        Token(")"),
      )
    ),
    Package(
      _TestObject,
      Seq(
        Token("to"),
        Token("("),
        Opt(
          Seq(
            Attr("a", Integer),
            Token(","),
          ),
        ),
        Attr("b", Integer),
        Token(")"),
      )
    )
  ),
  (
    lambda x: x, # test recursive object equality
    _test_grammar_1,
    _test_grammar_2
  ),
]
