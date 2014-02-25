"""
ans.py
Answer set data structures.
"""

from utils import *

import re
import string

import pypeg2 as peg # for parsing functionality

# A dictionary of regular expressions for various tokens:
class Tokens:
  QUERY_MARK = re.compile(r"\?")
  CONS = re.compile(r":-")
  WCONS = re.compile(r":~")
  DOT = re.compile(r"\.((?=[^.])|$)")
    # Matches a period NOT followed by another, but doesn't eat the following
    # character.
  AT = re.compile(r"@")
  OR = re.compile(r"|")
  NAF = re.compile(r"not")

  COMMA = re.compile(r",")
  COLON = re.compile(r":")
  SEMICOLON = re.compile(r";")

  OP_PLUS = re.compile(r"\+")
  OP_MINUS = re.compile(r"-")
  OP_TIMES = re.compile(r"\*((?=[^*])|$)")
  OP_DIV = re.compile(r"/")
  OP_MOD = re.compile(r"\\")
  OP_POW = re.compile(r"\*\*")
  OP_BITWISE_AND = re.compile(r"&")
  OP_BITWISE_OR = re.compile(r"\?")
  OP_BITWISE_XOR = re.compile(r"\^")
  OP_BITWISE_NEG = re.compile(r"~")

  CMP_EQ = re.compile(r"=")
  CMP_NEQ = re.compile(r"(<>)|(!=)")
  CMP_GT = re.compile(r">((?=[^=])|$)")
  CMP_LT = re.compile(r"<((?=[^=])|$)")
  CMP_GE = re.compile(r">=")
  CMP_LE = re.compile(r"<=")

  INTERVAL = re.compile(r"\.\.")

  ANONYMOUS = re.compile(r"_((?=[^a-zA-Z0-9_])|$)")
    # Matches an underscore NOT followed any further word characters, but
    # doesn't eat the following character.

  NUMBER = re.compile(r"0|([1-9][0-9]*)")

  ID = re.compile(r"[a-z][A-Za-z0-9_]*")
  VARIABLE = re.compile(r"[A-Z][A-Za-z0-9_]*")

  STRING = re.compile(r'"([^\\"]|(\\.))*"')
    # Matches a starting quote, followed by any number of tokens which are
    # either non-backslash, non-quote characters or which are escape codes ('\'
    # plus any character), finished by an ending quote.

  DIR_HIDE = re.compile(r"#hide")
  DIR_SHOW = re.compile(r"#show")
  DIR_CONST = re.compile(r"#const")
  DIR_DOMAIN = re.compile(r"#domain")
  DIR_EXTERNAL = re.compile(r"#external")

  DIRECTIVE_BODY = re.compile(r"[^.]*(?=\.)")
    # Matches any number of non-period characters followed by a period, but
    # doesn't eat the period.

  ABS = re.compile(r"\|")
  PAREN_OPEN = re.compile(r"\(")
  PAREN_CLOSE = re.compile(r"\)")
  CURLY_OPEN = re.compile(r"\{")
  CURLY_CLOSE = re.compile(r"\}")
  SQUARE_OPEN = re.compile(r"\[")
  SQUARE_CLOSE = re.compile(r"\]")

  KW_AGGREGATE_COUNT = re.compile(r"#count")
  KW_AGGREGATE_MIN = re.compile(r"#min")
  KW_AGGREGATE_MAX = re.compile(r"#max")
  KW_AGGREGATE_SUM = re.compile(r"#sum")

  KW_MAXIMIZE = re.compile(r"#maximi[sz]e")
  KW_MINIMIZE = re.compile(r"#minimi[sz]e")

  LOOSE_CONSTANT = re.compile(r"_*[a-z][a-zA-Z0-9_]*")
  LOOSE_VARIABLE = re.compile(r"_*[A-Z][a-zA-Z0-9_]*")
    # Matches some number of optional starting underscores, a first-position
    # alphabetic character, and then any number of alphanumerics and/or
    # underscores.

  LOOSE_INTEGER = re.compile(r"-?[0-9]+")

  KW_INFIMUM = re.compile(r"#infimum")
  KW_SUPREMUM = re.compile(r"#supremum")

  def ignore():
    '''
    A place to store ignored versions of the normal attributes.
    '''
    pass

for attr in Tokens.__dict__:
  if not attr.startswith("__") and not attr.endswith("__"):
    setattr(Tokens.ignore, attr, peg.omit(getattr(Tokens, attr)))

class NotGiven:
  pass

#def ensure_attr(attr, val=NotGiven, cons=NotGiven):
#  if val == NotGiven and cons == NotGiven:
#    raise ValueError("ensure_attr must be given one of 'val' or 'cons.'")
#  def munge(result):
#    nonlocal attr, val, cons
#    if not hasattr(result, attr):
#      if val != NotGiven:
#        setattr(result, attr, val)
#      elif cons != NotGiven:
#        setattr(result, attr, cons())
#    return result
#  return munge

def ensure_attr(attr, default, parse_as):
  class EnsureAttr:
    @classmethod
    def parse(cls, parser, text, pos):
      leftovers, result = parser.parse(text, parse_as)
      if getattr(result, attr) == None:
        setattr(result, attr, default)
      return result
  return EnsureAttr()

class Predicate:
  """
  A Predicate represents a predicate structure, something like:

    foo(bar, baz(xyzzy))

  Each Predicate object has a name and 0 or more arguments. Generally, unless
  the name begins and ends with a '"' character, it should only contain
  characters from the class [a-zA-Z0-9_] and its first character should be in
  [a-z]. This is not a strict rule however.
  """
  def __init__(self, name="_", *args):
    self.name = name
    self.args = list(args)

  def __str__(self):
    if self.args:
      return "{}({})".format(self.name, ', '.join(str(a) for a in self.args))
    else:
      return str(self.name)

  def __repr__(self):
    return "Predicate({}{})".format(
      repr(self.name),
      ', '.join([""] + [repr(a) for a in self.args])
    )

  def __hash__(self):
    return 47 * (
      47 * (
        31 + hash(self.args)
      ) + hash(self.name)
    )

  def __eq__(self, other):
    return type(self) == type(other) \
        and self.name == other.name \
        and self.args == other.args

  def __ne__(self, other):
    return not self == other

  def matches(self, other):
    return self.name == other.name and len(self.args) == len(other.args)

  def _cleanup(self):
    if self.args == None:
      self.args = list()

Predicate.grammar = [
  (
    peg.attr(
      "name",
      [ Tokens.STRING, Tokens.LOOSE_CONSTANT ]
    ),
    peg.optional(
      (
        "(",
        peg.attr(
          "args",
          peg.csl( Predicate, separator=Tokens.ignore.COMMA )
        ),
        ")"
      )
    )
  ),
  peg.attr(
    "name",
    [
      Tokens.ANONYMOUS,
      Tokens.LOOSE_INTEGER,
      Tokens.KW_INFIMUM,
      Tokens.KW_SUPREMUM,
    ]
  ),
]


class Variable(Predicate):
  """
  Variables can be inserted into predicate structures to do predicate binding.
  Generally their names should begin with uppercase letters (class [A-Z]) but
  should otherwise follow the conventions for Predicate names. A normal
  variable matches any predicate with the same number of arguments as it.
  """
  def __str__(self):
    if self.args:
      return "var<{}({})>".format(
        self.name,
        ', '.join(str(a) for a in self.args)
      )
    else:
      return "var<{}>".format(self.name)

  def __hash__(self):
    return 7 * hash(super())

  def matches(self, other):
    return len(self.args) == len(other.args)

class PatternVariable(Variable):
  """
  A PatternVariable is like a normal variable but to match a predicate it must
  also satisfy a regular expression match.
  """
  def __init__(self, name, pattern, *args):
    super().__init__(name, *args)
    self.re = re.compile(pattern)

  def __str__(self):
    if self.args:
      return "cvar<{}:'{}'({})>".format(
        self.name,
        self.re.pattern,
        ', '.join(str(a) for a in self.args)
      )
    else:
      return "cvar<{}:'{}'>".format(self.name, self.re.pattern)

  def __hash__(self):
    return 27 * (
      17 + hash(self.re.pattern)
    ) + hash(super())

  def __eq__(self, other):
    return type(self) == type(other) \
        and self.name == other.name \
        and self.re.pattern == other.re.pattern \
        and self.args == other.args

  def matches(self, other):
    return len(self.args) == len(other.args) \
        and self.re.match(other.name)

class Subtree(Variable):
  """
  A subtree is like a normal variable, but it matches an entire subtree with a
  single node, no matter what structure that subtree has. For this reason,
  Subtrees can't have arguments, unlike the other variable types.
  """
  def __init__(self, name):
    super().__init__(name)

  def __str__(self):
    return "subtree<{}>".format(self.name)

  def __hash__(self):
    return 51 * (
      11 + hash(self.name)
    )

  def __eq__(self, other):
    return type(self) == type(other) and self.name == other.name

  def matches(self, other):
    return True

# How to represent arbitrary values as predicates

def as_predicate(value):
  if type(value) == int:
    return Predicate(value)
  elif type(value) == Predicate:
    return value
  else:
    return Predicate(quote(str(value)))

def build_schema(predicate):
  """
  Takes the given pure-Predicate structure and converts all predicates whose
  names start with uppercase letters into simple Variable objects, thus
  producing a schema.
  """
  for i, arg in enumerate(predicate.args):
    predicate.args[i] = build_schema(arg)
  if type(predicate.name) == str \
  and predicate.name[0] in string.ascii_uppercase:
    return Variable(predicate.name, *predicate.args)
  else:
    return predicate

def bind(schema, predicate, prefix=""):
  """
  Takes a schema (see build_schema for a convenient way to produce one) and a
  predicate and tries to bind each Variable in the given schema to part of the
  given Predicate. If it succeeds, it returns a binding dictionary which maps
  variable names (using a dotted format to specify them absolutely relative to
  the root of the schema) to Predicate objects. So for example if we tried to
  bind:

    Unknown(foo, bar, Var(3, vwe(Baz)))

  against:

    xyzzy(foo, bar, twee(3, vwe(4)))

  the resulting binding dictionary would look like this:

    {
      "Unknown": xyzzy(foo, bar, twee(3, vwe(4))),
      "Unknown.Var": twee(3, vwe(4)),
      "Unknown.Var.vwe.Baz": 4,
    }

  keeping in mind that the above unquoted text represents Predicate objects. If
  the binding fails it returns None. Note that the prefix argument can be used
  to add a prefix to each binding in the result.
  """
  if not schema.matches(predicate):
    return None

  if prefix:
    prefix = prefix + '.' + str(schema.name)
  else:
    prefix = str(schema.name)
  result = {}

  if isinstance(schema, Variable):
    result[prefix] = predicate
    if isinstance(schema, Subtree):
      # Don't recurse further in this case...
      return result

  for i in range(len(schema.args)):
    b = bind(schema.args[i], predicate.args[i], prefix)
    if b == None:
      return None
    result.update(b)

  return result

def bindings(schemas, predicates):
  """
  Takes a dictionary of schemas (each indexed by a string) and an iterable of
  predicates and generates all successful bindings of schemas in the dictionary
  to predicates in the iterable. The schemas are matched against each predicate
  in sort order of their keys, and more than one binding per predicate may be
  generated. Empty (but not failed) bindings may be generated for schemas with
  no variables in them. Each value yielded is a tuple containing the key for
  the schema that generated it and then the binding result (see the bind
  function above).
  """
  skeys = sorted(schemas.keys())
  for p in predicates:
    for k in skeys:
      b = bind(schemas[k], p)
      if b != None:
        yield (k, b)

def filter(predicates, require=[], forbid=[]):
  """
  Filters the given predicate set according to the given require and/or forbid
  lists. Each predicate which matches all schemas in the require list (if any
  are given) and which matches none of the filters in the forbid list is kept,
  all other predicates are dropped.
  """
  for p in predicates:
    if all(bind(schema, p) != None for schema in require) \
    and all(bind(schema, p) == None for schema in forbid):
      yield p

def complex_term(*args, salt_cellar = [37]):
  salt_cellar[0] += 6
  if not args:
    raise ValueError("Tried to create complex term class without any contents.")
  def decorate(cls):
    nonlocal salt_cellar, args
    init_args = ', '.join("{}=None".format(a) for a in args)
    init_body = '\n  '.join("self.{var} = {var}".format(var=a) for a in args)
    hash_expr = '{} + hash(self.{})'.format(salt_cellar[0], args[0])
    for a in args[1:]:
      hash_expr = '47 * (' + hash_expr + ') + hash(self.{}) '.format(a)
    eq_expr = ' and '.join(
      "(self.{var} == other.{var})".format(var=a) for a in args
    )
    code = """
def __init__(self, {init_args}):
  {init_body}

def __hash__(self):
  return {hash_expr}

def __eq__(self, other):
  return type(self) == type(other) and {eq_expr}

def __ne__(self, other):
  return not self == other
""".format(
  init_args=init_args,
  init_body=init_body,
  hash_expr=hash_expr,
  eq_expr=eq_expr,
)
    exec(code, locals(), globals())
    cls.__init__ = __init__
    cls.__hash__ = __hash__
    cls.__eq__ = __eq__
    cls.__ne__ = __ne__
    return cls
  return decorate

# Class definitions for answer set elements:
# Based almost entirely on the ASP-CORE language specification (and probably
# missing some gringo-specific constructions).
# https://www.mat.unical.it/aspcomp2013/files/ASP-CORE-2.03b.pdf

@complex_term("negated", "op", "lhs", "rhs")
class Expression:
  def __str__(self):
    if self.rhs:
      return "{}({} {} {})".format(
        '-' if self.negated else '',
        self.lhs,
        self.op,
        self.rhs
      )
    elif self.op:
      return "{}{}({})".format(
        '-' if self.negated else '',
        self.op,
        self.lhs
      )
    else:
      return "{}({})".format(
        '-' if self.negated else '',
        self.lhs
      )

@complex_term("id", "terms")
class SimpleTerm:
  def __str__(self):
    if self.terms:
      return "{}({})".format(
        self.id,
        ', '.join(str(t) for t in self.terms)
      )
    else:
      return "{}".format(self.id)

@complex_term("negated", "id", "terms")
class ClassicalLiteral:
  def __str__(self):
    if self.terms:
      return "{}{}({})".format(
        '-' if self.negated else '',
        self.id,
        ', '.join(str(t) for t in self.terms)
      )
    else:
      return "{}{}".format(
        '-' if self.negated else '',
        self.id
      )

@complex_term("op", "lhs", "rhs")
class BuiltinAtom:
  def __str__(self):
    return "{} {} {}".format(self.lhs, self.op, self.rhs)

@complex_term("negated", "contents")
class NafLiteral:
  def __str__(self):
    if self.negated:
      return "not {}".format(self.contents)
    else:
      return str(self.contents)

@complex_term("weight", "level", "terms")
class WeightAtLevel:
  def __str__(self):
    return "{}@{}, {}".format(
      self.weight,
      self.level,
      ', '.join(str(t) for t in self.terms)
    )

@complex_term("terms", "constraints")
class AggregateElement:
  def __str__(self):
    if self.constraints:
      return "{} : {}".format(
        ', '.join(str(t) for t in self.terms),
        ', '.join(str(c) for c in self.constraints),
      )
    else:
      return ', '.join(str(t) for t in self.terms)

@complex_term("l", "lop", "function", "elements", "uop", "u")
class Aggregate:
  def __str__(self):
    return "{l}{lop}{function} {{ {elements} }}{uop}{u}".format(
      l=(self.l and (str(self.l) + ' ')) or '',
      lop=(self.lop and (str(self.lop) + ' ')) or '',
      function=self.function,
      elements='; '.join(str(e) for e in self.elements),
      uop=(self.uop and (str(self.uop) + ' ')) or '',
      u=(self.u and (' ' + str(self.u))) or '',
    )

@complex_term("literal", "constraints")
class ChoiceElement:
  def __str__(self):
    if self.constraints:
      return "{} : {}".format(
        self.literal,
        ', '.join(str(c) for c in self.constraints)
      )
    else:
      return str(self.literal)

@complex_term("l", "lop", "elements", "uop", "u")
class Choice:
  def __str__(self):
    return "{l}{lop}{{ {elements} }}{uop}{u}".format(
      l=(self.l and (str(self.l) + ' ')) or '',
      lop=(self.lop and (str(self.lop) + ' ')) or '',
      elements='; '.join(str(e) for e in (self.elements or [])),
      uop=(self.uop and (str(self.uop) + ' ')) or '',
      u=(self.u and (str(self.u) + ' ')) or '',
    )

@complex_term("weightlevel", "literals")
class OptimizeElement:
  def __str__(self):
    if self.literals:
      return "{} : {}".format(
        self.weightlevel,
        ', '.join(str(l) for l in self.literals)
      )
    else:
      return str(self.weightlevel)

@complex_term("function", "elements")
class Optimization:
  def __str__(self):
    return "{} {{ {} }}.".format(
      self.function,
      '; '.join(str(e) for e in (self.elements or []))
    )

@complex_term("body", "weightlevel")
class WeakConstraint:
  def __str__(self):
    return ":~ {}. [ {}@{}, {} ]".format(
      ', '.join(str(l) for l in self.body),
      self.weight,
      self.level,
      ', '.join(str(t) for t in self.terms)
    )

@complex_term("head", "body")
class Rule:
  def __str__(self):
    if self.head:
      if self.body:
        return "{} :- {}.".format(self.head, ', '.join(self.body))
      else:
        return "{}.".format(self.head)
    else:
      return ":- {}.".format(', '.join(self.body))

@complex_term("literal")
class Query:
  def __str__(self):
    return "{}?".format(self.literal)

@complex_term("directive", "contents")
class Directive:
  def __str__(self):
    return "{} {}".format(self.directive, self.contents)

@complex_term("text")
class Comment:
  def __str__(self):
    return "%* {} *%".format(self.text)

@complex_term("statements", "query")
class Program:
  def __str__(self):
    return "{}\n{}".format(
      '\n'.join(str(s) for s in self.statements),
      self.query
    )

# Grammar definitions for answer set elements:

ArithOp = [
  Tokens.OP_PLUS,
  Tokens.OP_MINUS,
  Tokens.OP_TIMES,
  Tokens.OP_DIV,
  Tokens.OP_MOD,
  Tokens.OP_POW,
  Tokens.OP_BITWISE_AND,
  Tokens.OP_BITWISE_OR,
  Tokens.OP_BITWISE_XOR,
  Tokens.OP_BITWISE_NEG,
]

Comparator = [
  Tokens.CMP_EQ,
  Tokens.CMP_NEQ,
  Tokens.CMP_LT,
  Tokens.CMP_GT,
  Tokens.CMP_LE,
  Tokens.CMP_GE,
]

Term = [
  Expression,
  SimpleTerm,
]

Term.append( #'cause this one is directly recursive
  (
    Tokens.ignore.PAREN_OPEN,
    Term,
    Tokens.ignore.PAREN_CLOSE,
  )
)

Terms = peg.csl(Term, separator=Tokens.ignore.COMMA)

SimpleTerm.grammar = [
  (
    peg.attr("id", Tokens.ID),
    peg.optional(
      Tokens.ignore.PAREN_OPEN,
      peg.optional(
        peg.attr("terms", Terms),
      ),
      Tokens.ignore.PAREN_CLOSE,
    )
  ),
  peg.attr("id", Tokens.NUMBER),
  peg.attr("id", Tokens.STRING),
  peg.attr("id", Tokens.VARIABLE),
  peg.attr("id", Tokens.ANONYMOUS),
]

Expression.grammar = (
  peg.flag("negated", Tokens.OP_MINUS),
  [
    (
      peg.attr(
        "lhs",
        (
          Tokens.ignore.PAREN_OPEN,
          Term,
          Tokens.ignore.PAREN_CLOSE,
        )
      ),
      peg.attr("op", ArithOp),
      peg.attr("rhs", Term)
    ),
    (
      peg.attr("lhs", SimpleTerm),
      peg.attr("op", ArithOp),
      peg.attr("rhs", Term)
    ),
    peg.attr(
      "lhs",
      SimpleTerm,
    ),
  ]
)

ClassicalLiteral.grammar = (
  peg.flag("negated", Tokens.OP_MINUS),
  peg.attr("id", Tokens.ID),
  peg.optional(
    Tokens.ignore.PAREN_OPEN,
    peg.optional(
      peg.attr("terms", Terms),
    ),
    Tokens.ignore.PAREN_CLOSE,
  )
)

BuiltinAtom.grammar = (
  peg.attr("lhs", Term),
  peg.attr("op", Comparator),
  peg.attr("rhs", Term),
)

NafLiteral.grammar = (
  peg.flag("negated", Tokens.NAF),
  peg.attr(
    "contents",
    [
      ClassicalLiteral,
      BuiltinAtom
    ]
  )
)

WeightAtLevel.grammar = (
  peg.attr("weight", Term),
  peg.optional(
    (
      Tokens.ignore.AT,
      peg.attr("level", Term),
    )
  ),
  peg.optional(
    (
      Tokens.ignore.COMMA,
      peg.attr("terms", Terms)
    )
  )
)

AggregateElement.grammar = (
  peg.attr("terms", Terms),
  peg.optional(
    Tokens.ignore.COLON,
    peg.attr(
      "constraints",
      peg.csl(NafLiteral, separator=Tokens.ignore.COMMA)
    )
  )
)

Aggregate.grammar = (
  peg.flag("negated", Tokens.NAF),
  peg.optional(
    (
      peg.attr("l", Term),
      peg.attr("lop", Comparator),
    )
  ),
  peg.attr(
    "function",
    [
      Tokens.KW_AGGREGATE_COUNT,
      Tokens.KW_AGGREGATE_MAX,
      Tokens.KW_AGGREGATE_MIN,
      Tokens.KW_AGGREGATE_SUM,
    ]
  ),
  Tokens.ignore.CURLY_OPEN,
  peg.optional(
    peg.attr(
      "elements",
      peg.csl(AggregateElement, separator=Tokens.ignore.SEMICOLON),
    )
  ),
  Tokens.ignore.CURLY_CLOSE,
  peg.optional(
    (
      peg.attr("uop", Comparator),
      peg.attr("u", Term),
    )
  ),
)

ChoiceElement.grammar = (
  peg.attr("literal", ClassicalLiteral),
  peg.optional(
    Tokens.ignore.COLON,
    peg.attr(
      "constraints",
      peg.csl(NafLiteral, separator=Tokens.ignore.COMMA)
    )
  )
)

Choice.grammar = (
  peg.optional(
    (
      peg.attr("l", Term),
      peg.attr("lop", Comparator),
    )
  ),
  Tokens.ignore.CURLY_OPEN,
  peg.optional(
    peg.attr(
      "elements",
      peg.csl(ChoiceElement, separator=Tokens.ignore.SEMICOLON)
    )
  ),
  Tokens.ignore.CURLY_CLOSE,
  peg.optional(
    (
      peg.attr("uop", Comparator),
      peg.attr("u", Term),
    )
  ),
)

Disjunction = peg.csl(ClassicalLiteral, separator=Tokens.ignore.OR)

Head = [
  Disjunction,
  Choice
]

Body = peg.csl(
  [
    NafLiteral,
    Aggregate
  ],
  separator=Tokens.ignore.COMMA
)

OptimizeElement.grammar = (
  peg.attr("weightlevel", WeightAtLevel),
  peg.optional(
    (
      Tokens.ignore.COLON,
      peg.attr("literals", peg.csl( NafLiteral, separator=Tokens.ignore.COMMA ))
    )
  )
)

OptimizeFunction = [ Tokens.KW_MAXIMIZE, Tokens.KW_MINIMIZE ]

Optimization.grammar = [
  (
    peg.attr("function", OptimizeFunction),
    Tokens.ignore.CURLY_OPEN,
    peg.attr(
      "elements",
      peg.csl(OptimizeElement, separator=Tokens.ignore.SEMICOLON)
    ),
    Tokens.ignore.CURLY_CLOSE,
    Tokens.ignore.DOT,
  ),
]

WeakConstraint.grammar = [
  (
    Tokens.ignore.WCONS,
    peg.optional(
      peg.attr("body", Body),
    ),
    Tokens.ignore.DOT,
    Tokens.ignore.SQUARE_OPEN,
    peg.attr("weightlevel", WeightAtLevel),
    Tokens.ignore.SQUARE_CLOSE,
  ),
]

Rule.grammar = [
  (
    Tokens.ignore.CONS,
    peg.attr("body", Body),
    Tokens.ignore.DOT
  ),
  (
    peg.attr("head", Head),
    peg.optional(
      (
        Tokens.ignore.CONS,
        peg.attr("body", Body),
      )
    ),
    Tokens.ignore.DOT
  ),
]

Directive.grammar = (
  peg.attr(
    "directive",
    [
      Tokens.DIR_HIDE,
      Tokens.DIR_SHOW,
      Tokens.DIR_CONST,
      Tokens.DIR_DOMAIN,
      Tokens.DIR_EXTERNAL,
    ]
  ),
  peg.attr(
    "contents",
    Tokens.DIRECTIVE_BODY
  ),
  Tokens.ignore.DOT
)

Statement = [
  Rule,
  WeakConstraint,
  Optimization,
  Directive,
]

Query.grammar = (
  peg.attr("literal", ClassicalLiteral),
  Tokens.ignore.QUERY_MARK
)

Comment.grammar = None

Program.grammar = (
  peg.attr("statements", peg.maybe_some( Statement )),
  peg.optional( peg.attr("query", Query) ),
)

# shortcuts:
Pr = Predicate
Vr = Variable
PVr = PatternVariable
SbT = Subtree

_test_cases = [
  (
    str,
    Pr(
      "p",
      Pr(
        "p",
        Pr(2)
      ),
      Pr("v",
        Pr(3),
        Pr(4),
        Pr("s")
      ),
      Pr("z"),
      Pr('"myeh myeh \\"jyeh, )"')
    ),
    'p(p(2), v(3, 4, s), z, "myeh myeh \\"jyeh, )")',
  ),
  (
    str,
    Pr(
      "value",
      Pr(5),
      Pr(6),
      Pr(9),
      Pr(
        "tr",
        Pr(2),
        Pr(-1),
        Pr(
          "tr",
          Pr(6),
          Pr(1),
          Pr(
            "tr",
            Pr(1),
            Pr(1),
            Pr(
              "tr",
              Pr(3),
              Pr(-1),
              Pr(
                "tr",
                Pr(3),
                Pr(1),
                Pr("none")
              )
            )
          )
        )
      )
    ),
    "value(5, 6, 9, tr(2, -1, tr(6, 1, tr(1, 1, tr(3, -1, tr(3, 1, none))))))",
  ),
  (
    build_schema,
    Pr(
      "foo",
      Pr("Bar"),
      Pr(
        "Baz",
        Pr(3),
        Pr("hoho"),
        Pr("Jig")
      )
    ),
    Pr(
      "foo",
      Vr("Bar"),
      Vr(
        "Baz",
        Pr(3),
        Pr("hoho"),
        Vr("Jig")
      )
    )
  ),
  (
    bind,
    (
      Pr(
        "foo",
        Vr("Bar"),
        Vr(
          "Baz",
          Pr(3),
          Pr("hoho"),
          Vr("Jig")
        )
      ),
      Pr(
        "foo",
        Pr("gah"),
        Pr(
          "ehhh",
          Pr(3),
          Pr("hoho"),
          Pr("lee")
        )
      )
    ),
    {
      "foo.Bar": Pr("gah"),
      "foo.Baz": Pr(
        "ehhh",
        Pr(3),
        Pr("hoho"),
        Pr("lee")
      ),
      "foo.Baz.Jig": Pr("lee")
    }
  ),
  # TODO: add more tests for binding with pattern variables and subtrees.
  (
    lambda text: pypeg2.parse(text, Term),
    "foo(bar, 5)",
    SimpleTerm(
      "foo",
      [
        SimpleTerm("bar"),
        SimpleTerm("5")
      ]
    ),
  ),
  (
    lambda text: pypeg2.parse(text, Term),
    "foo(Var(bar), baz(X, X+1)) / bar(Zed)",
    Expression(
      False,
      "/",
      SimpleTerm(
        "foo",
        [
          SimpleTerm( "Var", [ SimpleTerm("bar") ]),
          SimpleTerm(
            "baz",
            [
              SimpleTerm("X"),
              Expression(
                False,
                "+",
                SimpleTerm("X"),
                SimpleTerm("1"),
              )
            ]
          ),
        ]
      ),
      SimpleTerm( "bar", [ SimpleTerm("Zed") ])
    ),
  ),
  (
    lambda text: pypeg2.parse(text, Term),
    "-(x + 3) / 5 - 4", # Note awful operator binding x2
    Expression(
      True,
      "/",
      Expression(
        False,
        "+",
        SimpleTerm("x"),
        SimpleTerm("3"),
      ),
      Expression(
        False,
        "-",
        SimpleTerm("5"),
        SimpleTerm("4"),
      )
    ),
  ),
  (
    lambda text: str(pypeg2.parse(text, Term)),
    "-(x + 3) / 5 - 4 * 3 - 1",
    "-((x + 3) / (5 - (4 * (3 - 1))))"
  ),
  (
    lambda text: pypeg2.parse(text, Term),
    "X * Y - -Z", # Note again bad operator binding
    Expression(
      False,
      "*",
      SimpleTerm("X"),
      Expression(
        False,
        "-",
        SimpleTerm("Y"),
        Expression(
          True,
          None,
          SimpleTerm("Z")
        )
      )
    )
  ),
  (
    lambda text: str(pypeg2.parse(text, Term)),
    "X * Y - -Z",
    "(X * (Y - -(Z)))"
  ),
  (
    lambda text: pypeg2.parse(text, Statement),
    "foo(bar, baz).",
    Rule(
      [
        ClassicalLiteral(
          False,
          "foo",
          [
            SimpleTerm("bar"),
            SimpleTerm("baz"),
          ]
        )
      ],
      None
    ),
  ),
  (
    lambda text: str(pypeg2.parse(text, Statement)),
    "foo(bar, baz).",
    "foo(bar, baz).",
  ),
  (
    lambda text: pypeg2.parse(text, Statement),
    "foo(bar, baz) | -xyzzy :- a, not b.",
    Rule(
      [
        ClassicalLiteral(
          False,
          "foo",
          [
            SimpleTerm("bar"),
            SimpleTerm("baz"),
          ]
        ),
        ClassicalLiteral(
          True,
          "xyzzy"
        )
      ],
      [
        NafLiteral(
          False,
          ClassicalLiteral(
            False,
            "a"
          )
        ),
        NafLiteral(
          True,
          ClassicalLiteral(
            False,
            "b"
          )
        )
      ]
    ),
  ),
  (
    lambda text: str(pypeg2.parse(text, Statement)),
    "foo(bar, baz) | -xyzzy :- a, not b.",
    "foo(bar, baz) | -xyzzy :- a, not b.",
  ),
  (
    lambda text: pypeg2.parse(text, Statement),
    """\
1 <= { x(T) : not T < 3 ; y(T) } :-
  -3 < #count {
    -X, Y+5, foobar(X) : X = 3;
    other;
    a, b : c, not d
  } < 7,
  other.""",
    Rule(
      Choice(
        SimpleTerm("1"),
        "<=",
        [
          ChoiceElement(
            ClassicalLiteral(
              False,
              "x",
              [
                SimpleTerm("T")
              ]
            ),
            [
              NafLiteral(
                True,
                BuiltinAtom(
                  "<",
                  SimpleTerm("T"),
                  SimpleTerm("3")
                )
              )
            ]
          ),
          ChoiceElement(
            ClassicalLiteral(
              False,
              "y",
              [
                SimpleTerm("T")
              ]
            )
          )
        ],
      ),
      [ # rule-body
        Aggregate(
          False,
          Expression(
            True,
            None,
            SimpleTerm("3")
          ),
          "<",
          "#count",
          [
            AggregateElement(
              [
                Expression(True, None, SimpleTerm("X")),
                Expression(False, "+", SimpleTerm("Y"), SimpleTerm("5")),
                SimpleTerm("foobar", [ SimpleTerm("X") ]),
              ],
              [
                NafLiteral(
                  False,
                  BuiltinAtom(
                    "=",
                    SimpleTerm("X"),
                    SimpleTerm("3")
                  )
                )
              ]
            ),
            AggregateElement( [ SimpleTerm("other"), ] ),
            AggregateElement(
              [
                SimpleTerm("a"),
                SimpleTerm("b"),
              ],
              [
                NafLiteral(False, ClassicalLiteral(False, "c")),
                NafLiteral(True, ClassicalLiteral(False, "d")),
              ]
            ),
          ],
          "<",
          SimpleTerm("7")
        ),
        NafLiteral(False, ClassicalLiteral(False, "other"))
      ]   
    ),
  ),
  (
    lambda text: str(pypeg2.parse(text, Statement)),
    """\
1 <= { x(T) : not T < 3 ; y(T) } :-
  -3 < #count {
    -X, Y+5, foobar(X) : X = 3;
    other;
    a, b : c, not d
  } < 7,
  other.""",
    "1 <= { x(T) : not T < 3 ; y(T) } :- " +\
    "-(3) < #count { -(X), (Y+5), foobar(X) : X = 3; " +\
    "other; a, b : c, not d} < 7, other.",
  ),
  (
    lambda text: pypeg2.parse(text, Program),
    """\
a.
b.
q :- a, b.
    """,
    Program(
      [
        Rule( [ ClassicalLiteral(False, "a") ]),
        Rule( [ ClassicalLiteral(False, "b") ]),
        Rule(
          [ ClassicalLiteral(False, "q") ],
          [
            NafLiteral(False, ClassicalLiteral(False, "a")),
            NafLiteral(False, ClassicalLiteral(False, "b")),
          ]
        )
      ]
    ),
  ),
  (
    lambda text: str(pypeg2.parse(text, Program)),
    """\
a.
b.
q :- a, b.
    """,
    """\
a.
b.
q :- a, b.
""",
  ),
  (
    lambda text: pypeg2.parse(text, Program),
    """\
foo(3, 4).
bar(X, Y+1) :- foo(X, Y), not bar(X, Y-1).
bar(3, 5)?
    """,
    Program(
      [
        Rule(
          [
            ClassicalLiteral(
              False,
              "foo",
              [ SimpleTerm("3"), SimpleTerm("4") ]
            )
          ]
        ),
        Rule(
          [
            ClassicalLiteral(
              False,
              "bar",
              [
                SimpleTerm("X"),
                Expression(
                  False,
                  "+",
                  SimpleTerm("Y"),
                  SimpleTerm("1")
                )
              ]
            )
          ],
          [
            NafLiteral(
              False,
              ClassicalLiteral(
                False,
                "foo",
                [ SimpleTerm("X"), SimpleTerm("Y") ]
              )
            ),
            NafLiteral(
              True,
              ClassicalLiteral(
                False,
                "bar",
                [
                  SimpleTerm("X"),
                  Expression(
                    False,
                    "-",
                    SimpleTerm("Y"),
                    SimpleTerm("1")
                  )
                ]
              )
            )
          ]
        )
      ],
      Query(
        ClassicalLiteral(
          False,
          "bar",
          [
            SimpleTerm("3"),
            SimpleTerm("5"),
          ]
        )
      )
    ),
  ),
  (
    lambda text: str(pypeg2.parse(text, Program)),
    """\
foo(3, 4).
bar(X, Y+1) :- foo(X, Y), not bar(X, Y-1).
bar(3, 5)?
    """,
    """\
foo(3, 4).
bar(X, (Y+1)) :- foo(X, Y), not bar(X, (Y-1)).
bar(3, 5)?""",
  ),
]
