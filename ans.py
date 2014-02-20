"""
ans.py
Answer set data structures.
"""

from utils import *

import re

import string

class Predicate:
  """
  A Predicate represents a predicate structure, something like:

    foo(bar, baz(xyzzy))

  Each Predicate object has a name and 0 or more arguments. Generally, unless
  the name begins and ends with a '"' character, it should only contain
  characters from the class [a-zA-Z0-9_] and its first character should be in
  [a-z]. This is not a strict rule however.
  """
  def __init__(self, name, *args):
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
    init_args = ', '.join(args)
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

@complex_term("op", "arg")
class UnOp:
  def __str__(self):
    return "{}{}".format(self.op, self.arg)

@complex_term("op", "left", "right")
class BinOp:
  def __str__(self):
    return "{} {} {}".format(self.left, self.op, self.right)

@complex_term("items", "sep")
class List:
  def __str__(self):
    return (self.sep + ' ').join(str(it) for it in self.items)

@complex_term("open", "expr")
class Group:
  close = {
    '|': '|',
    '(': ')',
    '[': ']',
    '{': '}',
    '': '',
  }
  def __str__(self):
    return "{o}{expr}{c}".format(
      o=self.open,
      c=self.close[self.open]
      expr=self.expr,
    )

@complex_term("cmpr", "left", "right")
class Comparison:
  def __str__(self):
    return "{} {} {}".format(self.left, self.cmpr, self.right)

@complex_term("asgn", "left", "right")
class Assignment:
  def __str__(self):
    return "{} {} {}".format(self.left, self.asgn, self.right)

@complex_term("min", "max")
class Interval:
  def __str__(self):
    return "{} .. {}".format(self.min, self.max)

@complex_term("subject", "filter")
class Condition:
  def __str__(self):
    return "{} : {}".format(self.subject, self.filter)

# Note: a Pool is just a List with ';' as its separator.

@complex_term("literal", "weight")
class WeightedLiteral:
  def __str__(self):
    if self.weight == 1:
      return str(self.literal)
    else:
      return "{}={}".format(self.literal, self.weight)

@complex_term("weight", "priority")
class PrioritizedWeight:
  def __str__(self):
    return "{}@{}".format(self.weight, self.priority)

@complex_term("op", "lower", "upper", "group")
class Aggregate:
  def __str__(self):
    return "{l} {op} {group} {u}".format(
      l=self.lower,
      u=self.upper,
      op=self.op,
      group=group,
    )

@complex_term("opt", "group")
class Optimization:
  def __str__(self):
    return "{opt} {group} {u}".format(
      opt=self.opt,
      group=group,
    )

@complex_term("dir", "content")
class Directive:
  def __str__(self):
    return "{} {}.".format(self.dir, self.content)

@complex_term("text")
class Comment:
  def __str__(self):
    return "%* {} *%".format(self.text)


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
                Pr(
                  "tr",
                  Pr(1),
                  Pr(1),
                  Pr("none")
                )
              )
            )
          )
        )
      )
    ),
    "value(5, 6, 9, tr(2, -1, tr(6, 1, tr(1, 1, tr(3, -1, tr(3, 1, tr(1, 1, none)))))))",
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
]
