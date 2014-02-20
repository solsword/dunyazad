'''
parser.py
A parser for predicates and answer set rules.
'''

import re

from utils import *

from ans import *

class ParseError(Exception):
  pass

token_types = {
  "period": re.compile(r"\.((?=[^.])|$)"),
    # Matches a period NOT followed by another, but doesn't eat the following
    # character.
  "comma": re.compile(r","),
  "semicolon": re.compile(r";"),
  "qualification": re.compile(r":((?=[^-])|$)"),
    # Matches a colon NOT followed by a dash, but doesn't eat the following
    # character.
  "assignment": re.compile(r":?=((?=[^=])|$)"),
    # Matches an equals followed by a non-equals character optionally preceded
    # by a colon.
  "cmp_eq": re.compile(r"=="),
  "cmp_neq": re.compile(r"!="),
  "cmp_gt": re.compile(r">((?=[^=])|$)"),
  "cmp_lt": re.compile(r"<((?=[^=])|$)"),
  "cmp_ge": re.compile(r">="),
  "cmp_le": re.compile(r"<="),
  "op_plus": re.compile(r"\+"),
  "op_minus": re.compile(r"-"),
  "op_times": re.compile(r"\*((?=[^*])|$)"),
  "op_div": re.compile(r"/"),
  "op_mod": re.compile(r"\\"),
  "op_abs": re.compile(r"\|"),
  "op_pow": re.compile(r"\*\*"),
  "op_and": re.compile(r"&"),
  "op_or": re.compile(r"\?"),
  "op_xor": re.compile(r"\^"),
  "op_not": re.compile(r"~"),
  "interval": re.compile(r"\.\."),
  "neck": re.compile(r":-"),
  "anonymous": re.compile(r"_((?=[^a-zA-Z0-9_])|$)"),
    # Matches an underscore NOT followed any further word characters, but
    # doesn't eat the following character.
  "integer": re.compile(r"[0-9]+"),
  "constant": re.compile(r"_*[a-z][a-zA-Z0-9_]*"),
    # Matches some number of optional starting underscores, a first-position
    # lowercase alphabetic character, and then any number of alphanumerics
    # and/or underscores.
  "quoted": re.compile(r'"([^\\"]|(\\.))*"'),
    # Matches a starting quote, followed by any number of tokens which are
    # either non-backslash, non-quote characters or which are escape codes ('\'
    # plus any character), finished by an ending quote.
  "arguments_open": re.compile(r"\("),
  "arguments_close": re.compile(r"\)"),
  "set_open": re.compile(r"\{"),
  "set_close": re.compile(r"\}"),
  "multiset_open": re.compile(r"\["),
  "multiset_close": re.compile(r"\]"),
  "keyword": re.compile(r"#[a-z]+"),
  "kw_not": re.compile(r"not"),
  "whitespace": re.compile(r"\s"),
}

# Tokens ordered according to estimated frequency.
ordered_tokens = [
  "whitespace",
  "constant",
  "integer",
  "period",
  "comma",
  "arguments_open",
  "arguments_close",
  "quoted",
  "semicolon",
  "anonymous",
  "interval",
  "neck",
  "set_open",
  "set_close",
  "multiset_open",
  "multiset_close",
  "op_plus",
  "op_minus",
  "qualification",
  "op_times",
  "op_div",
  "op_mod",
  "op_abs",
  "op_pow",
  "op_and",
  "op_or",
  "op_xor",
  "op_not",
  "cmp_eq",
  "cmp_neq",
  "cmp_gt",
  "cmp_lt",
  "cmp_ge",
  "cmp_le",
  "kw_not",
  "assignment",
  "keyword",
]

def tokenize(text):
  '''
  Splits the given text up into tokens as defined by the regular expressions
  given in the 'tokens' dict.
  '''
  tokens = []
  while text:
    matched = False
    for t in ordered_tokens:
      m = token_types[t].match(text)
      if m:
        text = text[m.end():]
        if t != "whitespace":
          tokens.append((t, m.group()))
        matched = True
        break
    if not matched:
      raise ParseError("No token matches text starting '{}'.".format(text[:15]))
  return tokens


# Parser builders:

def disjunction_parser(*args):
  '''
  The following parse_* functions all return either an object paired with a
  token tail or None along with the entire token list given to them. This
  function returns a parser which tries the parsing functions given to it in
  order until one succeeds or they all fail, returning the first successful
  result. Note that in the case of ambiguous parsers, order is very important.

  If any of the given arguments are token strings instead of functions, they
  match that token and return the token string.
  '''
  def parse_disjunction(tokens):
    nonlocal args
    for a in args:
      if a in token_types:
        if tokens[0][0] == a:
          result, tail = tokens[0][1], tokens[1:]
        else:
          continue
      else:
        result, tail = a(tokens)

      if result:
        return result, tail

    return None, tokens
  return parse_disjunction

def chain_parser(*args):
  '''
  Works like disjunction_parser, but requires that each parser given matches
  one after the other in sequence. This parser that this function builds
  returns a list containing the results of each parser in the chain along with
  the remaining tokens, or if any of the parsers given fail, it returns None
  and the original tokens list.

  If any of the given arguments are token strings instead of functions, they
  match that token and return the token string.
  '''
  def parse_chain(tokens):
    nonlocal args
    tail = tokens
    results = []
    for a in args:
      if a in token_types:
        if tail[0][0] == a:
          result, tail = tail[0][1], tail[1:]
        else:
          return None, tokens
      else:
        result, tail = a(tail)

      if not result:
        return None, tokens
      results.append(result)
    return results, tail
  return parse_chain

def group_parser(member_parser, sep='comma', gropen=None, grclose=None):
  '''
  Constructs and returns a parser for a group with the given open and close
  token labels, which has sep-separated members that are parsable by the given
  member parser. If gropen/grclose are not given, then no token is required at
  the start/end of the list. The result of the parser is a Group object along
  with the tokens tail. Of course, None and the original token list will be
  returned if the parsing fails at some point.

  Note that sep, gropen, and grclose should be given as token names, but both
  sep and gropen must also fall into the categories defined below so that their
  respective string forms can be used in returned Group objects. grclose does
  not have this restriction as the string form of gropen automatically sets the
  closing string for the group (see class Group in ans.py).
  '''
  separators = {
    'comma': ',',
    'semicolon': ';',
  }
  openers = {
    None: '',
    'arguments_open': '(',
    'set_open': '{',
    'multiset_open': '[',
  }
  def parse_group(tokens):
    nonlocal member_parser, sep, gropen, grclose, separators, openers
    result = Group(openers[gropen], List([], separators[sep]))
    tail = list(tokens)
    if gropen:
      if tail[0][0] != gropen:
        return None, tokens
      tail[0] = (sep, separators[sep]) # hack for do .. while structure
    else:
      tail = [(sep, separators[sep])] + tail # hack as above
    while tail[0][0] == sep:
      tail = tail[1:]
      p, tail = member_parser(tail)
      if p:
        result.expr.items.append(p)
      else:
        return None, tokens
    if grclose:
      if tail[0][0] != grclose:
        return None, tokens
      return result, tail[1:]
    else:
      return result, tail
  return parse_group

def munged_parser(base, munge):
  '''
  Returns a parser that runs the given base parser and then calls the munge
  function on the result if the result isn't None.
  '''
  def parse_munged(tokens):
    nonlocal base, munge
    result, tail = base(tokens)
    if result != None:
      result = munge(result)
    return result, tail
  return parse_munged

def as_named_predicate_parser(*t_types):
  '''
  Returns a parser that parses any of the given token types as a unary
  predicate with a name equal to the token string.
  '''
  def parse_named_predicate(tokens):
    nonlocal t_types
    if tokens[0][0] in t_types:
      return Predicate(name=tokens[0][1]), tokens[1:]
    return None, tokens
  return parse_named_predicate

def as_string_parser(*t_types):
  '''
  Returns a parser that parses a single token of any of the given types,
  returning the token string directly as a result.
  '''
  def parse_string(tokens):
    nonlocal t_types
    if tokens[0][0] in t_types:
      return tokens[0][1], tokens[1:]
    return None, tokens
  return parse_string


# Low-level parsing functions:

parse_keyword = as_string_parser("keyword")

def parse_integer(tokens):
  t = tokens[0][0]
  s = tokens[0][1]
  if t == "integer":
    return (
      Predicate(name=int(s)),
      tokens[1:]
    )
  elif t == "op_minus":
    if tokens[1][0] == "integer":
      return (
        Predicate(name=int('-' + tokens[1][1])),
        tokens[2:]
      )
    else:
      return None, tokens
  else:
    return None, tokens

parse_anonymous = as_named_predicate_parser("anonymous")
parse_constant = as_named_predicate_parser("constant")
parse_quoted = as_named_predicate_parser("quoted")

parse_integer_or_anonymous = disjunction_parser(parse_integer, parse_anonymous)
parse_constant_or_quoted = as_named_predicate_parser("constant", "quoted")

parse_arg_list = group_parser(
  parse_predicate,
  gropen="arguments_open",
  grclose="arguments_close",
)

def parse_predicate(tokens):
  result, tail = parse_integer_or_anonymous(tokens)
  if result:
    return result, tail
  else:
    result, tail = parse_constant_or_quoted(tokens)
    if tail:
      args, tail = parse_arg_list(tail)
      if args:
        result.args = args.expr.items
    return result, tail

def parse_condition(tokens):
  result = Condition(None, None)

  result.subject, tail = parse_predicate(tokens)

  if (not result.subject) or tail[0][0] != "qualification":
    return None, tokens
  tail = tail[1:]

  result.filter, tail = parse_condition(tail)
  if result.filter:
    return result, tail
  else:
    result.filter, tail = parse_predicate(tail)
    if result.filter:
      return result, tail
    else:
      return None, tokens

parse_interval = munged_parser(
  chain_parser(
    parse_predicate,
    "interval",
    parse_predicate,
  ),
  lambda r: Interval(r[0], r[2])
)

parse_pool = group_parser(
  parse_predicate,
  sep="semicolon"
)

parse_aggregate_group = disjunction_parser(
  group_parser(
    parse_term,
    gropen="set_open",
    grclose="set_close"
  ),
  group_parser(
    parse_term,
    gropen="multiset_open",
    grclose="multiset_close"
  ),
)

def parse_aggregate(tokens):
  lower, tail = parse_expr(tokens)
  if lower == None:
    lower = Predicate(name=0)
  op, tail = parse_keyword(tokens)
  if op == None:
    op = "default"
  group, tail = parse_aggregate_group(tokens)
  if not group:
    return None, tokens
  if op == "default":
    if group.open == '[':
      op = "#sum"
    elif group.open == '{':
      op = "#count"
    else:
      assert(False, "bad aggregate group open symbol")
  upper, tail = parse_expr(tokens)
  if upper == None:
    upper = Predicate(name=1)
  return Aggregate(op, lower, upper, group)

parse_term = disjunction_parser(
  parse_aggregate,
  parse_condition,
  parse_interval,
  parse_pool,
  parse_expr,
  parse_predicate,
)

parse_rule_body = munged_parser(
  chain_parser(
    group_parser(
      parse_term,
    ),
    "period"
  ),
  lambda r: r[0] # should be the Group parsed by the group_parser above.
)

parse_rule_head = disjunction_parser(
  parse_condition,
  parse_predicate,
)

def parse_rule(tokens):
  head, tail = parse_rule_head(tokens)
  if not head:
    return None, tokens
  if tail[0][0] == "period":
    return Rule(head, None), tail[1:]
  elif tail[0][0] == "neck":
    body, tail = parse_rule_body(tail)
    if not body:
      return None, tokens
    return Rule(head, body)
  else:
    return None, tokens

def as_predicates(tokens):
  '''
  Takes a list of tokens (name + text tuples) and parses it as a sequence of
  predicates, generating Predicate objects. If the parsing fails at any point
  it raises a ParseError.
  '''
  while(tokens):
    p, tokens = parse_predicate(tokens)
    if not p:
      raise ParseError(
        "Failed to parse predicate from tokens: '{}'".format(
          ''.join(t[1] for t in tokens)
        )
      )
    if tokens and tokens[0][0] != "period":
      raise ParseError(
        "Extra tokens (expected period): '{}'".format(
          ''.join(t[1] for t in tokens)
        )
      )
    tokens = tokens[1:]
    yield p

def as_program(tokens):
  '''
  Takes a list of tokens (name + text tuples) and parses it as an answer set
  program (a mix of predicates and rules).
  '''
  # TODO!
  return None

def _test_bad_fact(text):
  try:
    list(as_predicates(tokenize(text)))
  except ParseError:
    return True
  return False

_test_cases = [
  (lambda t: as_predicates(tokenize(t)), "test", [Predicate("test")]),
  (lambda t: as_predicates(tokenize(t)), "two_part", [Predicate("two_part")]),
  (
    lambda t: as_predicates(tokenize(t)),
    "with_period.",
    [Predicate("with_period")]
  ),
  (lambda t: as_predicates(tokenize(t)), "_", [Predicate("_")]),
  (
    lambda t: as_predicates(tokenize(t)),
    "p(p(2), v(3, 4, s), z)",
    [
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
        Pr("z")
      )
    ]
  ),
  (lambda t: as_predicates(tokenize(t)), '"quoted"', [Predicate('"quoted"')]),
  (
    lambda t: as_predicates(tokenize(t)),
    '"negative(-3)"',
    [Predicate('"negative(-3)"')]
  ),
  (
    lambda t: as_predicates(tokenize(t)),
    'negative(-3)',
    [Predicate("negative", Predicate(-3))]
  ),
  (
    lambda t: as_predicates(tokenize(t)),
    '"qp"(test, "qp2")',
    [
      Pr(
        '"qp"',
        Pr("test"),
        Pr('"qp2"')
      )
    ]
  ),
  (
    lambda t: as_predicates(tokenize(t)),
    'p(p(2), v(3, 4, s), z, "myeh myeh \\"jyeh, )")',
    [
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
      )
    ]
  ),
  (
    lambda t: as_predicates(tokenize(t)),
    "value(5,6,9,tr(2,-1,tr(6,1,tr(1,1,tr(3,-1,tr(3,1,tr(1,1,none))))))).",
    [
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
      )
    ]
  ),
  (
    lambda t: as_predicates(tokenize(t)),
    'slot(n("P2.AgitatedInsult","S1"),to,empty).',
    [
      Pr(
        "slot",
        Pr(
          "n",
          Pr('"P2.AgitatedInsult"'),
          Pr('"S1"'),
        ),
        Pr("to"),
        Pr("empty"),
      )
    ]
  ),
  (_test_bad_fact, "3(v)", True),
  (_test_bad_fact, "_(1)", True),
]
