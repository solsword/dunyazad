'''
parser.py
A parser for predicates and answer set rules.
'''

import re

from utils import *

from ans import *

token_types = {
  "period": re.compile(r"\.((?=[^.])|$)"),
    # Matches a period NOT followed by another, but doesn't eat the following
    # character.
  "comma": re.compile(r","),
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
  "range": re.compile(r"\.\."),
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
  "anonymous",
  "quoted",
  "range",
  "neck",
  "set_open",
  "set_close",
  "multiset_open",
  "multiset_close",
  "op_plus",
  "op_minus",
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
  "qualification",
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
      err("No token matches text starting '{}'.".format(text[:15]))
      return None
  return tokens

def try_parses(tokens, *args):
  '''
  The following parsing functions all return either a Predicate object paired
  with a token tail or None along with the entire token list given to them.
  This function tries the parsing functions given to it in order until one
  succeeds or they all fail.
  '''
  for a in args:
    result, tail = a(tokens)
    if result:
      return result, tail
  return None, tokens

def parse_integer(tokens):
  '''
  Parses an integer constant
  '''
  try:
    t = tokens[0][0]
    s = tokens[0][1]
  except:
    print("ERR!", tokens)
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

def parse_anonymous(tokens):
  if tokens[0][0] == "anonymous":
    return (
      Predicate(name=tokens[0][1]),
      tokens[1:]
    )
  return None, tokens

def parse_constant(tokens):
  if tokens[0][0] == "constant":
    return (
      Predicate(name=tokens[0][1]),
      tokens[1:]
    )
  return None, tokens

def parse_quoted(tokens):
  if tokens[0][0] == "quoted":
    return (
      Predicate(name=tokens[0][1]),
      tokens[1:]
    )
  return None, tokens

def parse_arg_list(tokens):
  args = []
  tail = list(tokens)
  if tail[0][0] != "arguments_open":
    return None, tokens
  tail[0] = ("comma", ',') # hack for do .. while structure
  while tail[0][0] == "comma":
    tail = tail[1:]
    p, tail = parse_predicate(tail)
    if p:
      args.append(p)
    else:
      return None, tokens
  if tail[0][0] != "arguments_close":
    return None, tokens
  return args, tail[1:]
  

def parse_predicate(tokens):
  '''
  Takes a list of tokens, parses a predicate off the front of them, and returns
  that predicate plus a list of remaining tokens. If it can't do so, it returns
  None and the whole list.
  '''
  result, tail = try_parses(tokens, parse_integer, parse_anonymous)
  if result:
    return result, tail
  else:
    result, tail = try_parses(tokens, parse_constant, parse_quoted)
    if tail:
      args, tail = parse_arg_list(tail)
      if args:
        result.args = args
    return result, tail

def parse_predicates(tokens):
  '''
  Takes a list of tokens (name + text tuples) and parses it as a sequence of
  predicates, returning a list of Predicate objects. If the parsing fails at
  any point it prints an error and returns None.
  '''
  predicates = []
  while(tokens):
    p, tokens = parse_predicate(tokens)
    if not p:
      err(
        "Failed to parse predicate from tokens: '{}'".format(
          ''.join(t[1] for t in tokens)
        )
      )
      return None
    if tokens and tokens[0][0] != "period":
      err(
        "Extra tokens (expected period): '{}'".format(
          ''.join(t[1] for t in tokens)
        )
      )
      return None
    tokens = tokens[1:]
    predicates.append(p)
  return predicates

def parse_facts(text):
  '''
  Takes text and parses it as a list of predicates, tokenizing it first.
  '''
  return parse_predicates(tokenize(text))

def parse_program(tokens):
  '''
  Takes a list of tokens (name + text tuples) and parses it as an answer set
  program (a mix of predicates and rules).
  '''
  # TODO!
  return None


_test_cases = [
  (parse_facts, "test", [Predicate("test")]),
  (parse_facts, "two_part", [Predicate("two_part")]),
  (parse_facts, "with_period.", [Predicate("with_period")]),
  (parse_facts, "_", [Predicate("_")]),
  (
    parse_facts,
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
  (parse_facts, '"quoted"', [Predicate('"quoted"')]),
  (parse_facts, '"negative(-3)"', [Predicate('"negative(-3)"')]),
  (parse_facts, 'negative(-3)', [Predicate("negative", Predicate(-3))]),
  (
    parse_facts,
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
    parse_facts,
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
    parse_facts,
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
    parse_facts,
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
  (parse_facts, "3(v)", None),
  (parse_facts, "_(1)", None),
]
