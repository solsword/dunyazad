% utils.lp
% Various utilities, including script functions.
% vim: syn=python

unique_key(State, @unique(M)) :-
  max_unique(M),
  get_unique_key(State).

unique_key_used(K) :-
  unique_key(State, K).

#script (python)
import clingo

def capitalize(arg):
  s = str(arg)
  return s[0].upper() + s[1:]

def the(arg):
  s = str(arg)
  return "the " + s

def an(arg):
  s = str(arg)
  if s[0] in "aeiouAEIOU":
    return "an " + s
  else:
    return "a " + s

def pred(quoted):
  if isinstance(quoted, clingo.Symbol):
    if quoted.type == clingo.SymbolType.String:
      quoted = quoted.string
    else:
      quoted = quoted.name
  else:
    quoted = str(quoted)
  return clingo.Function(quoted, [])

def join(*args):
  return ''.join(str(a) for a in args)

def join_(*args):
  return '_'.join(str(a) for a in args)

def fmt(tmplt, *args):
  return str(tmplt).format(*(str(a) for a in args))

def join_lines(*args):
  return '\n'.join(str(a) for a in args)

def mkmem(*args):
  return '.'.join(str(a) for a in args)

def unique(lastroundmax, state=[0]):
  lastroundmax = lastroundmax.number
  if state[0] <= lastroundmax:
    state[0] = lastroundmax + 1
  state[0] += 1
  return state[0]

def inst(base, number):
  if isinstance(base, clingo.Symbol):
    if base.type == clingo.SymbolType.String:
      base = base.string
    else:
      base = base.name
  else:
    base = str(base)

  if isinstance(number, clingo.Symbol):
    if number.type == clingo.SymbolType.Number:
      number = number.number
    else:
      number = number.name
  else:
    number = str(number)

  return clingo.Function("{}_{}".format(base, number), [])

#end.
