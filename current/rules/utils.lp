% utils.lp
% Various utilities, including script functions.
% vim: syn=python

unique_key(State, @unique(M)) :-
  max_unique(M),
  get_unique_key(State).

unique_key_used(K) :-
  unique_key(State, K).

#script (python)
import gringo

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

def pred(str):
  return gringo.Fun(str)

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
  lastroundmax = int(lastroundmax)
  if state[0] <= lastroundmax:
    state[0] = lastroundmax + 1
  state[0] += 1
  return state[0]

def inst(base, number):
  return gringo.Fun(str(base) + "_" + str(number))

#end.
