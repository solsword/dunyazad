% utils.lp
% Various utilities, including script functions.

#script (python)
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

def join(*args):
  return ''.join(str(a) for a in args)

def join_lines(*args):
  return '\n'.join(str(a) for a in args)

def mkmem(*args):
  return '.'.join(str(a) for a in args)
#end.
