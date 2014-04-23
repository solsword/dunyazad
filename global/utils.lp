% utils.lp
% Various utilities, including script functions.

#script (python)
def join(*args):
  return ''.join(str(a) for a in args)

def mkmem(*args):
  return '.'.join(str(a) for a in args)
#end.
