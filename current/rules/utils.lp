% utils.lp
% Various utilities, including script functions.
% vim: syn=python

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

#def instify(predF, nodeF, idcounts={}):
#  node = str(nodeF)
#
#  # "inst/2" predicates get rewritten:
#  #print("T", predF, dir(predF))
#  #print("Ttest", predF)
#  #print("Tt", predF.name(), predF.args())
#  if type(predF) == str:
#    return predF
#  if predF.name() == "linst" and len(predF.args()) == 2:
#    # First, initialize the idcounts entry for this node if necessary:
#    if node not in idcounts:
#      idcounts[node] = {}
#      for other in idcounts:
#        if other != node:
#          for instantiated in idcounts[other]:
#            if instantiated in idcounts[node]:
#              idcounts[node][instantiated] += 1
#            else:
#              idcounts[node][instantiated] = 1
#    # Now
#    typF, nameF = predF.args()
#    #print("T2", typF, nameF)
#    idn = 1
#    tn = str(typF) + "_" + str(nameF)
#    if tn in idcounts[node]:
#      idn = idcounts[node][tn]
#    else:
#      idcounts[node][tn] = 1
#    return gringo.Fun("inst", [typF, join_(str(nameF), str(idn))])
#  else:
#    newargs = [instify(arg, nodeF) for arg in predF.args()]
#    #print("TZ", predF, newargs)
#    #print("TZ2", predF, predF.name(), newargs)
#    return gringo.Fun(predF.name(), newargs)

#end.
