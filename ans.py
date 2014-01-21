'''
ans.py
Answer set data structures.
'''

from utils import *

class Predicate:
  def __init__(self, name, *args):
    self.name = name
    self.args = list(args)

  def __str__(self):
    if self.args:
      return "{}({})".format(self.name, ', '.join(self.args))
    else:
      return self.name

  def __hash__(self):
    return 47 * (
      47 * (
        31 + hash(self.args)
      ) + hash(self.name)
    )

  def __eq__(self, other):
    return self.name == other.name and self.args == other.args

  def __ne__(self, other):
    return not self == other


#class Constraint:
#  def __init__(self, typ, TODO):
#    self.typ = typ
#
#  def __str__(self):
#    if self.head:
#      if self.body:
#        return "{} :- {}".format(self.head, self.body)
#      else:
#        return str(self.head)
#    else:
#      return ":- {}".format(self.body)
#
#  def __hash__(self):
#    # TODO
#    return 47 * (
#      47 * (
#        31 + hash(self.args)
#      ) + hash(self.name)
#    )
#
#  def __eq__(self, other):
#    # TODO
#    return self.name == other.name and self.args == other.args
#
#  def __ne__(self, other):
#    return not self == other
#
#class Rule:
#  def __init__(self, head, body):
#    self.head = head or []
#    self.body = body or []
#
#  def __str__(self):
#    if self.head:
#      if self.body:
#        return "{} :- {}".format(self.head, self.body)
#      else:
#        return str(self.head)
#    else:
#      return ":- {}".format(self.body)
#
#  def __hash__(self):
#    return 47 * (
#      47 * (
#        31 + hash(self.body)
#      ) + hash(self.head)
#    )
#
#  def __eq__(self, other):
#    return self.head == other.head and self.body == other.body
#
#  def __ne__(self, other):
#    return not self == other

# shortcuts:
Pr = Predicate
#Cn = Constraint
#Rl = Rule
