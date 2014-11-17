"""
english.py
Tools for NLG.
"""

from utils import *

@instance
class number:
  @instance
  class singular: pass
  @instance
  class plural: pass

@instance
class gender:
  @instance
  class masculine: pass
  @instance
  class feminine: pass
  @instance
  class neuter: pass
