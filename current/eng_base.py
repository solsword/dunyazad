"""
eng_base.py
Care English functionality.
"""

from utils import *

vowels = "aieou"
consonants = "bcdfghjklmnpqrstvwxyz"

def table_match(table, search):
  for entry in table:
    if all(
      (search[i] == entry[i] or search[i] == "any" or entry[i] == "any")
      for i in range(len(search))
    ):
      return entry[-1]
  return None
