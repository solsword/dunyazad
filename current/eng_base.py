"""
eng_base.py
Core English functionality.
"""

from utils import *

vowels = "aieou"
consonants = "bcdfghjklmnpqrstvwxyz"

GR_CASES = {
  "tense": (
    "present",
    "past",
    "infinitive",
    "imperative",
    "present participle",
    "past participle"
  ),
  "number": ("singular", "plural"),
  "person": ("first", "second", "third"),
  "gender": ("masculine", "feminine", "neuter"),
  "case": ("subjective", "objective", "possessive"),
  "position": ("modifier", "object"),
}

def table_match(table, search):
  for entry in table:
    if all(
      (search[i] == entry[i] or search[i] == "any" or entry[i] == "any")
      for i in range(len(search))
    ):
      return entry[-1]
  return None

def sentence(result):
  if not result.strip():
    return result
  if result[0].islower():
    result = result[0].upper() + result[1:]
  if result[-1] not in '.?!':
    result = result + '.'
  return result

