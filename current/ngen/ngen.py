#!/usr/bin/env python3

"""
ngen.py
Generates names for things using glyph-level n-gram Markov generation.
"""

import random

CATEGORIES = [
  "city",
  "town",
  "road",
  "wilderness",
  "name",
  "surname",
]

SOURCE_FILES = {
  "city": "cities.txt",
  "town": "towns.txt",
  "road": "roads.txt",
  "wilderness": "wildernesses.txt",
  "name": "names.txt",
  "surname": "surnames.txt",
}

DEFMLEN = {
  "city": 24,
  "town": 20,
  "road": 14,
  "wilderness": 30,
  "name": 18,
  "surname": 24,
}

DEFMLEV = {
  "city": 4,
  "town": 4,
  "road": 3,
  "wilderness": 3,
  "name": 4,
  "surname": 3,
}

DEFIGNPRB = {
  "city": 0.25,
  "town": 0.25,
  "road": 0.05,
  "wilderness": 0.01,
  "name": 0.03,
  "surname": 0.5,
}

INPUT = {
  "city": set(),
  "town": set(),
  "road": set(),
  "wilderness": set(),
  "name": set(),
  "surname": set(),
}

GRAMS = {
  2: {
    "city": {},
    "town": {},
    "road": {},
    "wilderness": {},
    "name": {},
    "surname": {},
  },
  3: {
    "city": {},
    "town": {},
    "road": {},
    "wilderness": {},
    "name": {},
    "surname": {},
  },
  4: {
    "city": {},
    "town": {},
    "road": {},
    "wilderness": {},
    "name": {},
    "surname": {},
  },
}

PREFIXES = {
  "city": set(),
  "town": set(),
  "road": set(),
  "wilderness":
    {
      "Mount",
      "Black",
      "Blue",
      "Green",
      "Red",
      "Orange",
      "Yellow",
      "North",
      "East",
      "South",
      "West",
      "Northeast",
      "Northwest",
      "Southeast",
      "Southwest",
      "Coastal",
    },
  "name": set(),
  "surname": set(),
}

SUFFIXES = {
  "city":
    {
      "city"
    },
  "town": set(),
  "road": set(), # extracted from file
  "wilderness":
    {
      "Mountain",
      "Peak",
      "Summit",
      "Valley",
      "Vale",
      "Cliffs",
      "Bluffs",
      "Hill",
      "Hills",
      "Forest",
      "Grove",
      "Woods",
      "Wood",
      "Jungle",
      "Marsh",
      "Swamp",
      "Bog",
      "Plains",
      "Flats",
      "Dells",
      "Coast",
      "Shore",
      "River Basin",
      "River Floodplain",
    },
  "name": set(),
  "surname": set(),
}

for c in CATEGORIES:
  sf = SOURCE_FILES[c]
  with open(sf) as fin:
    lines = [l[:-1] for l in fin.readlines()]
    for l in lines:
      INPUT[c].add(l)
  if c == "road":
    for i, l in enumerate(lines):
      bits = l.split()
      if len(bits) > 1:
        lines[i] = ' '.join(bits[:-1])
        SUFFIXES["road"].add(bits[-1])
  for l in lines:
    l = '^' + l + '$'
    for i in range(1,len(l)):
      for level in [4, 3, 2]:
        if i >= level - 1:
          pref = l[i-level+1:i]
          if pref not in GRAMS[level][c]:
            GRAMS[level][c][pref] = {}
          if l[i] not in GRAMS[level][c][pref]:
            GRAMS[level][c][pref][l[i]] = 0
          GRAMS[level][c][pref][l[i]] += 1

# Transform count dictionaries into probability lists:
for c in CATEGORIES:
  for n in GRAMS:
    for pref in GRAMS[n][c]:
      total = sum(GRAMS[n][c][pref][char] for char in GRAMS[n][c][pref])
      plist = []
      for char in GRAMS[n][c][pref]:
        prob = GRAMS[n][c][pref][char] / total
        plist.append((char, prob))
      GRAMS[n][c][pref] = sorted(plist, key=lambda x:x[1], reverse=True)

#rkey = list(GRAMS[3]["road"].keys())[3]
#print(GRAMS[3]["road"][rkey])

def sample(plist, ignoreprob=False):
  if ignoreprob:
    return random.choice(plist)[0]
  result = plist[0][0]
  x = random.random() # [0,1)
  for char, prob in plist:
    result = char
    x -= prob
    if x < 0:
      break
  return result

def name(category, maxlevel=None, maxlen=None, ignoreprob=None):
  maxlen = maxlen or DEFMLEN[category]
  maxlevel = maxlevel or DEFMLEV[category]
  ignoreprob = ignoreprob or DEFIGNPRB[category]
  n = '^'
  while len(n) <= maxlen and n[-1] != '$':
    i = len(n)
    for level in [4, 3, 2]:
      if i >= level-1 and maxlevel >= level:
        pref = n[i-level+1:i]
        if pref in GRAMS[level][category]:
          n += sample(
            GRAMS[level][category][pref],
            random.random() < ignoreprob
          )
          break
    # fallback: add a vowel
    if i == len(n):
      n += random.choice("aeiou")
  return n[1:-1].strip()

def fancyname(category, maxlevel=None, maxlen=None, ignoreprob=None):
  base = ""
  while not base or base in INPUT[category]:
    base = name(category, maxlevel, maxlen, ignoreprob)
  pref = ''
  suf = ''
  if len(PREFIXES[category]) > 0:
    pref = random.choice(list(PREFIXES[category])) + ' '
  if len(SUFFIXES[category]) > 0:
    suf = ' ' + random.choice(list(SUFFIXES[category]))
  return pref + base + suf

def main():
  for c in CATEGORIES:
    print("{}: '{}'".format(c, fancyname(c)))

if __name__ == "__main__":
  main()
