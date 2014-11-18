"""
english.py
Tools for NLG. Uses various sub-modules like nouns and verbs.
"""

import re

from utils import *

import ans

import nouns
import verbs

ANYTAG = re.compile(r"(\b[A-Z]#[a-z_/]+\b)")

TAGS = {
  "noun": re.compile(r"\bN#([a-z_]+)/([a-z_]+)\b"),
  "verb": re.compile(r"\bV#([a-z]+)/([a-z]+)/([a-z_]+)\b"),
  #"pronoun": re.compile(r"\bP#[a-z]+/[a-z]+\b"),
  #"determined": re.compile(r"\bT#[a-z]+\b"),
}

TR_TYPE = {
  "party_member": "person",
  "item": "item",
  "actor": "entity",
}

TSABR = {
  "prs": "present",
  "pst": "past",
  "inf": "infinitive",
  "imp": "imperative",
  "prp": "present participle",
  "psp": "past participle",
}

NOUN_SCHEMAS = {
  "name":
    ans.Pr(
      "st",
      ans.Vr("Node"),
      ans.Pr("property",
        ans.Pr("name"),
        ans.Pr("inst", ans.Vr("Type"), ans.Vr("Key")),
        ans.Vr("Name")
      )
    ),
  "number":
    ans.Pr(
      "st",
      ans.Vr("Node"),
      ans.Pr("property",
        ans.Pr("number"),
        ans.Pr("inst", ans.Vr("Type"), ans.Vr("Key")),
        ans.Vr("Number")
      )
    ),
  "gender":
    ans.Pr(
      "st",
      ans.Vr("Node"),
      ans.Pr("property",
        ans.Pr("gender"),
        ans.Pr("inst", ans.Vr("Type"), ans.Vr("Key")),
        ans.Vr("Gender")
      )
    ),
  "person":
    ans.Pr(
      "st",
      ans.Vr("Node"),
      ans.Pr("property",
        ans.Pr("person"),
        ans.Pr("inst", ans.Vr("Type"), ans.Vr("Key")),
        ans.Vr("Person")
      )
    ),
  "determined":
    ans.Pr(
      "st",
      ans.Vr("Node"),
      ans.Pr("property",
        ans.Pr("determined"),
        ans.Pr("inst", ans.Vr("Type"), ans.Vr("Key")),
        ans.Vr("Determination")
      )
    ),
}

TEXT_SCHEMAS = {
  "intro_text":
    ans.PVr("txt", "intro_text", ans.Vr("Node"), ans.Vr("Text")),
  "potential_text":
    ans.PVr("txt", "potential_text", ans.Vr("Node"), ans.Vr("Text")),
  "option_text":
    ans.PVr(
      "txt", "option_text",
      ans.Vr("Node"),
      ans.Pr("option", ans.Vr("Opt")),
      ans.Vr("Text")
    ),
  "action_text":
    ans.PVr(
      "txt", "action_text",
      ans.Vr("Node"),
      ans.Pr("option", ans.Vr("Opt")),
      ans.Vr("Text")
    ),
}

def glean_nouns(story):
  result = {}
  for sc, binding in ans.bindings(NOUN_SCHEMAS, story):
    n = binding["st.property.inst.Key"].unquoted()
    t = binding["st.property.inst.Type"].unquoted()
    if n not in result:
      result[n] = nouns.Noun(n, TR_TYPE[t] if t in TR_TYPE else "thing")
    if sc == "name":
      result[n].name = binding["st.property.Name"].unquoted()
    elif sc == "number":
      result[n].number = binding["st.property.Number"].unquoted()
    elif sc == "gender":
      result[n].gender = binding["st.property.Gender"].unquoted()
    elif sc == "person":
      result[n].person = binding["st.property.Person"].unquoted()
    elif sc == "determined":
      d = binding["st.property.Determination"].unquoted()
      result[n].determined = d == "true"
  return result

def build_text(template, ndict, pnslots=None, introduced=None):
  """
  Takes a text template and builds a filled-in string using the given nouns
  along with pronoun and noun usage information. Returns a tuple of the
  constructed string and the resulting pronoun slots and noun introduction.
  """
  if pnslots == None:
    pnslots = {
      "I": [0, set()],
      "we": [0, set()],
      "you": [0, set()],
      "he": [0, set()],
      "she": [0, set()],
      "it": [0, set()],
      "they": [0, set()],
    }
  if introduced == None:
    introduced = set()
  bits = re.split(ANYTAG, template)
  result = ""
  for b in bits:
    add = b
    if '.' in b: # TODO: Better sentence-counting! (?)
      # there's the end of a sentence somewhere in this bit: clean out
      # ambiguous and expired slots
      for s in pnslots:
        if len(pnslots[s][1]) > 1 or pnslots[s][0] > 2:
          pnslots[s] = [0, set()]
        elif len(pnslots[s][1]) > 0:
          pnslots[s][0] += 1
    for t in TAGS:
      m = TAGS[t].fullmatch(b)
      if m:
        if t == "noun":
          noun = m.group(1)
          if noun not in ndict:
            # TODO: Something about this
            print("ERROR!")
            print(template, noun, ndict)
            exit(1)
          pro = m.group(2)
          case, position = nouns.casepos(pro)
          slot = pnslots[nouns.pnslot(ndict[noun])]
          if noun in slot[1] and len(slot[1]) == 1:
            slot[0] = 0
            add = nouns.pronoun(ndict[noun], case, position)
          else:
            if slot[0] > 0:
              slot[0] = 0
              slot[1] = { noun }
            else:
              slot[1].add(noun)
            if noun in introduced:
              add = nouns.definite(ndict[noun])
            else:
              introduced.add(noun)
              add = nouns.indefinite(ndict[noun])
        elif t == "verb":
          verb = m.group(1)
          tense = TSABR[m.group(2)]
          agree = m.group(3)
          if agree == "_plural":
            verbs.conjugation(verb, tense, "plural", "third")
          else:
            add = verbs.conj_ref(ndict[agree], verb, tense)
        # if we matched a tag, don't bother checking the other tags:
        break
    result += add
  return result, pnslots, introduced

def find_node_structure(story):
  # TODO: HERE!!
  return {
    "root": {}
  }

def build_node_text(node, nouns, pnslots, introduced):
  # TODO: A more rigorous capitalization approach.
  intro, pnslots, introduced = build_text(
    node["intro"],
    nouns,
    pnslots,
    introduced
  )
  intro = intro.capitalize()
  situation, pnslots, introduced = build_text(
    node["situation"],
    nouns,
    pnslots,
    introduced
  )
  situation += "."
  situation = situation.capitalize()
  options = ""
  for opt in node["options"]:
    # TODO: Thread pnslots & introduced out per-option!
    txt, _dc, _dc = build_text(
      node["options"][opt],
      nouns,
      pnslots,
      introduced
    )
    options += "  #{}\n".format(txt.capitalize())
    txt, _dc, _dc = build_text(
      node["outcomes"][opt],
      nouns,
      pnslots,
      introduced
    )
    options += "    {}\n".format(txt.capitalize())
    # TODO: an appropriate GOTO here!
    options += "    *finish\n"
  return """
{label}
{intro}
{situation}
*choice
{options}
""".format(
  label=node["label"],
  intro=intro,
  situation=situation,
  options=options
)

def build_story_text(story):
  nodes = {}
  nouns = glean_nouns(story)
  pnslots = {
    "I": [0, set()],
    "we": [0, set()],
    "you": [0, { "the_party" }],
    "he": [0, set()],
    "she": [0, set()],
    "it": [0, set()],
    "they": [0, set()],
  }
  introduced = { "the_party" }
  node_structure = find_node_structure(story)
  for sc, bnd in ans.bindings(TEXT_SCHEMAS, story):
    node = bnd["txt.Node"].unquoted()
    if node not in nodes:
      nodes[node] = {
        "label": "*label {}".format(str(node).replace(":", "_")),
        "intro": "",
        "situation": "",
        "options": {},
        "outcomes": {},
        # TODO: state-change text
      }
    if sc == "intro_text":
      nodes[node]["intro"] = bnd["txt.Text"].unquoted()
    elif sc == "potential_text":
      if nodes[node]["situation"]:
        nodes[node]["situation"] += " and "
      nodes[node]["situation"] += bnd["txt.Text"].unquoted()
    elif sc == "option_text":
      nodes[node]["options"][bnd["txt.option.Opt"]] = bnd["txt.Text"].unquoted()
    elif sc == "action_text":
      nodes[node]["outcomes"][bnd["txt.option.Opt"]] =bnd["txt.Text"].unquoted()
  # TODO: HERE!
  return build_node_text(nodes["root"], nouns, pnslots, introduced)
