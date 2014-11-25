"""
english.py
Tools for NLG. Uses various sub-modules like nouns and verbs.
"""

import re
import copy

from utils import *

import ans

from ans import Pr, Vr, PVr, SbT

from eng_base import *

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
    Pr(
      "st",
      Vr("Node"),
      Pr("property",
        Pr("name"),
        Pr("inst", Vr("Type"), Vr("Key")),
        Vr("Name")
      )
    ),
  "number":
    Pr(
      "st",
      Vr("Node"),
      Pr("property",
        Pr("number"),
        Pr("inst", Vr("Type"), Vr("Key")),
        Vr("Number")
      )
    ),
  "gender":
    Pr(
      "st",
      Vr("Node"),
      Pr("property",
        Pr("gender"),
        Pr("inst", Vr("Type"), Vr("Key")),
        Vr("Gender")
      )
    ),
  "person":
    Pr(
      "st",
      Vr("Node"),
      Pr("property",
        Pr("person"),
        Pr("inst", Vr("Type"), Vr("Key")),
        Vr("Person")
      )
    ),
  "determined":
    Pr(
      "st",
      Vr("Node"),
      Pr("property",
        Pr("determined"),
        Pr("inst", Vr("Type"), Vr("Key")),
        Vr("Determination")
      )
    ),
}

TEXT_SCHEMAS = {
  "intro_text":
    PVr("txt", "intro_text", Vr("Node"), Vr("Text")),
  "potential_text":
    PVr("txt", "potential_text", Vr("Node"), Vr("Text")),
  "option_text":
    PVr("txt", "option_text", Vr("Node"), Pr("option", Vr("Opt")), Vr("Text")),
  "action_text":
    PVr("txt", "action_text", Vr("Node"), Pr("option", Vr("Opt")), Vr("Text")),
}

SUCCESSOR = Pr("successor", Vr("From"), Pr("option", Vr("Opt")), Vr("To"))

def conjugation_table(verb):
  result = {
    tense : [
      fmt.format(
        pronoun=table_match(
          nouns.PRONOUNS,
          (person, number, "neuter", "subjective", "any")
        ),
        verb=verbs.conjugation(verb, tense, number, person),
        be_p=verbs.conjugation("be", "present", number, person),
        have_p=verbs.conjugation("have", "present", number, person),
      )
        for person in GR_CASES["person"]
        for number in GR_CASES["number"]
    ]
      for tense, fmt in (
        ("present", "{pronoun} {verb}"),
        ("past", "{pronoun} {verb}"),
        ("present participle", "{pronoun} {be_p} {verb}"),
        ("past participle", "{pronoun} {have_p} {verb}"),
      )
  }
  result.update({
    "infinitive": "to {verb}".format(verb=verbs.conjugation(verb,"infinitive")),
    "imperative": "{verb} it".format(verb=verbs.conjugation(verb,"imperative")),
  })
  return result

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

def merge_pnslots(pns1, pns2):
  """
  Takes two sets of pronoun slots and merges them such that the result is valid
  for text that might follow text which resulted in either of the merged slot
  sets.
  """
  result = {}
  for pn in pns1:
    if pns1[pn][1] == pns2[pn][1]:
      result[pn] = [max(pns1[pn][0], pns2[pn][0]), set(pns1[pn][1])]
    else:
      # Any kind of ambiguity results in an empty slot:
      result[pn] = [0, set()]
  return result

def merge_txt_states(pilist):
  """
  Takes a list of pnslots, introduced pairs and returns a single pnslots,
  introduced pair that's valid no matter which of the input text states you're
  coming from.
  """
  result_pnslots = pilist[0][0]
  result_introduced = pilist[0][1]
  for pnslots, introduced in pilist[1:]:
    result_pnslots = merge_pnslots(result_pnslots, pnslots)
    result_introduced &= introduced
  return result_pnslots, result_introduced

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
  else:
    pnslots = copy.deepcopy(pnslots)
  if introduced == None:
    introduced = set()
  else:
    introduced = set(introduced)
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
  """
  Takes a story and looks at successor/3 predicates to determine the structure
  of nodes in the story, returning a dictionary that maps node names to both
  successor and predecessor entries: successor entries being option->node
  mappings and predecessor entries being a list of nodes that have this node as
  a successor.
  """
  result = {}
  for pr in story:
    scc = ans.bind(SUCCESSOR, pr)
    if scc:
      frm = scc["successor.From"].unquoted()
      opt = scc["successor.option.Opt"].unquoted()
      to = scc["successor.To"].unquoted()
      if frm not in result:
        result[frm] = {"successors":{}, "predecessors":[]}
      if to not in result:
        result[to] = {"successors":{}, "predecessors":[]}
      result[frm]["successors"][opt] = to
      result[to]["predecessors"].append(frm)
  return result

def build_node_text(node, node_structure, nouns, pnslots, introduced):
  """
  Builds text for the given node (should be a dictionary from the
  node_templates map). Returns the resulting text and a dictionary mapping
  options to their outgoing (pnslots, introduced) tuples.
  """
  outgoing = {}
  # TODO: A more rigorous capitalization approach.
  intro, _pnslots, _introduced = build_text(
    node["intro"],
    nouns,
    pnslots,
    introduced
  )
  intro = intro.capitalize()
  situation, _pnslots, _introduced = build_text(
    node["situation"],
    nouns,
    _pnslots,
    _introduced
  )
  situation += "."
  situation = situation.capitalize()
  options = ""
  if node["options"]:
    options = "*choice\n"
    for opt in node["options"]:
      txt, pnout, intout = build_text(
        node["options"][opt],
        nouns,
        _pnslots,
        _introduced
      )
      options += "  #{}\n".format(txt.capitalize())
      txt, pnout, intout = build_text(
        node["outcomes"][opt],
        nouns,
        pnout,
        intout
      )
      options += "    {}\n".format(txt.capitalize())
      successors = node_structure[node["name"]]["successors"]
      if opt in successors:
        scc = successors[opt]
        outgoing[scc] = (pnout, intout)
        options += "    *goto {}\n".format(scc.replace(":", "_"))
      else:
        options += "    *finish\n"
  else:
    options = "*finish"
  result = """
*label {label}
{intro}
{situation}
{options}
""".format(
  label=node["name"].replace(":", "_"),
  intro=intro,
  situation=situation,
  options=options
)
  return result, outgoing

def build_story_text(story, root=None):
  node_templates = {}

  # First, build all of the templates for the entire story:
  for sc, bnd in ans.bindings(TEXT_SCHEMAS, story):
    node = bnd["txt.Node"].unquoted()
    print("Adding {} template for node '{}'.".format(sc, node))
    if node not in node_templates:
      node_templates[node] = {
        "name": node,
        "intro": "",
        "situation": "",
        "options": {},
        "outcomes": {},
        # TODO: state-change text
      }
    txt = bnd["txt.Text"].unquoted()
    if sc == "intro_text":
      node_templates[node]["intro"] = txt
    elif sc == "potential_text":
      if node_templates[node]["situation"]:
        node_templates[node]["situation"] += " and "
      node_templates[node]["situation"] += txt
    elif sc == "option_text":
      opt = bnd["txt.option.Opt"].unquoted()
      node_templates[node]["options"][opt] = txt
    elif sc == "action_text":
      opt = bnd["txt.option.Opt"].unquoted()
      node_templates[node]["outcomes"][opt] = txt

  # Next, use the node structure to recursively render the story text in
  # ChoiceScript:
  nouns = glean_nouns(story)
  node_structure = find_node_structure(story)
  base_pnslots = {
    "I": [0, set()],
    "we": [0, set()],
    "you": [0, { "the_party" }],
    "he": [0, set()],
    "she": [0, set()],
    "it": [0, set()],
    "they": [0, set()],
  }
  base_introduced = { "the_party" }
  # Start with all root nodes on our open list:
  olist = [
    (n, base_pnslots, base_introduced) for n in node_templates.keys()
      if len(node_structure[n]["predecessors"]) == 0
  ]
  print("Root nodes: {}".format([n for (n, bp, bi) in olist]))
  # The ready dictionary keeps track of introduction and pronoun information
  # propagating between non-root nodes and has enough information to know when
  # a node is ready to be rendered:
  ready = {
    n: { pr: None for pr in node_structure[n]["predecessors"]}
      for n in node_templates.keys()
        if len(node_structure[n]["predecessors"]) > 0
  }
  results = []
  while olist:
    target, pnslots, introduced = olist.pop(0)
    print("Processing node: '{}'.".format(target))
    # build node text:
    txt, outgoing = build_node_text(
      node_templates[target],
      node_structure,
      nouns,
      pnslots,
      introduced
    )
    results.append(txt)
    # update our readiness information and propagate nodes to the open list as
    # they're fully ready:
    for n in [x for x in outgoing if x in ready]:
      # DEBUG:
      if None not in ready[n].values():
        raise RuntimeError("""
Updating readiness of already-ready node '{}' from node '{}'.
Readiness is: {}\
""".format(n, target, ready[n])
        )
      ready[n][target] = outgoing[n]
      if None not in ready[n].values():
        pns, intr = merge_txt_states(list(ready[n].values()))
        # TODO: Get rid of pnslots merging altogether?
        #olist.append((n, pns, intr))
        olist.append((n, base_pnslots, intr))
  return ("\n\n*comment " + '-'*72 + "\n\n").join(results)

# Testing:

_test_cases = [
  (
    conjugation_table,
    "be",
    {
      'present': ['I am', 'we are', 'you are', 'you are', 'it is', 'they are'],
      'past':
        ['I was', 'we were', 'you were', 'you were', 'it was', 'they were'],
      'present participle':
        ['I am being', 'we are being', 'you are being', 'you are being',
         'it is being', 'they are being'],
      'past participle':
        ['I have been', 'we have been', 'you have been', 'you have been',
         'it has been', 'they have been'],
      'infinitive': 'to be',
      'imperative': 'be it',
    }
  ),
  (
    conjugation_table,
    "do",
    {
      'present': ['I do', 'we do', 'you do', 'you do', 'it does', 'they do'],
      'past':
        ['I did', 'we did', 'you did', 'you did', 'it did', 'they did'],
      'present participle':
        ['I am doing', 'we are doing', 'you are doing', 'you are doing',
         'it is doing', 'they are doing'],
      'past participle':
        ['I have done', 'we have done', 'you have done', 'you have done',
         'it has done', 'they have done'],
      'infinitive': 'to do',
      'imperative': 'do it',
    }
  ),
  (
    conjugation_table,
    "have",
    {
      'present':
        ['I have', 'we have', 'you have', 'you have', 'it has', 'they have'],
      'past': ['I had', 'we had', 'you had', 'you had', 'it had', 'they had'],
      'present participle':
        ['I am having', 'we are having', 'you are having', 'you are having',
         'it is having', 'they are having'],
      'past participle':
        ['I have had', 'we have had', 'you have had', 'you have had',
         'it has had', 'they have had'],
      'infinitive': 'to have',
      'imperative': 'have it',
    }

  ),
  (
    conjugation_table,
    "travel",
    {
      'present':
        ['I travel', 'we travel', 'you travel', 'you travel', 'it travels',
         'they travel'],
      'past':
        ['I traveled', 'we traveled', 'you traveled', 'you traveled',
         'it traveled', 'they traveled'],
      'present participle':
        ['I am traveling', 'we are traveling', 'you are traveling',
         'you are traveling', 'it is traveling', 'they are traveling'],
      'past participle':
        ['I have traveled', 'we have traveled', 'you have traveled',
         'you have traveled', 'it has traveled', 'they have traveled'],
      'infinitive': 'to travel',
      'imperative': 'travel it',
    }

  ),
]
