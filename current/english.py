"""
english.py
Tools for NLG. Uses various sub-modules like nouns and verbs.
"""

import re
import copy
import random
import os

from utils import *

import ans

from ans import Pr, Vr, PVr, SbT

from eng_base import *

import nouns
import verbs

STATIC_RULES_DIR = "surface"

STATIC_RULES_SOURCES = list(
  walk_files(STATIC_RULES_DIR, lambda f: f.endswith(".rls"))
)

VAR = re.compile(r"(\?[a-z_]+)")

SUBST_SPLIT = re.compile(r"(\[\[(?:[A-Z]*\|)?[a-z_?*/]+\]\])")

SUBST = re.compile(r"\[\[([A-Z]*\|)?([a-z_?*/]+)\]\]")

ANYTAG = re.compile(r"(\b[A-Z]#[a-z_0-9/?]+\b)")

CAP = re.compile(r"@CAP@(.)")

TAGS = {
  "noun": re.compile(r"\bN#(\??[a-z_][a-z_0-9]*)/([a-z_]+)\b"),
  "verb": re.compile(r"\bV#([a-z]+)/([a-z]+)/(\??[a-z_][a-z_0-9]*)\b"),
}

TR_TYPE = {
  "party_member": "person",
  "item": "item",
  "actor": "person",
}

TSABR = {
  "prs": "present",
  "prsc": "present continuous",
  "pst": "past",
  "pstc": "past continuous",
  "ftr": "future",
  "ftrc": "future continuous",
  "rinf": "raw infinitive",
  "inf": "infinitive",
  "imp": "imperative",
  "prsp": "present perfect",
  "prspc": "present perfect continuous",
  "pstp": "past perfect",
  "pstpc": "past perfect continuous",
  "ftrp": "future perfect",
  "ftrpc": "future perfect continuous",
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
  "party_member":
    Pr(
      "st",
      Vr("Node"),
      Pr("state",
        Pr("party_member"),
        Pr("inst", Vr("Type"), Vr("Key")),
      )
    ),
}

STRUCTURE_SCHEMAS = {
  "action":
    Pr(
      "at",
      Vr("Node"),
      Pr("action", Pr("option", Vr("Option")), Vr("Action"))
    ),
  "outcome":
    Pr(
      "at",
      Vr("Node"),
      Pr("outcome", Pr("option", Vr("Option")), Vr("Outcome"))
    ),
  "initiator":
    Pr(
      "at",
      Vr("Node"),
      Pr("initiator", Pr("option", Vr("Option")), SbT("Initiator"))
    ),
  "arg":
    Pr(
      "at",
      Vr("Node"),
      Pr("arg", Pr("option", Vr("Option")), Vr("Arg"), SbT("Value"))
    ),
  "setup": Pr("setup", Vr("Node"), Vr("Setup")),
  "setup_arg":
    Pr(
      "at",
      Vr("Node"),
      Pr("setup_arg", Vr("Arg"), SbT("Value"))
    ),
}

INSTANCE_SCHEMA = Pr("inst", Vr("Type"), Vr("Key"))

TEXT_SCHEMAS = {
  "intro_text":
    PVr("txt", "intro_text", Vr("Node"), Vr("Setup"), Vr("Text")),
  "potential_text":
    PVr("txt", "potential_text", Vr("Node"), Vr("Text")),
#  "option_text":
#    PVr("txt", "option_text", Vr("Node"), Pr("option", Vr("Opt")), Vr("Text")),
#  "action_text":
#    PVr("txt", "action_text", Vr("Node"), Pr("option", Vr("Opt")), Vr("Text")),
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
    "infinitive": "{verb}".format(verb=verbs.conjugation(verb,"infinitive")),
    "imperative": "{verb} it".format(verb=verbs.conjugation(verb,"imperative")),
  })
  return result

def glean_nouns(story):
  """
  Takes a story and builds a nouns dictionary from the nouns it finds therein.
  """
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
    elif sc == "party_member":
      result[n].is_party_member = True
  return result

def glean_context_variables(story):
  """
  Takes a story and builds a variables dictionary that maps node/option pairs
  to variable mappings at that point in the story. Variables include:

    '_action' - the name of the action
    '_outcome' - the outcome of the action
    '_initiator' -  the initiator of the action
    - all action arguments by name

  Setups also get their own entries under node/'setup' for nodes that have a
  setup. These include:

    '_setup' - the name of the setup
    - all setup arguments by name
  """
  result = {}
  for sc, binding in ans.bindings(STRUCTURE_SCHEMAS, story):
    if sc == "action":
      n = binding["at.Node"].unquoted()
      o = binding["at.action.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}
      if "setup" not in result[n]:
        result[n]["setup"] = {}

      a = binding["at.action.Action"].unquoted()
      result[n][o]["_action"] = a

    if sc == "outcome":
      n = binding["at.Node"].unquoted()
      o = binding["at.outcome.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}

      out = binding["at.outcome.Outcome"].unquoted()
      result[n][o]["_outcome"] = out

    elif sc == "initiator":
      n = binding["at.Node"].unquoted()
      o = binding["at.initiator.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}

      ipr = binding["at.initiator.Initiator"]
      sb = ans.bind(INSTANCE_SCHEMA, ipr)
      if sb:
        i = sb["inst.Key"].unquoted()
      else:
        i = ipr.unquoted()
      result[n][o]["_initiator"] = i

    elif sc == "arg":
      n = binding["at.Node"].unquoted()
      o = binding["at.arg.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}

      a = binding["at.arg.Arg"].unquoted()
      vpr = binding["at.arg.Value"]
      sb = ans.bind(INSTANCE_SCHEMA, vpr)
      if sb:
        v = sb["inst.Key"].unquoted()
      else:
        v = vpr.unquoted()
      result[n][o][a] = v

    elif sc == "setup":
      n = binding["setup.Node"].unquoted()
      if n not in result:
        result[n] = {}
      if "setup" not in result[n]:
        result[n]["setup"] = {}

      s = binding["setup.Setup"].unquoted()
      result[n]["setup"]["_setup"] = s

    elif sc == "setup_arg":
      n = binding["at.Node"].unquoted()
      if n not in result:
        result[n] = {}
      if "setup" not in result[n]:
        result[n]["setup"] = {}

      a = binding["at.setup_arg.Arg"].unquoted()
      vpr = binding["at.setup_arg.Value"]
      sb = ans.bind(INSTANCE_SCHEMA, vpr)
      if sb:
        v = sb["inst.Key"].unquoted()
      else:
        v = vpr.unquoted()
      result[n]["setup"][a] = v
  return result
  # TODO: HERE! Thread this info through and use it!

def collate_rules(story):
  """
  Takes a complete story and collates a full set of grammar rules for use in
  build_text.
  """
  result = {}
  for f in STATIC_RULES_SOURCES:
    with open(f, 'r') as fin:
      key = None
      mode = "lines"
      para = ""
      for ln, line in enumerate(fin.readlines()):
        if mode == "lines":
          if not line.strip() or line.strip()[0] == "%":
            continue
          if line[0] == ":":
            key = line[1:-1]
            if key not in result:
              result[key] = []
            mode = "lines"
          elif line[0] == ">":
            key = line[1:-1]
            mode = "paragraph"
            para = ""
          elif key and line.strip():
            result[key].append(line[:-1]) # get rid of the newline
          elif key == None and line.strip():
            raise ValueError(
              "{}:{} - Rule product has no key.".format(f,ln)
            )
        elif mode == "paragraph":
          if line == "<":
            result[key].append(para)
            key = None
            mode = "lines"
          else:
            para += line
      if mode == "paragraph" and key != None:
        raise ValueError(
          "{} - Unterminated paragraph entry for key '{}'.".format(f, key)
        )
  # TODO: dynamic rules like aspects!
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

def keymatch(test, key):
  """
  Tests whether the given concrete key matches the given possibly variadic key.
  """
  tp = test.split('/')
  kp = key.split('/')
  if len(kp) > len(tp):
    return False
  for i, k in enumerate(kp):
    if tp[i] != k and k != '?':
      if i == len(kp) - 1 and k == '*':
        continue
      return False
  if len(kp) < len(tp) and kp[-1] != '*':
    return False
  return True

def subst_result(rules, key, flags):
  """
  Given a set of substitution rules, returns the substitution result for the
  given key using the given flags.
  """
  matching_keys = [k for k in rules if keymatch(k, key)]
  all_possibilities = []
  for ps in [rules[k] for k in matching_keys]:
    all_possibilities.extend(ps)
  # TODO: better/controllable randomness?
  if (len(all_possibilities) == 0):
    print("ERROR: No possible substitutions for key '{}'.".format(key))
  result = random.choice(all_possibilities)
  if 'S' in flags:
    result = "@CAP@" + sentence(result)
  return result

def subst_vars(text, vs):
  """
  Given a text with some variable substitutions to be made returns the result
  text after all argument substitutions.
  """
  result = ""
  bits = re.split(VAR, text)
  for b in bits:
    add = b
    m = VAR.fullmatch(b)
    if m:
      var = m.group(1)[1:] # take off the initial '?'
      if var in vs:
        add = vs[var]
      else:
        add = "ERROR: unknown variable '{}'".format(var)
    result += add
  return result

def subst_rules(text, rules):
  """
  Works like subst_args but does (one round of) rules substitutions instead of
  doing variable substitutions.
  """
  result = ""
  bits = re.split(SUBST_SPLIT, text)
  for b in bits:
    add = b
    m = SUBST.fullmatch(b)
    if m:
      if (m.group(1)):
        flags = m.group(1)[:-1] # take off the '|'
      else:
        flags = ''
      key = m.group(2)
      add = subst_result(rules, key, flags)
    result += add
  return result

def build_text(
  template,
  rules,
  cvars,
  ndict,
  pnslots=None,
  introduced=None,
  timeshift=None
):
  """
  Takes a text template and builds a filled-in string using the given rules,
  variables, and nouns along with pronoun and noun usage information. If a
  timeshift is given it should be either "past" or "future" and it will be
  applied to all the verbs in the text. Returns a tuple of the constructed
  string and the resulting rules, pronoun slots, and noun introduction.
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
  # First, repeatedly perform variable and rule substitutions until we've
  # reached a base text:
  bits = re.split(SUBST_SPLIT, template)
  template = subst_vars(template, cvars)
  while re.search(SUBST, template):
    template = subst_rules(template, rules)
    template = subst_vars(template, cvars)
  # Next, fill in any tags:
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
            print("ERROR! Noun '{}' not in ndict.".format(noun))
            print("Match is: '{}'".format(str(m)))
            print(template)
            print(noun)
            print(ndict)
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
            add = verbs.conjugation(verb, tense, "plural", "third", timeshift)
          elif agree == "_singular":
            add = verbs.conjugation(verb, tense, "singular", "third", timeshift)
          else:
            add = verbs.conj_ref(ndict[agree], verb, tense, timeshift)
        # if we matched a tag, don't bother checking the other tags:
        break
    result += add
  # Finally process capitalization directives:
  result = re.sub(CAP, lambda m: m.group(1).upper(), result)
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

def build_node_text(
  node,
  node_structure,
  grammar_rules,
  context_variables,
  nouns,
  pnslots,
  introduced,
  timeshift=None
):
  """
  Builds text for the given node (should be a dictionary from the
  node_templates map). Returns the resulting text and a dictionary mapping
  options to their outgoing (pnslots, introduced) tuples. If a timeshift is
  given, it should be either "past" or "future" and will be applied to all
  verbs in the text generated.
  """
  outgoing = {}
  # TODO: A more rigorous capitalization approach.
  intro, _pnslots, _introduced = build_text(
    node["intro"],
    grammar_rules,
    context_variables["setup"],
    nouns,
    pnslots,
    introduced,
    timeshift
  )
  intro = sentence(intro)
  situation, _pnslots, _introduced = build_text(
    node["situation"],
    grammar_rules,
    context_variables["setup"],
    nouns,
    _pnslots,
    _introduced,
    timeshift
  )
  situation = sentence(situation)
  options = ""
  if node["options"]:
    options = "*choice\n"
    for opt in node["options"]:
      txt, pnout, intout = build_text(
        node["options"][opt],
        grammar_rules,
        context_variables[opt],
        nouns,
        _pnslots,
        _introduced,
        timeshift
      )
      options += "  #{}\n".format(sentence(txt))
      txt, pnout, intout = build_text(
        node["outcomes"][opt],
        grammar_rules,
        context_variables[opt],
        nouns,
        pnout,
        intout,
        timeshift
      )
      options += "    {}\n".format(sentence(txt))
      successors = node_structure[node["name"]]["successors"]
      if opt in successors:
        scc = successors[opt]
        outgoing[scc] = (pnout, intout)
        options += "    *goto {}\n".format(scc)
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
  label=node["name"],
  intro=intro,
  situation=situation,
  options=options
)
  return result, outgoing

def build_story_text(story, timeshift=None):
  node_templates = {}
  gr_rules = collate_rules(story)

  #print("Build story text rules:")
  #print(gr_rules)

  # First, build the dictionary of templates for all polished nodes:
  for sc, bnd in ans.bindings(
    { "_": Pr("node_status_reached", Vr("Node"), Pr("polished")) },
    story
  ):
    node = bnd["node_status_reached.Node"].unquoted()
    node_templates[node] = {
      "name": node,
      "intro": "",
      "situation": "",
      "options": {},
      "outcomes": {},
      # TODO: state-change text
    }

  # Next, build the intro and potential templates for the story:
  for sc, bnd in ans.bindings(TEXT_SCHEMAS, story):
    node = bnd["txt.Node"].unquoted()
    print("Adding {} template for node '{}'...".format(sc, node))
    txt = bnd["txt.Text"].unquoted()
    if sc == "intro_text":
      if node_templates[node]["intro"]:
        node_templates[node]["intro"] += " "
      node_templates[node]["intro"] += txt
    elif sc == "potential_text":
      if node_templates[node]["situation"]:
        node_templates[node]["situation"] += " and "
      node_templates[node]["situation"] += txt

    #elif sc == "option_text":
    #  opt = bnd["txt.option.Opt"].unquoted()
    #  node_templates[node]["options"][opt] = txt
    #elif sc == "action_text":
    #  opt = bnd["txt.option.Opt"].unquoted()
    #  node_templates[node]["outcomes"][opt] = txt

  # Next find context variables which also gives us an idea of the story
  # structure:
  cvrs = glean_context_variables(story)

  # Error check:
  for k in [key for key in cvrs if key not in node_templates]:
    print("WARNING: Node '{}' has context but isn't polished.".format(k))
  for k in [key for key in node_templates if key not in cvrs]:
    print("WARNING: Node '{}' is polished, but has no context.".format(k))

  # Iterate over our nodes and options adding static option/outcome templates:
  for node in cvrs:
    for option in [o for o in cvrs[node] if o != "setup"]:
      nts = node_templates[node]
      nts["options"][option] = "[[S|action/?_action/option]]"
      nts["outcomes"][option] = "[[S|action/?_action/outcome/?_outcome]]"

  # Next, use the node structure to recursively render the story text in
  # ChoiceScript:
  nouns = glean_nouns(story)
  node_structure = find_node_structure(story)
  base_pnslots = {
    "I": [0, set()],
    "we": [0, set()],
    "you": [0, { "you" }],
    "he": [0, set()],
    "she": [0, set()],
    "it": [0, set()],
    "they": [0, set()],
  }
  base_introduced = { "you" }
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
    #print("open:", [ole[0] for ole in olist])
    target, pnslots, introduced = olist.pop(0)
    # build node text:
    txt, outgoing = build_node_text(
      node_templates[target],
      node_structure,
      gr_rules,
      cvrs[target],
      nouns,
      pnslots,
      introduced,
      timeshift
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
