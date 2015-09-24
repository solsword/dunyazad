"""
english.py
Tools for NLG. Uses various sub-modules like nouns and verbs.
"""

import re
import copy
import random
import os

from utils import *

import packrat

import ans

from ans import Pr, Vr, PVr, SbT

from eng_base import *

import nouns
import verbs

STATIC_RULES_DIR = "surface"

STATIC_RULES_SOURCES = list(
  walk_files(STATIC_RULES_DIR, lambda f: f.endswith(".rls"))
)

VAR = re.compile(r"(\?[a-z_A-Z0-9]+)")

SBST_FLAGS = re.compile(r"([A-Z]+)\|")
SBST_KEY = re.compile(r"([A-Za-z_0-9]+)=")

KV_TOKENS = re.compile(r"(@)|(\\.)|(\[\[)|(\]\])")

ANYTAG = re.compile(r"(\b[A-Z]#[a-zA-Z_0-9/?']+\b)")

CAP = re.compile(r"(?:@CAP@)+(.)")
PAR = re.compile(r"@PAR@")
BREAK = re.compile(r"@@(?!CAP@)(?!PAR@)")

MAX_PRONOUN_AGE = 1

TAGS = {
  "directive": re.compile(r"\bD#([a-z_][a-z_0-9]*)/(\??[a-z_][a-z_0-9]*)\b"),
  "noun": re.compile(r"\bN#(\??[a-z_A-Z][a-z_A-Z0-9]*)/([a-z_]+)\b"),
  "verb":
    re.compile(r"\bV#([a-zA-Z']+)/([a-z]+)/(\??[a-z_A_Z][a-z_A-Z0-9]*)\b"),
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
  "class":
    Pr(
      "st",
      Vr("Node"),
      Pr("property",
        Pr("type"),
        Pr("inst", Vr("Type"), Vr("Key")),
        Vr("Class")
      )
    ),
  "name":
    Pr(
      "surface_property",
      Pr("name"),
      Pr("inst", Vr("Type"), Vr("Key")),
      Vr("Name")
    ),
  "number":
    Pr(
      "surface_property",
      Pr("number"),
      Pr("inst", Vr("Type"), Vr("Key")),
      Vr("Number")
    ),
  "gender":
    Pr(
      "surface_property",
      Pr("gender"),
      Pr("inst", Vr("Type"), Vr("Key")),
      Vr("Gender")
    ),
  "person":
    Pr(
      "surface_property",
      Pr("person"),
      Pr("inst", Vr("Type"), Vr("Key")),
      Vr("Person")
    ),
  "determined":
    Pr(
      "surface_property",
      Pr("determined"),
      Pr("inst", Vr("Type"), Vr("Key")),
      Vr("Determination")
    ),
  "has_item": 
    Pr(
      "st",
      Pr("root"),
      Pr(
        "relation",
        Pr("has_item"),
        PVr("owner", "inst", Vr("Type"), Vr("Key")),
        PVr("item", "inst", Vr("Type"), Vr("Key")),
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
      Pr(
        "outcome",
        Pr("option", Vr("Option")),
        Pr("o", Vr("OutVar"), Vr("OutVal"))
      )
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
  "relevant_skill":
    Pr(
      "at",
      Vr("Node"),
      Pr("relevant_skill",
        Pr("option", Vr("Option")),
        PVr("actor", "inst", Vr("Type"), Vr("Key")),
        Pr("has"),
        Vr("Skill")
      )
    ),
  "relevant_skill_missing":
    Pr(
      "at",
      Vr("Node"),
      Pr("relevant_skill",
        Pr("option", Vr("Option")),
        PVr("actor", "inst", Vr("Type"), Vr("Key")),
        Pr("missing"),
        Vr("Skill")
      )
    ),
  "relevant_tool":
    Pr(
      "at",
      Vr("Node"),
      Pr("relevant_tool",
        Pr("option", Vr("Option")),
        PVr("actor", "inst", Vr("Type"), Vr("Key")),
        Pr("has"),
        PVr("item", "inst", Vr("Type"), Vr("Key")),
      )
    ),
  "relevant_tool_missing":
    Pr(
      "at",
      Vr("Node"),
      Pr("relevant_tool",
        Pr("option", Vr("Option")),
        PVr("actor", "inst", Vr("Type"), Vr("Key")),
        Pr("missing"),
        Vr("Skill")
      )
    ),
  "potential_state":
    Pr(
      "at",
      Vr("Node"),
      Pr("potential", Vr("PType"), Pr("state", Vr("SName"), SbT("Inst")))
    ),
  "potential_property":
    Pr(
      "at",
      Vr("Node"),
      Pr(
        "potential",
        Vr("PType"),
        Pr("property", Vr("PName"), SbT("Inst"), Vr("PVal"))
      )
    ),
  "potential_relation":
    Pr(
      "at",
      Vr("Node"),
      Pr(
        "potential",
        Vr("PType"),
        Pr("relation", Vr("RName"), SbT("From"), SbT("To"))
      )
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

PROLOGUE_SCHEMAS = {
  "has_item": 
    Pr(
      "st",
      Pr("root"),
      Pr(
        "relation",
        Pr("has_item"),
        PVr("owner", "inst", Vr("Type"), Vr("Key")),
        PVr("item", "inst", Vr("Type"), Vr("Key")),
      )
    ),
  "has_skill": 
    Pr(
      "st",
      Pr("root"),
      Pr(
        "property",
        Pr("has_skill"),
        PVr("inst", "inst", Vr("Type"), Vr("Key")),
        Vr("Skill")
      )
    ),
  "relationship":
    Pr(
      "st",
      Pr("root"),
      Pr(
        "property",
        Pr("relationship"),
        Pr("inst", Vr("Type"), Vr("Key")),
        Vr("Relationship")
      )
    ),
}

SUCCESSOR = Pr("successor", Vr("From"), Pr("option", Vr("Opt")), Vr("To"))

NATIONS = [
  "Uuland",
  "Aswand",
  "Zhawe",
  "Fuella",
  "Toowoad",
  "Bebekken",
  "Bixeira",
  "Arre Cira",
  "Velin",
  "Jichish",
  "Lincun",
  "Guremi",
  "Bryam",
  "Kalkhan",
  "Pora Zamot",
  "Jyväsky",
  "Ron Parcos",
  "Jpenyu",
  "Cejavri",
  "Tucumán",
  "Shenza",
  "Gorholm Sal",
  "Khāminár",
]

CITIES = [
  "Evyame",
  "Kikris",
  "Quexian",
  "Darsk",
  "Nhatpur",
  "Guato",
  "Czel Yasa",
  "Onarnówka",
  "Muromai",
  "Goplin",
  "Charín",
  "Ransk",
  "Baghī",
  "Prinernevon",
  "Ha Loudi",
  "Mochishi",
  "Moinha",
  "Újszállow",
  "Enshya",
  "Aix-un-Sakuts",
  "Yúteld",
  "Såbuhen",
]

SPECIAL_FILTERING_VARIABLES = {
  "_Now": "_node",
  "_Opt": "_option",
  "_Act": "_action",
}

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
    if "st.property.inst.Key" in binding:
      n = binding["st.property.inst.Key"].unquoted()
    elif "st.state.inst.Key" in binding:
      n = binding["st.state.inst.Key"].unquoted()
    elif "st.relation.item.Key" in binding:
      n = binding["st.relation.item.Key"].unquoted()
    elif "surface_property.inst.Key" in binding:
      n = binding["surface_property.inst.Key"].unquoted()
    else:
      raise ValueError(
        "Unknown binding structure for noun schema:\n{}".format(binding)
      )
    if n not in result:
      result[n] = nouns.Noun(n)
    if sc == "class":
      result[n].cls = binding["st.property.Class"].unquoted()
    elif sc == "name":
      result[n].name = binding["surface_property.Name"].unquoted()
    elif sc == "number":
      result[n].number = binding["surface_property.Number"].unquoted()
    elif sc == "gender":
      result[n].gender = binding["surface_property.Gender"].unquoted()
    elif sc == "person":
      result[n].person = binding["surface_property.Person"].unquoted()
    elif sc == "determined":
      d = binding["surface_property.Determination"].unquoted()
      result[n].determined = d == "true"
    elif sc == "has_item":
      o = binding["st.relation.owner.Key"].unquoted()
      if o not in result:
        result[o] = nouns.Noun(o)
      result[n].owner = result[o]
    elif sc == "party_member":
      result[n].is_party_member = True
  return result

def glean_intro_variables(story, nouns):
  """
  Takes a story and builds a variables dictionary used for rendering the
  prologue. The following variables are defined:

      '_node' - set to "root"
      'nation' - the nation that you come from
      'city' - the city that you come from
      'destination' - the country that is your destination
      'n_party_members' - the number of party members (including you). Either
        "two" or "three"
      'your_name' - the name of the main character
      'your_items' - a list variable listing your items by key
      'your_skills' - a list variable listing your skills by name

      'member_*_tag' - the name of a party member (other than you)
      'member_*_relation' - the relation of a party member
      'member_*_items' - the items of a party member
      'member_*_skills' - the skills of a party member

      '*' here is 'one' or 'two'
  """
  result = { "_node": "root" }
  party_members = [nouns[n] for n in nouns if nouns[n].is_party_member]
  if len(party_members) == 1:
    result["n_party_members"] = "one"
  elif len(party_members) == 2:
    result["n_party_members"] = "two"
  elif len(party_members) == 3:
    result["n_party_members"] = "three"
  else:
    result["n_party_members"] = "ERROR: bad number of party members: {}".format(
      len(party_members)
    )
  random.shuffle(NATIONS)
  result["nation"] = NATIONS[0]
  result["destination"] = NATIONS[1]
  result["city"] = random.choice(CITIES)
  result["your_items"] = []
  result["your_skills"] = []
  # fill in defaults from noun info:
  for pm in party_members:
    if pm.tag == "you":
      result["your_name"] = pm.name
    else:
      result[pm.tag + "_tag"] = pm.tag
      result[pm.tag + "_relationship"] = "ERROR: unknown relationship"
      result[pm.tag + "_items"] = []
      result[pm.tag + "_skills"] = []
  # update using predicate info:
  for sc, b in ans.bindings(PROLOGUE_SCHEMAS, story):
    if sc == "has_item":
      o = b["st.relation.owner.Key"].unquoted()
      i = b["st.relation.item.Key"].unquoted()
      if o == "you":
        result["your_items"].append(i)
      elif nouns[o].is_party_member:
        result[o + "_items"].append(i)
    elif sc == "has_skill":
      n = b["st.property.inst.Key"].unquoted()
      s = b["st.property.Skill"].unquoted()
      if n == "you":
        result["your_skills"].append(s)
      elif nouns[n].is_party_member:
        result[n + "_skills"].append(s)
    elif sc == "relationship":
      n = b["st.property.inst.Key"].unquoted()
      if nouns[n].is_party_member and n != "you":
        result[n + "_relation"] = b["st.property.Relationship"].unquoted()
    else:
      raise ValueError("Unknown schema '{}' in PROLOGUE_SCHEMAS.".format(sc))
  return result

def glean_context_variables(story):
  """
  Takes a story and builds a variables dictionary that maps node/option pairs
  to variable mappings at that point in the story. Variables include:

    '_node' - the id of the current node
    '_option' - the number of the current option
    '_setting' - the current setting
    '_action' - the name of the action
    '_outcome_*' - the values of each outcome variable for the action
    '_initiator' -  the initiator of the action
    '_relevant_skills' - a mapping from actors to lists of relevant skills
    '_relevant_tools' - a mapping from actors to lists of relevant tools
    - all action arguments by name

  Setups also get their own entries under node/'setup' for nodes that have a
  setup. These include:

    '_setup' - the name of the setup
    - all setup arguments by name

  Finally, each potential for a node gets put into the list node/'potentials'
  as a dictionary with the following keys:

    '_ptype' - the type of potential ('problem' or 'opportunity')
    '_stype' - the type of state ('state', 'property', or 'relation')
    '_sname' - the name of the state (e.g. 'threatening')
    '_inst' - for 'state'- and 'property'-type states, the instance id
    '_value' - for 'property'-type states, the property value
    '_from' - for 'relation'-type states, the 'from' instance id
    '_to' - for 'relation'-type states, the 'to' instance id
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

      result[n][o]["_node"] = n
      result[n][o]["_option"] = o

      a = binding["at.action.Action"].unquoted()
      result[n][o]["_action"] = a

    if sc == "outcome":
      n = binding["at.Node"].unquoted()
      o = binding["at.outcome.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}

      outvar = binding["at.outcome.o.OutVar"].unquoted()
      outval = binding["at.outcome.o.OutVal"].unquoted()
      result[n][o]["_outcome_" + outvar] = outval

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

    elif sc == "relevant_skill":
      n = binding["at.Node"].unquoted()
      o = binding["at.relevant_skill.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}

      a = binding["at.relevant_skill.actor.Key"].unquoted()
      s = binding["at.relevant_skill.Skill"].unquoted()
      if "_relevant_skills" not in result[n][o]:
        result[n][o]["_relevant_skills"] = {}
      if a not in result[n][o]["_relevant_skills"]:
        result[n][o]["_relevant_skills"][a] = ([], [])
      result[n][o]["_relevant_skills"][a][0].append(s)

    elif sc == "relevant_skill_missing":
      n = binding["at.Node"].unquoted()
      o = binding["at.relevant_skill.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}

      a = binding["at.relevant_skill.actor.Key"].unquoted()
      s = binding["at.relevant_skill.Skill"].unquoted()
      if "_relevant_skills" not in result[n][o]:
        result[n][o]["_relevant_skills"] = {}
      if a not in result[n][o]["_relevant_skills"]:
        result[n][o]["_relevant_skills"][a] = ([], [])
      result[n][o]["_relevant_skills"][a][1].append(s)

    elif sc == "relevant_tool":
      n = binding["at.Node"].unquoted()
      o = binding["at.relevant_tool.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}

      a = binding["at.relevant_tool.actor.Key"].unquoted()
      t = binding["at.relevant_tool.item.Key"].unquoted()
      if "_relevant_tools" not in result[n][o]:
        result[n][o]["_relevant_tools"] = {}
      if a not in result[n][o]["_relevant_tools"]:
        result[n][o]["_relevant_tools"][a] = ([], [])
      result[n][o]["_relevant_tools"][a][0].append(t)

    elif sc == "relevant_tool_missing":
      n = binding["at.Node"].unquoted()
      o = binding["at.relevant_tool.option.Option"].unquoted()
      if n not in result:
        result[n] = {}
      if o not in result[n]:
        result[n][o] = {}

      a = binding["at.relevant_tool.actor.Key"].unquoted()
      s = binding["at.relevant_tool.Skill"].unquoted()
      if "_relevant_tools" not in result[n][o]:
        result[n][o]["_relevant_tools"] = {}
      if a not in result[n][o]["_relevant_tools"]:
        result[n][o]["_relevant_tools"][a] = ([], [])
      result[n][o]["_relevant_tools"][a][1].append(s)

    elif sc == "potential_state":
      n = binding["at.Node"].unquoted()
      if n not in result:
        result[n] = {}
      if "potentials" not in result[n]:
        result[n]["potentials"] = []

      pt = binding["at.potential.PType"]
      sn = binding["at.potential.state.SName"]
      si = binding["at.potential.state.Inst"]
      sb = ans.bind(INSTANCE_SCHEMA, si)
      if sb:
        v = sb["inst.Key"].unquoted()
      else:
        v = si.unquoted()
      result[n]["potentials"].append(
        {
          "_ptype": pt,
          "_stype": "state",
          "_sname": sn,
          "_inst": v,
        }
      )

    elif sc == "potential_property":
      n = binding["at.Node"].unquoted()
      if n not in result:
        result[n] = {}
      if "potentials" not in result[n]:
        result[n]["potentials"] = []

      pt = binding["at.potential.PType"]
      pn = binding["at.potential.property.PName"]
      pv = binding["at.potential.property.PVal"]
      pi = binding["at.potential.property.Inst"]
      sb = ans.bind(INSTANCE_SCHEMA, pi)
      if sb:
        v = sb["inst.Key"].unquoted()
      else:
        v = pi.unquoted()
      result[n]["potentials"].append(
        {
          "_ptype": pt,
          "_stype": "property",
          "_sname": pn,
          "_inst": v,
          "_value": pv
        }
      )

    elif sc == "potential_relation":
      n = binding["at.Node"].unquoted()
      if n not in result:
        result[n] = {}
      if "potentials" not in result[n]:
        result[n]["potentials"] = []

      pt = binding["at.potential.PType"]
      rn = binding["at.potential.relation.RName"]
      rf = binding["at.potential.relation.From"]
      sb = ans.bind(INSTANCE_SCHEMA, rf)
      if sb:
        vf = sb["inst.Key"].unquoted()
      else:
        vf = rf.unquoted()
      rt = binding["at.potential.relation.To"]
      sb = ans.bind(INSTANCE_SCHEMA, rt)
      if sb:
        vt = sb["inst.Key"].unquoted()
      else:
        vt = rt.unquoted()
      result[n]["potentials"].append(
        {
          "_ptype": pt,
          "_stype": "relation",
          "_sname": rn,
          "_from": vf,
          "_to": vt
        }
      )

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

def enhance_context_variables(cvrs, nouns):
  """
  Adds _type_of_{} variables to each set of context variables in the given
  context variables structure, using the given set of nouns.
  """
  for node in cvrs:
    if "setup" in cvrs[node]:
      enhance_vars(cvrs[node]["setup"], nouns)
    if "potentials" in cvrs[node]:
      for p in cvrs[node]["potentials"]:
        enhance_vars(p, nouns)
    for opt in [n for n in cvrs[node] if n != "setup" and n != "potentials"]:
      enhance_vars(cvrs[node][opt], nouns)

def enhance_vars(vs, nouns):
  """
  A helper for enhance_context_variables that enhances a flat dictionary of
  variables.
  """
  for key in [k for k in vs.keys() if not k.startswith("_type_of_")]:
    if type(vs[key]) == str and vs[key] in nouns:
      n = nouns[vs[key]]
      vs["_type_of_" + key] = n.cls

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
      preconditions = []
      for ln, line in enumerate(fin.readlines()):
        if mode == "lines":
          if not line.strip() or line.strip()[0] == "%":
            continue
          if line[0] == ':':
            key = line[1:-1]
            if key not in result:
              result[key] = []
            mode = "lines"
            preconditions = []
          elif line[0] == '{':
            if line[-2] != '}':
              raise ValueError("Invalid predicate filter:\n{}".format(line))
            fltr = line[1:-2]
            pcs = packrat.parse_completely(
              fltr,
              packrat.SepList(ans.SimpleTerm, sep=';')
            )
            preconditions = tuple(ans.build_fancy_schema(p) for p in pcs)
          elif line[0] == ">":
            key = line[1:-1]
            if key not in result:
              result[key] = []
            mode = "paragraph"
            para = ""
          elif key and line.strip():
            result[key].append((preconditions, line[:-1])) # no newline
          elif key == None and line.strip():
            raise ValueError(
              "{}:{} - Rule product has no key.".format(f,ln)
            )
        elif mode == "paragraph":
          if line == "<\n":
            if para[-1] == '\n':
              para = para[:-1]
            result[key].append((preconditions, para))
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
  Returns a boolean and a value for the '_' variable bound to the value of the
  variadic key at the last place where the concrete key contained a '?', or the
  empty string if the concrete key contained no '?'s.
  """
  tp = test.split('/')
  kp = key.split('/')
  underscore = ""
  if len(kp) > len(tp):
    return False, ""
  for i, k in enumerate(kp):
    if tp[i] != k and tp[i] != '?' and k != '?':
      if i == len(kp) - 1 and k == '*':
        continue
      return False, ""
    elif tp[i] == '?':
      underscore = k
  if len(kp) < len(tp) and kp[-1] != '*':
    return False, ""
  return True, underscore

def subst_vars(text, vs):
  """
  Given a text with some variable substitutions to be made returns the result
  text after one round of argument substitutions.
  """
  result = ""
  bits = re.split(VAR, text)
  for b in bits:
    add = b
    m = VAR.fullmatch(b)
    if m:
      var = m.group(1)[1:] # take off the initial '?'
      if var in vs:
        if type(vs[var]) == list:
          add = ' '.join(vs[var])
        elif type(vs[var]) == str:
          add = vs[var]
        elif isinstance(vs[var], ans.Predicate):
          add = str(vs[var])
        else:
          add = "ERROR: variable '{}' has weird type {}".format(var, type(var))
      else:
        add = "ERROR: unknown variable '{}'".format(var)
    result += add
  return result

def subst_all_vars(text, vs):
  """
  Recursively perform variable substitutions until no variables are left to
  expand.
  """
  while VAR.search(text):
    text = subst_vars(text, vs)
  return text


def decontextualized_binding(binding, vs):
  result = {}
  for key in binding:
    val = binding[key].unquoted()
    dk = '_' + key.split('.')[-1]
    if (dk in result and result[dk] != binding[key]):
      return None
    if dk in SPECIAL_FILTERING_VARIABLES:
      if vs[SPECIAL_FILTERING_VARIABLES[dk]] != val:
        return None
    result[dk] = val
  return result

def binding_is_consistent(world, binding):
  return not any(key in binding and world[key] != binding[key] for key in world)

def precondition_possibilities(story, preconditions, vs):
  possibilities = [{}]

  for pre in preconditions:
    hits = []
    nextworlds = []

    # all possible bindings of this schema:
    for fact in story:
      b = ans.bind(pre, fact)
      if b:
        db = decontextualized_binding(b, vs)
        if db:
          hits.append(db)

    # for each hit, extend each possibility that that hit is consistent with:
    for h in hits:
      for p in possibilities:
        if binding_is_consistent(p, h):
          merged = {}
          merged.update(p)
          merged.update(h)
          nextworlds.append(merged)

    if not nextworlds:
      return []

    possibilities = nextworlds

  return possibilities

class Substitution:
  def __init__(self, flags='', key='', vs=None):
    self.flags = flags
    self.key = key
    self.vs = vs or {}

  def expand(self, rules, vs, story):
    """
    Given a set of substitution rules, returns the substitution result for this
    Substitution object.
    """
    if self.key.split('/')[0] == "special":
      return self.special_expand(rules, vs)
    matching_keys = []
    for k in rules:
      matches, underscore = keymatch(k, self.key)
      if matches:
        matching_keys.append((k, underscore))
    all_possibilities = []
    for (ps, u) in [(rules[k], u) for (k, u) in matching_keys]:

      for precond, expand in ps: # for each possible expansion
        prvars = {'_': u}
        # for each possible binding over preconditions of this expansion:
        for possibility in precondition_possibilities(
          story,
          precond,
          vs
        ):
          ps = {}
          ps.update(prvars)
          ps.update(possibility)
          all_possibilities.append((expand, ps))

    # TODO: better/controllable randomness?
    if (len(all_possibilities) == 0):
      raise KeyError("No possible substitutions for key '{}'.".format(self.key))
    result, underscore = random.choice(all_possibilities)
    if 'S' in self.flags:
      result = "@CAP@" + sentence(result)
    return result, underscore

  def special_expand(self, rules, vs):
    """
    Performs a special expansion function.
    """
    if self.key.split('/')[0] != "special":
      raise KeyError("Special expand called with invalid key.")
    function = self.key.split('/')[1:]
    if function == ["and_list"]:
      if "list" not in vs:
        raise ValueError("and_list function: could not find variable ?list.")
      if vs["list"] not in vs:
        raise ValueError(
          "and_list function: ?list '{}' not found.".format(vs["list"])
        )
      l = vs[vs["list"]]
      if type(l) != list:
        raise ValueError("and_list function: invalid ?list contents.")
      if "tmpl_prefix" in vs:
        l = ["[[{}/{}]]".format(vs["tmpl_prefix"], x) for x in l]
      if len(l) == 0:
        return ("nothing", "")
      if len(l) == 1:
        return (l[0], "")
      elif len(l) == 2:
        return (l[0] + " and " + l[1], "")
      else:
        return (l[0] + ", " + ", ".join(l[1:-1]) + ", and " + l[-1], "")
    else:
      raise KeyError(
        "Unknown special function '{}'.".format('/'.join(function))
      )

def parse_kv(text):
  """
  Parses a substitution key/value pair off of the front of the given text, and
  returns the pair as a tuple followed by any leftover text (which will either
  be empty or start with an '@').
  """
  if text[0] != '@':
    return None
  text = text[1:]
  km = SBST_KEY.match(text)
  if not km:
    return None
  key = km.group(1)
  leftovers = text[km.end():]
  value = ""
  depth = 0
  while leftovers:
    m = KV_TOKENS.match(leftovers)
    cycle = 1
    if not m:
      pass
    elif m.group(1) and depth == 0: # '@'
      break
    elif m.group(2): # '\.'
      value += m.group(2)[1]
      leftovers = leftovers[2:]
      continue
    elif m.group(3): # '[['
      cycle = 2
      depth += 1
    elif m.group(4): # ']]'
      cycle = 2
      depth -= 1
    value += leftovers[:cycle]
    leftovers = leftovers[cycle:]
  return ( key, value ), leftovers

def parse_initial_substituion(text):
  """
  Tries to parse the start of "text" as a Substitution and return a tuple
  (Substitution, text). Returns None if it can't do so.
  """
  if text[:2] != '[[':
    return None
  text = text[2:]
  depth = 1
  found = False
  end = -1
  while end < len(text) - 1:
    end += 1
    if text[end:end+2] == '[[':
      end += 1
      depth += 1
    elif text[end:end+2] == ']]':
      depth -= 1
      if depth == 0:
        found = True
        break
      else:
        end += 1
  if not found:
    return None

  sbst = text[:end]
  leftovers = text[end + 2:]

  m = SBST_FLAGS.match(sbst)
  if m:
    flags = m.group(1)
    sbst = sbst[m.end():]
  else:
    flags = ''

  if '@' in sbst:
    keyend = sbst.index('@')
  else:
    keyend = len(sbst)

  key = sbst[:keyend]
  sbst = sbst[keyend:]

  vs = {}
  while sbst:
    pkvr = parse_kv(sbst)
    if pkvr == None:
      return None
    kv, sbst = pkvr
    vs[kv[0]] = kv[1]

  return Substitution(flags, key, vs), leftovers

def parse_substitutions(text):
  """
  Parses flat text into a list of string and Substitution objects.
  """
  result = ['']
  while text:
    if text[:2] == '[[':
      attempt = parse_initial_substituion(text)
      if attempt == None:
        result[-1] += text[:2]
        text = text[2:]
      else:
        sbst, text = attempt
        result.append(sbst)
        result.append('')
    else:
      result[-1] += text[0]
      text = text[1:]
  return result

def run_grammar(text, rules, vs, story):
  """
  Runs the given grammar rules of the text, expanding it recursively as
  necessary, using the given variable mappings and story predicates.
  """
  # First exhaust variable substitutions:
  text = subst_all_vars(text, vs)
  # Next recursively substitute rule results:
  result = ""
  bits = parse_substitutions(text)
  for b in bits:
    if isinstance(b, Substitution):
      newvars = dict(vs)
      newvars.update(b.vs)
      expansion, exvars = b.expand(rules, newvars, story)
      newvars.update(exvars)
      result += run_grammar(expansion, rules, newvars, story)
    else:
      result += b
  return result

def build_text(
  template,
  rules,
  cvars,
  story,
  ndict,
  base_pnslots=None,
  introduced=None,
  timeshift=None,
  fmt="twee"
):
  """
  Takes a text template and builds a filled-in string using the given rules,
  variables, and nouns along with pronoun and noun usage information. If a
  timeshift is given it should be either "past" or "future" and it will be
  applied to all the verbs in the text. Returns a tuple of the constructed
  string and the resulting rules, pronoun slots, and noun introduction.
  """
  tsstack = [timeshift]
  if base_pnslots == None:
    pnslots = {
      "I": [0, set()],
      "we": [0, set()],
      "you": [0, set()],
      "he/she": [0, set()],
      "it": [0, set()],
      "they": [0, set()],
    }
  else:
    pnslots = copy.deepcopy(base_pnslots)
  if introduced == None:
    introduced = set()
  else:
    introduced = set(introduced)
  # First, recursively perform variable and rule substitutions until we've
  # reached a base text:
  template = run_grammar(template, rules, cvars, story)
  # Next, fill in any tags:
  result = template
  prev_result = ""
  while ANYTAG.search(result):
    if result == prev_result:
      print("ERROR: nothing changed despite presence of a tag.")
      print("Stuck at:")
      print(result)
      exit(1)
    bits = re.split(ANYTAG, result)
    prev_result = result
    result = ""
    for b in bits:
      add = b
      if '.' in b: # TODO: Better sentence-counting! (?)
        # there's the end of a sentence somewhere in this bit: clean out
        # all slots
        pnslots = copy.deepcopy(base_pnslots)
        # TODO: Fix this!!
      for t in TAGS:
        m = TAGS[t].fullmatch(b)
        if m:
          if t == "directive":
            directive = m.group(1)
            argument = m.group(2)
            if directive == "timeshift":
              argument = argument.replace('_', ' ')
              if argument in verbs.TIMESHIFTS:
                tsstack.append(argument)
              elif argument == "pop":
                if len(tsstack) > 1:
                  tsstack = tsstack[:-1]
                else:
                  raise RuntimeError(
                    "Popped last timeshift from timeshift stack."
                  )
              else:
                raise ValueError("Unknown timeshift '{}'.".format(argument))
            else:
              raise ValueError("Unknown directive '{}'.".format(directive))
            add = ""

          elif t == "noun":
            noun = m.group(1)
            if noun not in ndict:
              raise KeyError("Noun '{}' not in ndict.".format(noun))
            pro = m.group(2)
            nopro = False
            if pro[0] == '_':
              nopro = True
              pro = pro[1:]
            case, position = nouns.casepos(pro)
            slot = pnslots[nouns.pnslot(ndict[noun])]
            if (
              not nopro
            and
              slot[0] <= MAX_PRONOUN_AGE
            and
              noun in slot[1]
            and
              len(slot[1]) == 1
            ):
              slot[0] = 0
              add = nouns.pronoun(ndict[noun], case, position)
              # TODO: Gender introduction awareness!
            else:
              if slot[0] > 0:
                slot[0] = 0
                slot[1] = { noun }
              else:
                slot[1].add(noun)
              if noun in introduced:
                add = nouns.definite(ndict[noun], case)
              else:
                introduced.add(noun)
                add = nouns.indefinite(ndict[noun], case)

            # we saw a noun: increment all third-person pnslots age counters
            # except for the counter for the noun we saw:
            for sl in ["he/she", "it", "they"]:
              if pnslots[sl] != slot:
                pnslots[sl][0] += 1

          elif t == "verb":
            verb = m.group(1)
            tense = TSABR[m.group(2)]
            agree = m.group(3)
            if agree == "_plural":
              add = verbs.conjugation(
                verb,
                tense,
                "plural",
                "third",
                tsstack[-1]
              )
            elif agree == "_singular":
              add = verbs.conjugation(
                verb,
                tense,
                "singular",
                "third",
                tsstack[-1]
              )
            else:
              add = verbs.conj_ref(ndict[agree], verb, tense, tsstack[-1])
          # if we matched a tag, don't bother checking the other tags:
          break
      result += add
  # Finally process directives:
  if fmt == "choicescript":
    result = re.sub(PAR, '\n\n', result)
  elif fmt == "turk":
    result = re.sub(PAR, ' ', result)
  elif fmt == "example":
    result = re.sub(PAR, '\n\n', result)
    result = re.sub('\n\n', '<br/>', result)
  elif fmt == "twee":
    result = re.sub(PAR, '<br/><br/>', result)
  result = re.sub(BREAK, '', result)
  result = re.sub(CAP, lambda m: m.group(1).upper(), result)
  return result, pnslots, introduced

def find_node_structure(story, nodelist):
  """
  Takes a story and looks at successor/3 predicates to determine the structure
  of nodes in the story, ignoring nodes not on the given nodelist, and
  returning a dictionary that maps node names to both successor and predecessor
  entries: successor entries being option->node mappings and predecessor
  entries being a list of nodes that have this node as a successor.
  """
  result = {}
  for node in nodelist:
    result[node] = {"successors":{}, "predecessors":[]}
  for pr in story:
    scc = ans.bind(SUCCESSOR, pr)
    if scc:
      frm = scc["successor.From"].unquoted()
      opt = scc["successor.option.Opt"].unquoted()
      to = scc["successor.To"].unquoted()
      if frm not in nodelist or to not in nodelist:
        continue
      result[frm]["successors"][opt] = to
      result[to]["predecessors"].append(frm)
  return result

def build_node_text(
  node,
  node_structure,
  grammar_rules,
  context_variables,
  story,
  nouns,
  pnslots,
  introduced,
  timeshift=None,
  fmt="twee"
):
  """
  Builds text for the given node (should be a dictionary from the
  node_templates map). Returns the resulting text and a dictionary mapping
  options to their outgoing (pnslots, introduced) tuples. If a timeshift is
  given, it should be either "past" or "future" and will be applied to all
  verbs in the text generated.
  """
  outgoing = {}
  intro, _pnslots, _introduced = build_text(
    node["intro"],
    grammar_rules,
    context_variables["setup"] if "setup" in context_variables else {},
    story,
    nouns,
    pnslots,
    introduced,
    timeshift,
    fmt
  )
  intro = sentence(intro)
  situation = ""
  for i, stmpl in enumerate(node["situation"]):
    stext, _pnslots, _introduced = build_text(
      stmpl,
      grammar_rules,
      context_variables["potentials"][i],
      story,
      nouns,
      _pnslots,
      _introduced,
      timeshift,
      fmt
    )
    if situation:
      situation += " and " + stext
    else:
      situation = stext
  situation = sentence(situation)
  options = ""
  if fmt == "example":
    options += "<ol>"
  ocount = len(node["options"])
  if ocount == 1:
    opt = list(node["options"].keys())[0]
    txt, pnout, intout = build_text(
      node["options"][opt],
      grammar_rules,
      context_variables[opt],
      story,
      nouns,
      _pnslots,
      _introduced,
      timeshift,
      fmt
    )
    if fmt == "example":
      options += "\n  <li>{}</li>".format(sentence(txt))
      # In the "example" format we're just concerned with formatting the
      # options for a single node, ignoring successors and outcomes.
    elif fmt == "turk":
      options += "{}<snop>".format(sentence(txt))
      # The same is true of the "turk" format...
    else:
      options += '\n' + sentence(txt)
      txt, pnout, intout = build_text(
        node["outcomes"][opt],
        grammar_rules,
        context_variables[opt],
        story,
        nouns,
        pnout,
        intout,
        timeshift,
        fmt
      )
      options += '\n' + sentence(txt)
      successors = node_structure[node["name"]]["successors"]
      if opt in successors:
        scc = successors[opt]
        outgoing[scc] = (pnout, intout)
        if fmt == "choicescript":
          options += "\n*goto {}\n".format(scc)
        elif fmt == "twee":
          options += "\n[[Onwards...|{}][$outcome='']]\n".format(scc)
        elif fmt == "example":
          options += "\nOnwards...\n"
        elif fmt == "turk":
          # we don't care about this
          pass
      else:
        # TODO: Epilogue here!
        if fmt == "choicescript":
          options += "\n*finish\n"
  elif ocount > 1:
    if fmt == "choicescript":
      options = "*choice\n"
    for opt in node["options"]:
      opt_txt, pnout, intout = build_text(
        node["options"][opt],
        grammar_rules,
        context_variables[opt],
        story,
        nouns,
        _pnslots,
        _introduced,
        timeshift,
        fmt
      )
      out_txt, pnout, intout = build_text(
        node["outcomes"][opt],
        grammar_rules,
        context_variables[opt],
        story,
        nouns,
        pnout,
        intout,
        timeshift,
        fmt
      )
      successors = node_structure[node["name"]]["successors"]
      scc = None
      if opt in successors:
        scc = successors[opt]
        outgoing[scc] = (pnout, intout)
      if fmt == "choicescript":
        options += "  #{}\n".format(sentence(opt_txt))
        options += "    {}\n".format(sentence(out_txt))
        if scc:
          options += "    *goto {}\n".format(scc)
        else:
          options += "    *finish\n"
      elif fmt == "twee":
        if scc:
          options += '[[{opt}|{scc}][$outcome="{out}"]]\n'.format(
            opt=sentence(opt_txt),
            out=sentence(out_txt),
            scc=scc
          )
        else:
          options += '[[{opt}|End][$outcome="{out}"]]\n'.format(
            opt=sentence(opt_txt),
            out=sentence(out_txt)
          )
      elif fmt == "example":
        options += "\n  <li>{}</li>".format(sentence(opt_txt))
      elif fmt == "turk":
        options += "{}<snop>".format(sentence(opt_txt))
  else:
    options = "ERROR: node {} had <= 0 options!"
  if fmt == "choicescript":
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
  elif fmt == "twee":
    result = """
:: {label}
<<if $outcome neq "">><<print $outcome>> <<endif>>\
{intro}
{situation}
{options}
""".format(
  label=node["name"],
  intro=intro,
  situation=situation,
  options=options
)
  elif fmt == "example":
    options += "\n</ol>"
    result = """
<p>
{intro}
</p>
<p>
{situation}
</p>
{options}
""".format(
  label=node["name"],
  intro=intro,
  situation=situation,
  options=options
)
  elif fmt == "turk":
    result = """
{intro}
<snip>
{situation}
<snip>
{options}
""".format(
  label=node["name"],
  intro=intro,
  situation=situation,
  options=options
)
  return result, outgoing

def build_intro_text(
  template,
  grammar_rules,
  context_variables,
  story,
  nouns,
  pnslots,
  introduced,
  timeshift=None,
  fmt="twee"
):
  result, _pnslots, introduced = build_text(
    template,
    grammar_rules,
    context_variables,
    story,
    nouns,
    pnslots,
    introduced,
    timeshift,
    fmt
  )
  return result, introduced

def build_story_text(story, mode="full", timeshift=None, fmt="twee"):
  node_templates = {}
  gr_rules = collate_rules(story)
  story_parts = {
    "framing": "ERROR: missing story part",
    "assets": "ERROR: missing story part",
    "set_off": "ERROR: missing story part",
  }
  if mode == "example":
    story_parts["setup"] = "ERROR: missing story part"
    story_parts["potentials"] = "ERROR: missing story part"
    story_parts["prompt"] = "ERROR: missing story part"
    story_parts["optlist"] = "ERROR: missing story part"

  # First, build the dictionary of templates for all polished nodes:
  for sc, bnd in ans.bindings(
    { "_": Pr("node_status_reached", Vr("Node"), Pr("polished")) },
    story
  ):
    node = bnd["node_status_reached.Node"].unquoted()
    node_templates[node] = {
      "name": node,
      "intro": "",
      "situation": [],
      "options": {},
      "outcomes": {},
      # TODO: state-change text
    }

  # Next find context variables which also gives us an idea of the story
  # structure, as well as the nouns:
  cvrs = glean_context_variables(story)
  nouns = glean_nouns(story)
  introvars = glean_intro_variables(story, nouns)
  enhance_context_variables(cvrs, nouns)

  # Error check:
  for k in [key for key in cvrs if key not in node_templates]:
    print("WARNING: Node '{}' has context but isn't polished.".format(k))
  for k in [key for key in node_templates if key not in cvrs]:
    print("WARNING: Node '{}' is polished, but has no context.".format(k))

  # Iterate over our nodes and options adding text templates:
  for node in node_templates:
    if "setup" in cvrs[node]:
      node_templates[node]["intro"] = "[[setup/{}/intro]]".format(
        cvrs[node]["setup"]["_setup"]
      )
    if "potentials" in cvrs[node]:
      for p in cvrs[node]["potentials"]:
        node_templates[node]["situation"].append(
          "[[potential/{}/{}]]".format(p["_stype"], p["_sname"])
        )
    options = [o for o in cvrs[node] if o != "setup" and o != "potentials"]
    ocount = len(options)
    for option in options:
      nts = node_templates[node]
      ovrs = cvrs[node][option]
      # If a party member is the initiator of an option at a choice...
      initiator = ovrs["_initiator"]

      if (
        initiator != "unknown"
       and nouns[initiator].is_party_member
       and len(options) > 1
       and initiator != "you"
      ):
        nts["options"][option] = \
            "[[misc/you_ask_for@statement=[[action/?_action/option]]]]"
      else:
        nts["options"][option] = "[[action/?_action/option]]"

      nts["outcomes"][option] = "[[S|action/?_action/outcome]]"

      # If this is a choice, add skill/tool info:
      if ocount > 1:
        rlist = []
        if "_relevant_skills" in ovrs:
          rsks = ovrs["_relevant_skills"]
          for actor in rsks:
            glist = []
            blist = []
            for skill in rsks[actor][0]:
              glist.append(
                "[[misc/is_skilled/{}@who={}]]".format(skill, actor)
              )
            for skill in rsks[actor][1]:
              blist.append(
                "[[misc/is_unskilled/{}@who={}]]".format(skill, actor)
              )
            if glist and blist:
              rlist.append(
                ", and ".join(glist) + ", but " + ", and ".join(blist)
              )
            elif glist:
              rlist.append(", and ".join(glist))
            elif blist:
              rlist.append(", and ".join(blist))

        if "_relevant_tools" in ovrs:
          rtls = ovrs["_relevant_tools"]
          for actor in rtls:
            relevant = ""
            if rtls[actor][0]:
              relevant += "N#{}/they V#have/prs/{} ".format(
                actor,
                actor
              ) + ", and ".join(
                "N#{}/them".format(tool) for tool in rtls[actor][0],
              )
              if rtls[actor][1]:
                relevant += " but "
            if rtls[actor][1]:
              relevant += "N#{}/they V#have/prs/{} no ".format(
                actor,
                actor
              ) + ", nor any ".join(
                "tool for {}".format(skill) for skill in rtls[actor][1],
              )
            if relevant:
              rlist.append(relevant)

        if rlist:
          nts["options"][option] += " (@CAP@{})".format(
            ". @CAP@".join(rlist)
          )
        else:
          nts["options"][option] += " (no relevant skills)"

  # Next, use the node structure to recursively render the story text in
  # ChoiceScript:
  node_structure = find_node_structure(story, node_templates.keys())
  base_pnslots = {
    "I": [0, set()],
    "we": [0, set()],
    "you": [0, { "you" }],
    "he/she": [0, set()],
    "it": [0, set()],
    "they": [0, set()],
  }
  # You and your party members start as 'introduced'
  base_introduced = { "you" } | { n for n in nouns if nouns[n].is_party_member }
  # Generate the intro text
  if mode == "full":
    intro_text, base_introduced = build_intro_text(
      "[[prologue/{}]]".format(mode),
      gr_rules,
      introvars,
      story,
      nouns,
      base_pnslots,
      base_introduced,
      timeshift,
      fmt
    )
    if fmt == "twee":
      # TODO: Make title/author/css/etc. parameters!
      intro_text = """\
:: Intro
{}

[[Begin your journey...|root]]
""".format(intro_text.replace('\n', ' '))
    elif fmt == "example":
      intro_text = intro_text.replace('\n', '<br/>\n')
    elif fmt == "turk":
      intro_text = re.sub(PAR, '\n', intro_text)
      framing, assets, set_off = intro_text.split('\n')
  elif mode == "example":
    story_parts["framing"], base_introduced = build_intro_text(
      "[[prologue/setting/{}]]".format(mode),
      gr_rules,
      introvars,
      story,
      nouns,
      base_pnslots,
      base_introduced,
      timeshift,
      fmt
    )
    story_parts["assets"], base_introduced = build_intro_text(
      "[[prologue/assets]]",
      gr_rules,
      introvars,
      story,
      nouns,
      base_pnslots,
      base_introduced,
      timeshift,
      fmt
    )
    story_parts["set_off"], base_introduced = build_intro_text(
      "[[prologue/get_started/{}]]".format(mode),
      gr_rules,
      introvars,
      story,
      nouns,
      base_pnslots,
      base_introduced,
      timeshift,
      fmt
    )
    if fmt == "twee":
      intro_text = """\
:: Intro
{framing}
{assets}
{set_off}

<<display root>>
""".format(
  framing=story_parts["framing"].replace('\n', ' '),
  assets=story_parts["assets"].replace('\n', ' '),
  set_off=story_parts["set_off"].replace('\n', ' '),
)
    elif fmt == "example":
      story_parts["framing"] = story_parts["framing"].replace('\n', '<br/>\n')
      story_parts["assets"] = story_parts["assets"].replace('\n', '<br/>\n')
      story_parts["set_off"] = story_parts["set_off"].replace('\n', '<br/>\n')
      intro_text = """\
<p>{framing}</p>
<p>{assets}</p>
<p>{set_off}</p>
""".format(
  framing=story_parts["framing"].replace('\n', ' '),
  assets=story_parts["assets"].replace('\n', ' '),
  set_off=story_parts["set_off"].replace('\n', ' '),
)
    else:
      intro_text = '\n\n'.join(
        [
          story_parts["framing"],
          story_parts["assets"],
          story_parts["set_off"]
        ]
      )
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
  results = [intro_text]
  while olist:
    target, pnslots, introduced = olist.pop(0)
    print("Building text for node '{}'".format(target))
    # build node text:
    txt, outgoing = build_node_text(
      node_templates[target],
      node_structure,
      gr_rules,
      cvrs[target],
      story,
      nouns,
      pnslots,
      introduced,
      timeshift,
      fmt=fmt
    )
    if mode == "example" and story_parts["setup"]=="ERROR: missing story part":
      if fmt == "example":
        spliton = '</p>'
      elif fmt == "turk":
        spliton = '<snip>'
      else:
        spliton = '  '
      bits = txt.replace('\n', ' ').split(spliton)
      story_parts["setup"] = bits[0]
      story_parts["potentials"] = bits[1]
      story_parts["prompt"] = "What do you do?"
      story_parts["optlist"] = bits[2]
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
  if fmt == "choicescript":
    return ("\n\n*comment " + '-'*72 + "\n\n").join(results)
  elif fmt == "example":
    body = "\n\n".join(results)
    return """
<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Example Story Choice</title>
<style id="baseCSS">
</style>
</head>
<body>
<h1>Example Story Choice</h1>
{body}
</body>
</html>
""".format(body=body)
  elif fmt == "turk":
    # a tab-delimited input row:
    opts = story_parts["optlist"].split("<snop>")
    return """\
"{framing}","{assets}","{set_off}","{setup}","{potentials}","{prompt}","{opt1}","{opt2}","{opt3}"
""".format(
  framing=story_parts["framing"],
  assets=story_parts["assets"],
  set_off=story_parts["set_off"],
  setup=story_parts["setup"],
  potentials=story_parts["potentials"],
  prompt=story_parts["prompt"],
  opt1=opts[0],
  opt2=opts[1],
  opt3=opts[2],
)
  else:
    return ("\n\n").join(results)

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
  (
    run_grammar,
    (
      "template.. [[S|a/b]]",
      { "a/b": [ ((), "sentence substitution test") ] },
      {},
      set(),
    ),
    "template.. @CAP@Sentence substitution test."
  ),
  (
    run_grammar,
    (
      "variable.. ?var",
      {},
      { "var": "val"},
      set(),
    ),
    "variable.. val"
  ),
  (
    run_grammar,
    (
      "nested.. ?var [[S|test@var=[[test3@var=[[test4@var=stop]]]]]][[test2]]",
      {
        "test":  [((), "subst1 ?var") ],
        "test2": [((), "subst2 ?var") ],
        "test3": [((), "subst3 ?var") ],
        "test4": [((), "subst4 ?var") ],
      },
      { "var": "initial"},
      set(),
    ),
    "nested.. initial @CAP@Subst1 subst3 subst4 stop.subst2 initial"
  ),
  (
    run_grammar,
    (
      "[[setup/monster_attack/approach/leviathan]]",
      {
        "setup/monster_attack/approach/?": [ ((), "?monster test") ]
      },
      { "monster": "leviathan_17"},
      set(),
    ),
    "leviathan_17 test"
  ),
  (
    run_grammar,
    (
      "[[setup/monster_attack/approach/leviathan@adverb=majestically]]",
      {
        "setup/monster_attack/approach/leviathan":
          [ ((), "?adverb ?monster test") ]
      },
      { "monster": "leviathan_17"},
      set(),
    ),
    "majestically leviathan_17 test"
  ),
  (
    run_grammar,
    (
      "[[setup/monster_attack/approach/leviathan@adverb=majestically@monster=vv]]",
      {
        "setup/monster_attack/approach/leviathan":
          [ ((), "?adverb ?monster test") ]
      },
      { "monster": "leviathan_17"},
      set(),
    ),
    "majestically vv test"
  ),
  (
    run_grammar,
    (
      "[[setup/monster_attack/approach/leviathan@adverb=majestically@activity=tentacles curling]]",
      {
        "setup/monster_attack/approach/leviathan":
          [ ((), "?adverb ?activity ?monster test") ]
      },
      { "monster": "leviathan_17"},
      set(),
    ),
    "majestically tentacles curling leviathan_17 test"
  ),
  (
    run_grammar,
    (
      "[[test]] [[t2]]",
      {
        "test":
          [
            (
              ( Pr("test", Vr("A1"), Vr("A2")), ),
              "test ?_A1 ?_A2"
            )
          ],
        "t2":
          [
            (
              ( Vr("U", Pr("gosh")), ),
              "t2 ?_U gosh"
            )
          ],
      },
      {},
      {
        Pr("test", Pr("foo"), Pr("bar")),
        Pr("t2", Pr("gosh"))
      },
    ),
    "test foo bar t2 t2(gosh) gosh"
  ),
]
