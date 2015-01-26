"""
viz.py
"""

import os, subprocess

import ans

from ans import Pr, Vr, PVr, SbT

VIZ_GV = os.path.join("out", "viz.gv")
VIZ_SVG = os.path.join("out", "viz.svg")

SCHEMAS = {
  "story_node": PVr("story_node", "story_node", Vr("Node")),
  "action":
    PVr(
      "at", "at",
      Vr("Node"),
      Pr("action", Pr("option", Vr("Opt")), Vr("Action"))
    ),
  "node_type": PVr("node_type", "node_type", Vr("Node"), Vr("Type")),
  "node_status_reached":
    PVr("node_status_reached", "node_status_reached", Vr("Node"), Vr("Status")),
  "successor":
    PVr(
      "successor", "successor",
      Vr("From"),
      Pr("option", Vr("Opt")),
      Vr("To"),
    ),

  "vignette": PVr("vignette", "vignette", Vr("Node"), Vr("Root")),

  "setup": PVr("setup", "setup", Vr("Node"), Vr("Which")),
}

NODE_STATUSES = [
  "uninitialized",
  "initialized",
  "built",
  "branched",
  "polished",
]

def viz(story):
  """
  Visualize the given set of predicates (writes out to a file).
  """
  nodes = set()
  node_properties = {}
  links = {}
  for sch, bnd in ans.bindings(SCHEMAS, story):
    if sch == "story_node":
      node = bnd["story_node.Node"].unquoted()
      nodes.add(node)
      if node not in node_properties:
        node_properties[node] = {}
    elif sch == "node_type":
      node = bnd["node_type.Node"].unquoted()
      typ = bnd["node_type.Type"].unquoted()
      nodes.add(node)
      if node not in node_properties:
        node_properties[node] = {}
      node_properties[node]["type"] = typ
    elif sch == "node_status_reached":
      node = bnd["node_status_reached.Node"].unquoted()
      status = bnd["node_status_reached.Status"].unquoted()
      nodes.add(node)
      if node not in node_properties:
        node_properties[node] = {}
      if (
        "status" not in node_properties[node]
      ) or (
        NODE_STATUSES.index(node_properties[node]["status"])
      <
        NODE_STATUSES.index(status)
      ):
        node_properties[node]["status"] = status
    elif sch == "successor":
      src = bnd["successor.From"].unquoted()
      opt = bnd["successor.option.Opt"].unquoted()
      dst = bnd["successor.To"].unquoted()
      if src not in links:
        links[src] = {}
      links[src][opt] = dst
    elif sch == "action":
      src = bnd["at.Node"].unquoted()
      opt = bnd["at.action.option.Opt"].unquoted()
      act = bnd["at.action.Action"].unquoted()
      if src not in links:
        links[src] = {}
      if "actions" not in links[src]:
        links[src]["actions"] = {}
      links[src]["actions"][opt] = act
    elif sch == "vignette":
      node = bnd["vignette.Node"].unquoted()
      root = bnd["vignette.Root"].unquoted()
      nodes.add(node)
      if node not in node_properties:
        node_properties[node] = {}
      node_properties[node]["vignette"] = root
    elif sch == "setup":
      node = bnd["setup.Node"].unquoted()
      setup = bnd["setup.Which"].unquoted()
      nodes.add(node)
      if node not in node_properties:
        node_properties[node] = {}
      node_properties[node]["setup"] = setup

    # Write out the graphviz graph:
    gv = """\
digraph "story" {
  graph [
    fontname = "TeXGyre-Pagella",
    fontsize = 12
  ];
"""
    for node in nodes:

      if "status" not in node_properties[node]:
        color = "gray"
      elif node_properties[node]["status"] == "uninitialized":
        color = "red"
      elif node_properties[node]["status"] == "polished":
        color = "green"
      else:
        color = "yellow"

      gvattrs = ['color = {}'.format(color),]

      if "type" not in node_properties[node]:
        shape = "polygon"
      elif node_properties[node]["type"] == "event":
        shape = "box"
      elif node_properties[node]["type"] == "choice":
        shape = "oval"
      elif node_properties[node]["type"] == "ending":
        shape = "invtriangle"
      gvattrs.append('shape = {}'.format(shape))

      if "setup" in node_properties[node]:
        gvattrs.append(
          'label = "s:{}"'.format(node_properties[node]["setup"])
        )
      gv += '  "{}" [{}];\n'.format(node, ', '.join(gvattrs))

    for src in links:
      if src not in nodes:
        continue
      for opt in [o for o in links[src] if o != "actions"]:
        dst = links[src][opt]
        if dst not in nodes:
          continue
        if "actions" in links[src] and opt in links[src]["actions"]:
          act = links[src]["actions"][opt]
        else:
          act = "unknown"
        linkattrs = ['label = "{}"'.format(act)]
        if (
          node_properties[src].get("vignette")
        and (
            node_properties[src].get("vignette")
          ==
            node_properties[dst].get("vignette")
          )
        ):
          linkattrs.append('style = bold')
        gvattrs = ''
        if linkattrs:
          gvattrs = ' [{}]'.format(', '.join(linkattrs))
        gv += '  "{}" -> "{}"{};\n'.format(src, dst, gvattrs)
  gv += "}"
  with open(VIZ_GV, 'w') as fout:
    fout.write(gv)
  subprocess.call(["dot", VIZ_GV, "-Tsvg", "-o", VIZ_SVG])
