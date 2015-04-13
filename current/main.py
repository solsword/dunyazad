#!/usr/bin/env python3
"""
main.py
The main file for story generation.
"""

import os
import os.path
import subprocess

import sys

import asp
import ans
import parser
import tasks
import viz
import english

from utils import *

CRASHFILE = os.path.join("out", "crash.lp")

def write_crashfile(e):
  with open(CRASHFILE, 'w') as fout:
    fout.write('% stdout:\n%' + '\n%'.join(e.stdout.split('\n')))
    fout.write('% stderr:\n%' + '\n%'.join(e.stderr.split('\n')))
    fout.write(e.program)

def main(
  storyfile = None,
  scaffoldfiles = None,
  scaffoldfrags = None,
  nodelimit = 12,
  fmt="twee"
):
  story = []
  sofar = []
  scaffolding = ""
  error = False
  if not os.path.isdir("out"):
    os.mkdir("out")
  if not os.path.isdir(os.path.join("out", "snapshots")):
    os.mkdir(os.path.join("out", "snapshots"))

  if scaffoldfiles:
    for sf in scaffoldfiles:
      with open(sf, 'r') as fin:
        scaffolding += (
          tasks.SEP + "% Scaffold file '{}' start.".format(sf) + tasks.SEP
        + fin.read()
        + tasks.SEP + "% Scaffold file '{}' end.".format(sf) + tasks.SEP
        )

  if scaffoldfrags:
    for fr in scaffoldfrags:
      scaffolding += (
        tasks.SEP + "% Scaffold fragment '{}' start.".format(fr) + tasks.SEP
      + tasks.fr(fr)
      + tasks.SEP + "% Scaffold fragment '{}' end.".format(fr) + tasks.SEP
      )

  target = "unknown"
  if storyfile:
    print("Parsing existing story file '{}'...".format(storyfile))
    with open(storyfile, 'r') as fin:
      # TODO: parse faster here!
      sofar = list(ans.parse_fans_fast(fin.read()))
    print("  ...done.")
  else:
    try:
      program, setup = tasks.setup_story([], scaffolding)
      story.extend(setup)
      with open(os.path.join("out", "snapshots", "prog-setup.lp"), 'w') as fout:
        fout.write(program)
    except asp.ASPError as e:
      print(
        "Error during setup: {}. Dumping source to '{}'".format(
          e.message,
          CRASHFILE
        )
      )
      write_crashfile(e)
      return False
 
    n = -1
    sofar = story
    keepgoing = True
    while len(list(tasks.all_nodes(story))) < nodelimit and keepgoing:
      keepgoing = False
      n += 1
      target = tasks.random_uninstantiated(story)
      if not target:
        print("No nodes to instantiate this cycle.")
      else:
        keepgoing = True
        print("Instantiating node '{}'...".format(target))
        try:
          program, story = tasks.instantiate_node(story, target, scaffolding)
          with open(
            os.path.join("out", "snapshots", "prog-inst-{}.lp".format(n)),
            'w'
          ) as fout:
            fout.write(program)
          with open(
            os.path.join("out", "snapshots", "story-{}.lp".format(n)),
            'w'
          ) as fout:
            for pr in story:
              fout.write(str(pr) + '.\n')
        except asp.ASPError as e:
          print(
            "Error during instantiation: {}. Dumping source to '{}'".format(
              e.message,
              CRASHFILE
            )
          )
          write_crashfile(e)
          error = True
          break
 
      target = tasks.random_unbranched(story)
      if not target:
        print("No nodes to branch this cycle.")
      else:
        keepgoing = True
        print("Branching node '{}'...".format(target))
        try:
          program, story = tasks.branch_node(story, target, scaffolding)
          with open(
            os.path.join("out", "snapshots", "prog-branch-{}.lp".format(n)),
            'w'
          ) as fout:
            fout.write(program)
        except asp.ASPError as e:
          print(
            "Error during branching: {}. Dumping source to '{}'".format(
              e.message,
              CRASHFILE
            )
          )
          write_crashfile(e)
          error = True
          break

      target = tasks.random_unpolished_ending(story)
      if not target:
        print("No endings to polish this cycle.")
      else:
        keepgoing = True
        print("Polishing ending node '{}'...".format(target))
        try:
          program, story = tasks.polish_ending(story, target, scaffolding)
          with open(
            os.path.join("out", "snapshots", "prog-polish-{}.lp".format(n)),
            'w'
          ) as fout:
            fout.write(program)
        except asp.ASPError as e:
          print(
            "Error while polishing ending: {}. Dumping source to '{}'".format(
              e.message,
              CRASHFILE
            )
          )
          write_crashfile(e)
          error = True
          break

      sofar = story

#  for pr in sofar:
#    print(str(pr) + '.')
#  nouns = english.glean_nouns(sofar)
#  for k in nouns:
#    print(nouns[k])
  print("Story nodes:")
  for pr in sofar:
    if (pr.name == "story_node"):
      print("  " + pr.args[0].name)

  print("Drawing story graph...")
  viz.viz(sofar, error, str(target))

  print("Writing out story facts...")
  with open(os.path.join("out", "facts.lp"), 'w') as fout:
    for pr in sofar:
      fout.write(str(pr) + '.\n')

  print("Building story text...")
  outfile = "story.txt"
  if fmt == "twee":
    outfile = "story.tw"
  with open(os.path.join("out", outfile), 'w') as fout:
    fout.write(english.build_story_text(sofar, timeshift=None, fmt=fmt))
  if fmt == "twee":
    html = subprocess.check_output(
      ["twee", os.path.join("out", outfile), os.path.join("twine", "*.tw")]
    )
    with open(os.path.join("out", "tlottolad.html"), 'w') as fout:
      fout.write(html.decode())
  return not error

if __name__ == "__main__":
  success = False
  storyfile = None
  scfiles = []
  scfrags = []
  nodelimit = 12
  fmt = "twee"

  if '-s' in sys.argv:
    idx = sys.argv.index('-s')
    storyfile = sys.argv[idx+1]

  if '-f' in sys.argv:
    idx = sys.argv.index('-f')
    scfiles.append(sys.argv[idx+1])

  if '-r' in sys.argv:
    idx = sys.argv.index('-r')
    scfrags.append(sys.argv[idx+1])

  if '-n' in sys.argv:
    idx = sys.argv.index('-n')
    nodelimit = int(sys.argv[idx+1])

  if "--twee" in sys.argv:
    fmt = "twee"

  if "--choicescript" in sys.argv:
    fmt = "choicescript"

  success = main(storyfile, scfiles, scfrags, nodelimit, fmt)

  if not success:
    exit(1)
