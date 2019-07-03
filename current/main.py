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
import tasks
import viz
import english

from utils import *

CRASHFILE = os.path.join("out", "crash.lp")

CLINGO_EXE = ["clingo"]
TWEE_EXE = ["twee"]
TWEE2_EXE = ["twee2"]
VIZ_EXE = ["dot"]

def write_crashfile(e):
  with open(CRASHFILE, 'w') as fout:
    fout.write('% stdout:\n%' + '\n%'.join(e.stdout.split('\n')))
    fout.write('% stderr:\n%' + '\n%'.join(e.stderr.split('\n')))
    fout.write(e.program)

def main(
  mode = "full",
  intro_mode = "full",
  storyfile = None,
  scaffoldfiles = None,
  scaffoldfrags = None,
  nodelimit = 12,
  fmt="twee2",
  do_viz = True,
  seed=0,
  rand=0.0
):
  print("Seed: {}".format(seed))
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
      program, setup = tasks.setup_story([], scaffolding, seed, rand)
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
    if mode == "example":
      nodelimit = 2
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
          program, story = tasks.instantiate_node(
            story,
            target,
            scaffolding,
            seed,
            rand
          )
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
          program, story = tasks.branch_node(
            story,
            target,
            scaffolding,
            seed,
            rand
          )
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
          program, story = tasks.polish_ending(
            story,
            target,
            scaffolding,
            seed,
            rand
          )
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

  print("Story nodes:")
  for pr in sofar:
    if (pr.name == "story_node"):
      print("  " + pr.args[0].name)

  if (do_viz):
    print("Drawing story graph...")
    viz.viz(sofar, error, str(target))

  print("Writing out story facts...")
  with open(os.path.join("out", "facts.lp"), 'w') as fout:
    for pr in sofar:
      fout.write(str(pr) + '.\n')

  print("Building story text...")
  outfile = "story.txt"
  output = english.build_story_text(
    sofar,
    mode,
    intro_mode,
    timeshift=None,
    fmt=fmt
  )
  if fmt == "twee":
    outfile = "story.tw"
  elif fmt == "twee2":
    outfile = "story.tw2"
  elif fmt == "example":
    outfile = "story.html"
  elif fmt == "turk":
    outfile = "dunyazad.input"
    output = """\
"seed","framing","assets","set_off","setup","potentials","prompt","opt1","opt2","opt3","out1","out2","out3"
"{}",{}\
""".format(str(seed), output)
  with open(os.path.join("out", outfile), 'w') as fout:
    fout.write(output)
  if fmt == "twee":
    print("Compiling Twine version...")
    html = subprocess.check_output(
      TWEE_EXE + [os.path.join("out", outfile), os.path.join("twine", "*.tw")]
    )
    with open(os.path.join("out", "tlottolad.html"), 'w') as fout:
      fout.write(html.decode())
  if fmt == "twee2":
    print("Compiling Twine2 version...")
    messages = subprocess.check_output(
      TWEE2_EXE + [
        "build",
        os.path.join("twine", "template.tw"),
        os.path.join("out", "tlottolad.html"),
        "--format=SugarCube2"
      ]
    )
  return not error

if __name__ == "__main__":
  success = False
  storyfile = None
  scfiles = []
  scfrags = []
  mode = "full"
  intro_mode = "full"
  nodelimit = 12
  fmt = "twee2"
  do_viz = True
  seed = random.randint(1, 100000)
  rand = 0.15

  if '-s' in sys.argv:
    idx = sys.argv.index('-s')
    storyfile = sys.argv[idx+1]

  if '-f' in sys.argv:
    idx = sys.argv.index('-f')
    scfiles.append(sys.argv[idx+1])

  if '-r' in sys.argv:
    idx = sys.argv.index('-r')
    scfrags.append(sys.argv[idx+1])

  if '-e' in sys.argv:
    # TODO: Seeds don't work at all!
    idx = sys.argv.index('-e')
    seed = int(sys.argv[idx+1])

  if '-n' in sys.argv:
    idx = sys.argv.index('-n')
    nodelimit = int(sys.argv[idx+1])

  if '-x' in sys.argv:
    idx = sys.argv.index('-x')
    mode = "example"
    intro_mode = "brief"
    fmt = "example"
    fragment = sys.argv[idx+1]
    if fragment.startswith("fragments/") and fragment.endswith(".lp"):
      fragment = fragment[10:-3]
    scfrags.append(fragment)
    scfrags.append("example")

  if '--example' in sys.argv:
    mode = "example"
    intro_mode = "brief"
    scfrags.append("example")

  if '--brief-intro' in sys.argv:
    intro_mode = "brief"

  if "--twee" in sys.argv:
    fmt = "twee"

  if "--twee2" in sys.argv:
    fmt = "twee2"

  if "--choicescript" in sys.argv:
    fmt = "choicescript"

  if "--turk" in sys.argv:
    fmt = "turk"

  if '--clingo-exe' in sys.argv:
    idx = sys.argv.index('--clingo-exe')
    CLINGO_EXE = '&'.split(sys.argv[idx+1])

  if '--twee-exe' in sys.argv:
    idx = sys.argv.index('--twee-exe')
    TWEE_EXE = '&'.split(sys.argv[idx+1])

  if '--twee2-exe' in sys.argv:
    idx = sys.argv.index('--twee2-exe')
    TWEE2_EXE = '&'.split(sys.argv[idx+1])

  if '--graphviz-exe' in sys.argv:
    idx = sys.argv.index('--graphviz-exe')
    VIZ_EXE = '&'.split(sys.argv[idx+1])

  if '--no-viz' in sys.argv:
    do_viz = False

  asp.CLINGO_EXE = CLINGO_EXE
  viz.VIZ_EXE = VIZ_EXE
  success = main(
    mode,
    intro_mode,
    storyfile,
    scfiles,
    scfrags,
    nodelimit,
    fmt,
    do_viz,
    seed,
    rand
  )

  if not success:
    exit(1)
