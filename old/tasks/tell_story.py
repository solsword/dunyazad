def run(self):
  bind = {
    "story_node": ans.Pr("story_node", ans.Vr("Node")),
    "node_option": ans.Pr("at", ans.Vr("Node"), ans.Pr("option",ans.Vr("Opt"))),
  }

  # TODO: Auto list here
  setups = [
    "setup_roc_attack",
    "setup_being_robbed",
  ]

  subtask(
    self,
    'setup'
  )
  yield tn.TaskStatus.Ongoing.InProgress
  for i in range(4):
    bindings = ans.bindings(bind, self.net.mem.code.story)
    nodes_with_options = [
      binding["at.Node"].name
        for (schema, binding) in bindings
        if schema == "node_option"
    ]
    unfinished = [
      binding["story_node.Node"].name
        for (schema, binding) in ans.bindings(bind, self.net.mem.code.story)
        if (
          schema == "story_node"
        and
          binding["story_node.Node"].name not in nodes_with_options
        )
    ]
    # DEBUG:
    print("------------")
    print("Unfinished: ", unfinished)
    print("------------")
    if not unfinished:
      print("No more unfinished.")
      print()
      break
    n = random.choice(unfinished)
    s = random.choice(setups)
    subtask(self, s, **{ 'args.target_node':n, })
    yield tn.TaskStatus.Ongoing.InProgress
    subtask(self, 'fill_node', **{ 'args.target_node':n, })
    yield tn.TaskStatus.Ongoing.InProgress
  yield tn.TaskStatus.Final.Completed
