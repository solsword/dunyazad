def run(self):
  subtask(self, 'add_character', **{'args.name':'A'} )
  yield tn.TaskStatus.Ongoing.InProgress
  spawn_task(self.net, 'setup_provocation')
