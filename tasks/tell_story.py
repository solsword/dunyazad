def run(self):
  subtask(self, 'add_character', **{'args.name':'A'} )
  subtask(self, 'add_character', **{'args.name':'B'} )
  subtask(self, 'add_character', **{'args.name':'C'} )
  yield tn.TaskStatus.InProgress
  spawn_task(self.net, 'setup_provocation')
