def run(self):
  subtask('add_character', {'args.name':'A'} )
  subtask('add_character', {'args.name':'B'} )
  subtask('add_character', {'args.name':'C'} )
  yield TaskStatus.InProgress
  spawn_task('setup_provocation')
