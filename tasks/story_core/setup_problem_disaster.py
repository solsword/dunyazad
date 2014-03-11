def run(self):
  subtask(
    self,
    'add_event',
    **{
      'args.type':'protagonist',
    }
  )
  subtask(
    self,
    'add_character',
    **{ 'args.role':'companion', 'args.name':'Anenri', }
  )
  subtask(
    self,
    'add_character',
    **{ 'args.role':'antagonist', 'args.name':'Umyrgai', }
  )
  subtask(
    self,
    'add_character',
    **{ 'args.role':'stranger', 'args.name':'Quollips', }
  )
  yield tn.TaskStatus.Ongoing.InProgress
  #self.add_story_fact(id_pr("evt", 1))
  spawn_task(self.net, 'setup_provocation')
