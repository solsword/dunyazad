"""
tasks.py
Defines some default tasks and task building blocks.
"""

import tasknet

def mktask(func, returnsstatus=False, passtask=False):
  """
  Turns an ordinary function into a task. If 'returnsstatus' is specified the
  function is assumed to return a TaskStatus object; otherwise the task always
  returns a status of "Completed" after running the function. If 'passtask' is
  specified, the function is assumed to take a single argument holding the Task
  object created; otherwise it is called without arguments.
  """
  if passtask:
    if returnsstatus:
      def gen(t):
        yield func(t)
      else:
        func(t)
        yield tasknet.TaskStatus.Completed
  else:
    if returnsstatus:
      def gen(t):
        yield func()
    else:
      def gen(t):
        func()
        yield tasknet.TaskStatus.Completed
  gen.__name__ = func.__name__
  return Task(gen)

def asptask(asp):
  """
  Takes a string (full of ASP predicates and/or constraints) and returns a Task
  object which does the following when run:
    1. Gathers ASP source code from the following locations:
      a: The code passed as an argument to this function, stored in the task's
         local memory at mem.code.
      b: Any global code for the task's network, in net.mem.code.global.
      c: The current story state for the task's network, in net.mem.code.story.
    2. Combines any source code found into a single ASP problem and solves it
       by calling `clingo.`
    3. Scans the resulting answer set for the following predicates and behaves
       accordingly, updating the code in net.mem.code.story if directed to do
       so and yielding an appropriate status:
     status(completed|failed|inprogress|blocked)
       The task will yield the given status. If either "completed" or "failed"
       is the result, it will automatically be removed from the active tasks
       list. If multiple status() predicates are detected only the last
       alphabetically is honored (but this behavior shouldn't be depended on).
     action_add(<predicate structure>)
       The given predicate structure should be added to the story predicates.
       This takes the form of a simple textual copy-and-paste.
     action_remove(<predicate structure>)
       The given predicate structure should be removed from the story
       predicates. The predicate text must exactly match an entire line of the
       story code (minus the final period of course).
  """
  # TODO: HERE!
  pass
