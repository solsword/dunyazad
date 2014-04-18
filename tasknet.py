"""
tasknet.py
Task network representation: tracks tasks and dependencies between them.
"""

import random

from utils import *

import obj

@abstract
class TaskStatus:
  aliases = {}
  def __str__(self):
    return self.alias

@abstract
class Initial(TaskStatus):
  pass
TaskStatus.Initial = Initial

@abstract
class Ready(TaskStatus.Initial):
  alias = "ready"
TaskStatus.Initial.Ready = Ready

@abstract
class Ongoing(TaskStatus):
  pass
TaskStatus.Ongoing = Ongoing

@abstract
class InProgress(TaskStatus.Ongoing):
  alias = "in_progress"
TaskStatus.Ongoing.InProgress = InProgress

@abstract
class Blocked(TaskStatus.Ongoing):
  alias = "blocked"
TaskStatus.Ongoing.Blocked = Blocked

@abstract
class Final(TaskStatus):
  pass
TaskStatus.Final = Final

@abstract
class Completed(TaskStatus.Final):
  alias = "completed"
TaskStatus.Final.Completed = Completed

@abstract
class Failed(TaskStatus.Final):
  alias = "failed"
TaskStatus.Final.Failed = Failed

@abstract
class Crashed(TaskStatus.Final):
  alias = "crashed"
TaskStatus.Final.Crashed = Crashed

# Collect all aliases into an aliases dictionary:
for attr, val in walk_attributes(TaskStatus):
  if hasattr(val, "alias"):
    TaskStatus.aliases[val.alias] = val


class Dependency:
  """
  A dependency indicates that one task relies on another being in a certain
  state. If the dependent task isn't in the appropriate state, attempts to run
  the depending task will automatically return a "blocked" status.
  """
  def __init__(self, head, tail, requires=TaskStatus.Final.Completed):
    self.head = head
    self.tail = tail
    self.requires = requires

  def check(self):
    return self.tail.status == self.requires

  def __hash__(self):
    return 47 * (
      47 * (
        47 * (
          31 + hash(self.head)
        ) + hash(self.tail)
      ) + hash(self.requires)
    )

  def __eq__(self, other):
    return \
      self.head == other.head \
     and self.tail == other.tail \
     and self.requires == other.requires

  def __ne__(self, other):
    return not self == other


class Task:
  """
  A task has a generator function that gets run when the task is up for
  execution. This function should perform a small chunk of work and then
  quickly yield one of the TaskStatus objects back to the task scheduler. If it
  yields Completed, Failed, or Crashed the task network will be updated
  accordingly, removing the task from the pool of active tasks.

  Each task also has a priority, a local memory, a list of dependencies, a
  current status, a parent task network (which is None by default), and an
  error.

  Tasks can also be assigned a "source" which indicates where the code came
  from, and which is printed when errors or warnings are issued.
  """
  def __init__(
    self,
    func,
    priority=0.5,
    mem=None,
    deps=None,
    net=None,
    source="unknown",
    error=None,
  ):
    self.func = func
    self.name = self.func.__name__
    self.priority = priority
    self.deps = deps or set()
    self.mem = mem or obj.Obj()
    self.net = net or obj.Obj()
    self.status = TaskStatus.Initial.Ready
    self.source = source
    self.error = error
    self._gen = None

  def ready(self):
    return all(d.check() for d in self.deps)

  def run(self):
    if not self._gen:
      self._gen = self.func(self)
    if not self.ready():
      self.status = TaskStatus.Ongoing.Blocked
    else:
      try:
        self.status = next(self._gen, TaskStatus.Final.Completed)
        self.error = None
      except Exception as e:
        self.status = TaskStatus.Final.Crashed
        self.error = e
    return self.status

  def add_dep(self, other, state=TaskStatus.Final.Completed):
    self.deps.add(Dependency(self, other, requires=state))

  def rm_dep(self, other, state=TaskStatus.Final.Completed):
    self.deps.remove(Dependency(self, other, requires=state))

  def __str__(self):
    return "task '{}' #{} source: {}".format(
      self.name,
      hash(self),
      self.source
    )

class MissingTaskException(Exception):
  pass

class TaskNet:
  """
  A TaskNet is a network of tasks. Each task keeps track of its own
  dependencies, while the task network just tracks the pool of tasks. The
  network does keep track of the last task run, active vs. finished tasks, and
  a global memory object which all of its tasks can access.
  """
  def __init__(self, active=None, finished=None, mem=None):
    self.last = None
    self.active = active or []
    self.finished = finished or []
    self.mem = mem or obj.Obj()

  def collect_unifinished(self):
    """
    Returns a list of unfinished tasks: finalized tasks that either failed or
    crashed, and tasks that aren't finalized.
    """
    return self.active + [
      t for t in self.finished if t.status != TaskStatus.Final.Completed
    ]

  def ready(self):
    """
    Returns a generator for the set of ready tasks from the active task list of
    this network.
    """
    for t in self.active:
      if t.ready():
        yield t

  def add_task(self, task, finished=False):
    """
    Adds the given task to this task network, and sets the task's net field to
    this network. Optionally marks it as finished.
    """
    if finished:
      self.finished.append(task)
    else:
      self.active.append(task)
    task.net = self

  def add_tasks(self, *tasks, finished=False):
    """
    Adds multiple tasks to this task network, and sets the tasks' net fields to
    this network. Optionally marks them as finished.
    """
    if finished:
      self.finished.extend(tasks)
    else:
      self.active.extend(tasks)
    for t in tasks:
      t.net = self

  def run_task(self, task):
    """
    Runs the given task (which should be on the active list; raises an error if
    isn't). Automatically handles last-task tracking and moves the task onto
    the finished list if it returns a Final status. This function returns the
    status returned by the task.
    """
    if task not in self.active:
      raise MissingTaskException(
        "Task to run missing from task network's active list."
      )
    st = task.run()
    self.last = task
    if issubclass(st, TaskStatus.Final):
      self.active.remove(task)
      self.finished.append(task)
    return st

  def next(self, mode="round-robin", visited=set()):
    """
    Returns the "next" task to be run (returns None if there are no ready
    tasks). Possible selection modes include:
      first: select the first ready task according to the order of tasks in the
         active task list.
      random: use the hash of the last task to determine which to run next out
         of the currently ready tasks. A presumably chaotic but also
         deterministic ordering.
      round-robin (the default): selects a task using the same pseudorandom
         method as "random," but never revisits a task until all tasks have
         been visited. Note that this visitation property may be violated if
         next() is called using a different mode.
    Note that the "visited" argument is just a pseudo-static variable and
    shouldn't ever be used when calling this function.
    """
    if not self.active:
      return None

    firstready = next(self.ready(), None)
    if not firstready:
      return None
    else:
      if mode == "first":
        return firstready
      elif mode == "random":
        random.seed(hash(self.active[0]))
        return random.choice(list(self.ready()))
      elif mode == "round-robin":
        random.seed(hash(self.active[0]))
        ready = list(self.ready())
        candidates = [t for t in ready if t not in visited]
        if not candidates:
          candidates = ready
          visited.clear()
        choice = random.choice(candidates)
        visited.add(choice)
        return choice
      else:
        raise ValueError("Invalid scheduling mode '{}'".format(mode))

  def step(self, scheduling="round-robin"):
    """
    Determines the next task using the given scheduling mode (see `next`) and
    runs it (see `run_task`). Returns a tuple of the task that was run and its
    resulting status, or None if there were no tasks to execute.
    """
    n = self.next(mode=scheduling)
    if n:
      return (n, self.run_task(n))
    else:
      return None

  def run(self, scheduling="round-robin", maintenance=None):
    """
    Repeatedly calls `step` until there are no more tasks to accomplish, using
    the given scheduling mode. When no more tasks are available, it returns a
    list of "leftover" tasks: non-finished tasks and finished tasks that either
    failed or crashed.
    
    If a maintenance function is given, it is called with the tasknet, the
    last-run task, and the status of the last-run task as arguments after every
    step. The maintenance function is not called if step() returns None (which
    is the condition for finishing a run).
    """
    if maintenance:
      result = self.step(scheduling=scheduling)
      while result:
        maintenance(self, *result)
        result = self.step(scheduling=scheduling)
    else:
      while self.step(scheduling=scheduling):
        pass
    return self.collect_unifinished()

def trace_net(net, task, result):
  """
  Designed to be passed as a maintenance function to TaskNet.run, this will
  print a simple trace of activity.
  """
  print(
    "Task {}: {}".format(
      task.name, result.__name__
    )
  )

def compile_trace(destination):
  """
  Given a destination list, returns a maintenance function that appends a trace
  string to the given list every time it is called.
  """
  def trace(net, task, result):
    nonlocal destination
    destination.append("Task {}: {}".format(task.name, result.__name__))
  return trace

def _test_create_tasknet():
  tn = TaskNet()
  return True

def _test_add_tasks():
  tn = TaskNet()
  def put_hello(t):
    t.net.mem.string = "Hello "
    yield TaskStatus.Final.Completed
  def put_world(t):
    t.net.mem.string += "world!"
    yield TaskStatus.Final.Completed
  t1 = Task(put_hello)
  t2 = Task(put_world)
  t2.add_dep(t1)
  tn.add_tasks(t1, t2)
  return True

def _test_run_net():
  tn = TaskNet()
  def put_hello(t):
    t.net.mem.string = "Hello "
    yield TaskStatus.Final.Completed
  def put_world(t):
    t.net.mem.string += "world!"
    yield TaskStatus.Final.Completed
  t1 = Task(put_hello)
  t2 = Task(put_world)
  t2.add_dep(t1)
  tn.add_tasks(t1, t2)
  tn.run()
  return tn.mem.string

def _test_net_unfinished():
  tn = TaskNet()
  def put_hello(t):
    t.net.mem.string = "Hello "
    yield TaskStatus.Final.Completed
  def put_world(t):
    t.net.mem.string += "world!"
    yield TaskStatus.Final.Completed
  def impossible(t):
    yield TaskStatus.Final.Failed
  t1 = Task(put_hello)
  t2 = Task(put_world)
  t2.add_dep(t1)
  t3 = Task(impossible)
  tn.add_tasks(t1, t2, t3)
  return [t.name for t in tn.run()]

def _test_net_trace():
  tn = TaskNet()
  def put_hello(t):
    t.net.mem.string = "Hello "
    yield TaskStatus.Final.Completed
  def put_world(t):
    t.net.mem.string += "world!"
    yield TaskStatus.Final.Completed
  t1 = Task(put_hello)
  t2 = Task(put_world)
  t2.add_dep(t1)
  tn.add_tasks(t1, t2)
  trace = []
  tn.run(maintenance=compile_trace(trace))
  return trace

def _test_add_character_task():
  import ans, storytasks
  net = TaskNet()
  net.mem.code.universal = ans.load_logic("global")
  net.mem.code.story = set()
  storytasks.spawn_task(net, "add_character")
  leftovers = net.run()
  unfinished = [t for t in leftovers if t.status != TaskStatus.Final.Crashed]
  crashed = [t for t in leftovers if t.status == TaskStatus.Final.Crashed]
  print('-'*80)
  print("Unfinished:")
  print('-'*80)
  print('\n'.join(str(t) for t in unfinished))
  print('-'*80)
  print("Crashed:")
  print('-'*80)
  print(
    '\n'.join(
      "{}\n{}".format(
        t,
        format_exception(t.error) if t.error else '<no error?!>'
      ) for t in crashed
    )
  )
  print('-'*80)
  print("Story:")
  print('-'*80)
  print(
    '\n'.join(
      sorted(
        str(predicate) + '.' for predicate in net.mem.code.story
      )
    )
  )
  print('-'*80)

_test_cases = [
  (issubclass, (TaskStatus.Final.Completed, TaskStatus.Final), True),
  (_test_create_tasknet, True),
  (_test_add_tasks, True),
  (_test_run_net, "Hello world!"),
  (_test_net_unfinished, ["impossible"]),
  (_test_net_trace, ["Task put_hello: Completed", "Task put_world: Completed"]),
  (_test_add_character_task, None),
]
