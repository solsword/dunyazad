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

def task_status(cls):
  """
  Creates and returns an instance of the given class but also registers it as a
  TaskStatus by adding it to TaskStatus' __dict__ and to the dictionary of
  aliases.
  """
  setattr(TaskStatus, cls.__name__, cls)
  TaskStatus.aliases[cls.alias] = cls
  return cls

@task_status
class Ready(TaskStatus):
  alias = "ready"

@task_status
class Completed(TaskStatus):
  alias = "completed"

@task_status
class Failed(TaskStatus):
  alias = "failed"

@task_status
class InProgress(TaskStatus):
  alias = "in_progress"

@task_status
class Blocked(TaskStatus):
  alias = "blocked"


class Dependency:
  """
  A dependency indicates that one task relies on another being in a certain
  state. If the dependent task isn't in the appropriate state, attempts to run
  the depending task will automatically return a "blocked" status.
  """
  def __init__(self, head, tail, requires=TaskStatus.Completed):
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
  yields Succeeded or Failed the task network will be updated accordingly,
  removing the task from the pool of active tasks.

  Each task also has a priority, a local memory, a list of dependencies, a
  current status, and a parent task network (which is None by default).

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
    source="unknown"
  ):
    self.func = func
    self.name = self.func.__name__
    self.priority = priority
    self.deps = deps or set()
    self.mem = mem or obj.Obj()
    self.net = net or obj.Obj()
    self.status = TaskStatus.Ready
    self.source = source
    self._gen = None

  def ready(self):
    return all(d.check() for d in self.deps)

  def run(self):
    if not self._gen:
      self._gen = self.func(self)
    if not self.ready():
      self.status = TaskStatus.Blocked
    else:
      self.status = next(self._gen, TaskStatus.Completed)
    return self.status

  def add_dep(self, other, state=TaskStatus.Completed):
    self.deps.add(Dependency(self, other, requires=state))

  def rm_dep(self, other, state=TaskStatus.Completed):
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
    Runs the given task (which should be on the active list; returns an error
    if isn't). Automatically handles last-task tracking and moves the task onto
    the finished list if it returns a status of either Completed or Failed.
    """
    if task not in self.active:
      raise MissingTaskException(
        "Task to run missing from task network's active list."
      )
    st = task.run()
    self.last = task
    if st in (TaskStatus.Completed, TaskStatus.Failed):
      self.active.remove(task)
      self.finished.append(task)

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

  def step(self, scheduling="round-robin"):
    """
    Determines the next task using the given scheduling mode (see `next`) and
    runs it (see `run_task`). Returns True if a step was executed and False if
    there were no tasks to execute.
    """
    n = self.next(mode=scheduling)
    if n:
      self.run_task(n)
      return True
    else:
      return False

  def run(self, scheduling="round-robin"):
    """
    Repeatedly calls `step` until there are no more tasks to accomplish, using
    the given scheduling mode.
    """
    while self.step(scheduling=scheduling):
      pass

def _test_tasknet_basic():
  tn = TaskNet()
  def put_hello(t):
    t.net.mem.string = "Hello "
    yield TaskStatus.Completed
  def put_world(t):
    t.net.mem.string += "world!"
    yield TaskStatus.Completed
  t1 = Task(put_hello)
  t2 = Task(put_world)
  t2.add_dep(t1)
  tn.add_tasks(t1, t2)
  tn.run()
  return tn.mem.string

_test_cases = [
  (_test_tasknet_basic, "Hello world!")
]
