"""
tn.py
Task network representation: tracks tasks and dependencies between them.
"""

@singleton("task status")
class TaskStatus:
  @singleton("ready")
  class Ready: pass

  @singleton("completed")
  class Completed: pass

  @singleton("failed")
  class Failed: pass

  @singleton("in progress")
  class InProgress: pass

  @singleton("blocked")
  class Blocked: pass

  @singleton("suspended")
  class Suspended: pass


@singleton("goal status")
class GoalStatus:
  @singleton("ready")
  class Ready: pass

  @singleton("completed")
  class Completed: pass

  @singleton("failed")
  class Failed: pass

  @singleton("in progress")
  class InProgress: pass

  @singleton("dormant")
  class Dormant: pass


@singleton("goal policy")
class GoalPolicy:
  @singleton("try once")
  class TryOnce:
    def check(task_result, tasks_remaining):
      if task_result == TaskStatus.Succeeded:
        return GoalStatus.Completed
      elif task_result == TaskStatus.Failed:
        return GoalStatus.Failed
      else:
        return GoalStatus.Failed

  @singleton("repeat on success")
  class RepeatOnSuccess: pass

  @singleton("repeat on failure")
  class RepeatOnFailure: pass

  @singleton("repeat")
  class Repeat: pass


class obj:
  """
  A perfectly generic object.
  """
  def __init__(self, **kwargs):
    self.__dict__.update(kwargs)

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

class Task:
  """
  A task has a generator function that gets run when the task is up for
  execution. This function should perform a small chunk of work and then
  quickly yield one of the TaskStatus singletons back to the task scheduler. 
  If it yields Succeeded or Failed the task network will be updated
  accordingly, removing the task from the pool of active tasks.

  Each task also has a priority, a local memory, a list of dependencies, a
  current status, and a parent task network (which is None by default).
  """
  def __init__(self, func, priority=0.5, deps=None, mem=None, net=None):
    self.func = func
    self.priority = priority
    self.deps = deps or []
    self.mem = mem or obj()
    self.net = net
    self.status = TaskStatus.Ready

  def ready(self):
    return all(d.check() for d in self.deps)

  def run(self):
    if not self.ready():
      return TaskStatus.Blocked
    self.status = self.func(self)
    return self.status

  def __str__(self):
    return "task '{}'".format(self.func.__name__)

class Goal:
  """
  While a Task is a chunk of work to do, a Goal is an abstract desire, which
  might be pursued via a variety of tasks.

  A Goal has a condition, which when met fulfils the goal. It also has a
  policy, which indicates what should happen when a goal-critical task fails.
  Optionally, a Goal can have preconditions, which will keep it suppressed
  until they are met.

  Finally, a goal has a list of critical tasks, it's own memory object, and a
  reference to a parent network.
  """
  def __init__(
    self,
    cond,
    policy=None,
    precond=None,
    tasks=None,
    mem=None,
    net=None
  ):
    self.func = func
    self.priority = priority
    self.deps = deps or []
    self.mem = mem or obj()
    self.net = net
    self.status = TaskStatus.Ready

class TaskNet:
  """
  A TaskNet is a network of tasks. Each task keeps track of its own
  dependencies, while the task network just tracks the pool of tasks. The
  network does keep track of active vs. inactive tasks though, and it stores a
  global memory object which all of its tasks can access.
  """
  def __init__(self, active=None, inactive=None, mem=None):
    self.active = active or []
    self.inactive = inactive or []
    self.mem = mem or obj()

  def ready(self):
    """
    A generator for the set of ready tasks from the active task list of this
    network.
    """
    for t in self.active:
      if t.ready():
        yield t

  def 
