"""
storytasks.py
Defines some default tasks and task building blocks for story tasks.
"""

import copy
import re
import sys

from warnings import *

import tasknet as tn
import asp
import ans
import obj

from utils import *
from storyutils import *

# Global task directory (on module import, tasks from this directory will be
# loaded).
TASK_DIRECTORY = "tasks"

# This global variable stores tasks loaded from files in the tasks/
# subdirectory:
TASK_LIST = {}

class StoryTask(tn.Task):
  """
  A StoryTask is just a Task with some extra convenience methods for
  story-related operations.
  """
  def empty_story(self):
    """
    Resets the task's network's story to an empty set.
    """
    self.net.mem.code.story = set();

  def add_story_fact(self, predicate):
    """
    Adds the given predicate to the story facts for this task's task network.
    """
    self.net.mem.code.story.add(predicate)

  def remove_story_facts(self, *predicates):
    """
    Removes the given predicate(s) from this task's network's story.
    """
    self.net.mem.code.story = set(
      ans.filter(
        self.net.mem.code.story,
        forbid=predicates
      )
    )

  def story(self):
    """
    Returns a set of Predicates specifying the story that this task is
    operating on. Just grabs self.net.mem.code.story.
    """
    return self.net.mem.code.story

class TaskOverwriteWarning(Warning):
  pass

def register_task(task):
  """
  Registers the given task to the global TASK_LIST, producing a warning if a
  task with the same name as the new task already exists, and overwriting the
  old task in that case. Note that registration should be used for abstract
  tasks, and copies of registered tasks should be used to do actual work (see
  the clone_task function below).
  """
  global TASK_LIST
  if task.name in TASK_LIST:
    warn(
      "New task {} overwrites old task {}".format(
        task,
        TASK_LIST[task.name]
      ),
      TaskOverwriteWarning
    )
  TASK_LIST[task.name] = task

def clone_task(task, **kwargs):
  """
  Takes a Task object and returns a new Task that has the memory locations
  specified as keyword arguments overwritten with the given values. The new
  Task object will not inherit the dependencies, network, or status of the old
  Task object, but instead will be a fresh Task ready for insertion.
  """
  newmem = copy.deepcopy(task.mem)

  for key in kwargs:
    newmem[key] = kwargs[key]

  return StoryTask(
    task.func,
    priority=task.priority,
    mem=newmem,
    deps=None,
    net=None,
    source="cloned from {}".format(task)
  )

class TaskMissingError(Exception):
  pass

def spawn_task(_net, _name, **kwargs):
  """
  Looks up a task by name in the task list and spawns a copy of that task on
  the given network, passing keyword arguments (other than the arguments _net
  and _name that are used here) into clone_task to be written into the cloned
  task's memory. This function returns the Task object that it creates, but
  doesn't add any dependencies to or from it.
  """
  if _name not in TASK_LIST:
    raise TaskMissingError("Task '{}' not found.".format(_name))
  t = clone_task(TASK_LIST[_name], **kwargs)
  _net.add_task(t)
  return t

def subtask(_parent, _name, **kwargs):
  """
  Works like spawn_task, but takes a Task as its first argument and spwans the
  new task on the given task's network. It also adds a dependency from the
  parent task to the newly spawned task.
  """
  if _name not in TASK_LIST:
    raise TaskMissingError("Task '{}' not found.".format(_name))
  t = clone_task(TASK_LIST[_name], **kwargs)
  _parent.net.add_task(t)
  _parent.add_dep(t)
  return t

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
      def gen(t):
        func(t)
        yield tn.TaskStatus.Final.Completed
  else:
    if returnsstatus:
      def gen(t):
        yield func()
    else:
      def gen(t):
        func()
        yield tn.TaskStatus.Final.Completed
  gen.__name__ = func.__name__
  return StoryTask(gen, source="function '{}'".format(func.__name__))

def get_mem_predicates(mem, exclude=[], basename="mem"):
  """
  Takes an Obj and recursively enumerates all of its fields (except those
  matching the given exclude regexes). Using the optional parameter basename,
  the name of the base predicate can be changed. For each field, a Predicate is
  created of the form:

    <basename>("path.to.key", value)

  Note that the first argument to the base predicate is always quoted. The
  second argument takes on one of several forms depending on the value present:

    value is:            predicate form:

      3                    Predicate(3)
      3.5                  Predicate('"3.5"')
      "foo"                Predicate('"foo"')
      "has spaces"         Predicate('"has spaces"')
      Pr("a", Pr("b"))     Pr("a", Pr("b"))
      lambda: 3            Pr('"<function <lambda> at 0xfeeddeadbeef"')

  This means that if you store a predicate directly into a memory location,
  that predicate's structure will be preserved in the memory readout.
  """
  for key, value in obj.obj_contents(mem):
    if any(rx.match(key) for rx in exclude):
      continue
    yield ans.Predicate(
      basename,
      ans.Predicate(quote(key)),
      ans.as_predicate(value),
    )

def assemble_problem(task):
  return '\n'.join([
    "%%%%%%%%%%%%%%",
    "% Task code: %",
    "%%%%%%%%%%%%%%",
    '\n'.join(str(rule) for rule in strsorted(task.mem.code)),
    "%%%%%%%%%%%%%%%%%",
    "% Local memory: %",
    "%%%%%%%%%%%%%%%%%",
    '\n'.join(
      str(predicate) + '.' for predicate in get_mem_predicates(
        task.mem,
        exclude=[re.compile("code")]
      )
    ),
    "%%%%%%%%%%%%%%%%%%",
    "% Global memory: %",
    "%%%%%%%%%%%%%%%%%%",
    '\n'.join(
      str(predicate) + '.' for predicate in get_mem_predicates(
        task.net.mem,
        exclude=[re.compile("code")],
        basename="glmem"
      )
    ),
    "%%%%%%%%%%%%%%%%%%%",
    "% Universal code: %",
    "%%%%%%%%%%%%%%%%%%%",
    '\n'.join(str(rule) for rule in strsorted(task.net.mem.code.universal)),
    "%%%%%%%%%%%%%%%",
    "% Story code: %",
    "%%%%%%%%%%%%%%%",
    '\n'.join(
      str(predicate) + '.' for predicate in strsorted(task.net.mem.code.story)
    ),
  ])

class ASPTaskError(Exception):
  pass

# Schemas for binding active predicates:
active_schemas = {
  "proposed": ans.Pr("story", ans.Pr("proposed"), ans.SbT("Predicate")),
  "local_mem": ans.Pr("local_mem", ans.Vr("Address"), ans.SbT("Value")),
  "global_mem": ans.Pr("global_mem", ans.Vr("Address"), ans.SbT("Value")),
  "run_code": ans.Pr( "run_code", ans.Vr("QuotedCode")),
  "error": ans.Pr("error", ans.SbT("Message")),
  "status": ans.Pr("status", ans.Vr("String")),
  "spawn_task": ans.Pr("spawn_task", ans.Vr("Id"), ans.Vr("TaskName")),
  "task_arg": ans.Pr("task_arg", ans.Vr("Id"), ans.Vr("Key"), ans.SbT("Value")),
}

def asptask(name, code, source="unknown"):
  """
  Takes a name and a string (full of ASP predicates and/or constraints) and
  returns a Task object (setting the Tasks's source if given or using 'unknown'
  as the default source). The returned Task does the following when run:
    1. Gathers ASP source code from the following locations:
      a: The code passed as an argument to this function, stored in the task's
         local memory at mem.code.
      b: Any universal code for the task's network, in net.mem.code.universal.
      c: The current story state for the task's network, in net.mem.code.story.
      d: The entire contents of local memory *except* mem.code, converted into
         predicates of the form:
           mem("memory.address.of.key", "value").
      e: The entire contents of global memory *except* the net.mem.code
         subtree, converted into predicates of the form:
           glmem("memory.address.of.key", "value").
    2. Combines any source code found into a single ASP problem and solves it
       by calling `clingo` (see the asp module).
    3. Scans the resulting answer set for the following predicates and behaves
       accordingly, updating the code in net.mem.code.story and yielding an
       appropriate status:
     error(<predicate>)
       If any error predicates are generated, an exception will be raised and
       none of the other predicates in this list will be heeded.
     status(completed|failed|inprogress|blocked)
       The task will yield the given status. If either "completed" or "failed"
       is the result, it will automatically be removed from the active tasks
       list. If multiple status() predicates are detected an error is
       generated.
     story(proposed, <predicate>)
       The given predicate structure will be used as part of the current story
       going onwards, in the form story(current, <predicate>).
     local_mem(<address>, <predicate>)
       The given local memory address will be set to the given predicate.
     global_mem(<address>, <predicate>)
       The given (network-) global memory address will be set to the given
       predicate.
     spawn_task(<id>, <task-name>)
       A copy of the named task will be spawned on the current task's network
       after it completes. Arguments can be passed using the task_arg predciate
       structure.
     task_arg(<id>, <key>, <value>)
       Passes an argument to the task with the given id. This will set
       mem.args.<key> to <value> (which can be a predicate structure) in the
       spawned task.
     run_code(<code>)
       Runs the given Python code (the code must be quoted). Several useful
       local variables are available (and editing them affects continued
       operation):
         status:
           The current return status (a TaskStatus symbol) as specified by a
           'status' predicate. This can be modified to change the status
           yielded by the current execution cycle.
         task:
           The current Task object. This can be used to access both local and
           global memory as normal.
         addlist:
           A list of Predicate objects that are about to be added to the story.
         rmlist:
           A list of Predicate objects that are about to be removed from the
           story.
         lmemlist:
           A list of Address, Value objects that are about to be set in local
           memory.
         gmemlist:
           A list of Address, Value objects that are about to be set in
           (network-) global memory.
  """
  def gen(t):
    nonlocal name, code, source
    if not t.mem.code:
      raise ASPTaskError(
        "Task '{}' has no code (missing t.mem.code)!".format(name)
      )
    if type(t.net.mem.code.story) == obj.EmptyObj:
      raise ASPTaskError("No story found (missing t.net.mem.code.story)!")
    if type(t.net.mem.code.universal) == obj.EmptyObj:
      raise ASPTaskError(
        "No universal constraints object (missing t.net.mem.code.universal)!"
      )
    problem = assemble_problem(t)
    predicates = asp.solve(problem)

#    print("Predicates:")
#    for p in predicates:
#      print(p)

    errors = []
    status = None
    proplist = []
    to_run = []
    to_spawn = {}
    lmemlist = []
    gmemlist = []
    for (schema, binding) in ans.bindings(active_schemas, predicates):
      if schema == "error":
        print("Error in Clingo output!")
        errors.append(dequote(str(binding["error.Message"])))
      elif schema == "status":
        s = dequote(str(binding["status.String"]))
        if status == None:
          status = s
        else:
          status = status + " and " + s
      elif schema == "proposed":
        proplist.append(binding["story.Predicate"])
      elif schema == "local_mem":
        lmemlist.append(
          (
            dequote(str(binding["local_mem.Address"].name)),
            binding["local_mem.Value"]
          )
        )
      elif schema == "global_mem":
        gmemlist.append(
          (
            dequote(str(binding["global_mem.Address"])),
            binding["global_mem.Value"]
          )
        )
      elif schema == "spawn_task":
        tid = str(binding["spawn_task.Id"])
        tname = dequote(str(binding["spawn_task.TaskName"]))
        if tid not in to_spawn:
          to_spawn[tid] = {
            "name": "<unknown>",
            "args": {},
          }
        to_spawn[tid]["name"] = tname
      elif schema == "task_arg":
        tid = str(binding["task_arg.Id"])
        tkey = dequote(str(binding["task_arg.Key"]))
        tval = dequote(str(binding["task_arg.Value"]))
        if tid not in to_spawn:
          to_spawn[tid] = {
            "name": "<unknown>",
            "args": {},
          }
        to_spawn[tid]["args"][tkey] = tval
      elif schema == "run_code":
        to_run.append(unquote(binding["run_code.QuotedCode"]))

    if errors:
      raise ASPTaskError(
        "Error(s) while resolving answer set task:\n" + '\n'.join(errors)
      )

    if status in tn.TaskStatus.aliases:
      status = tn.TaskStatus.aliases[status]
    elif status == None:
      status = tn.TaskStatus.Final.Completed
    else:
      raise ASPTaskError(
        "Error: answer set produced invalid status '{}'.".format(status)
      )

    # Run code blocks before additions and removals are processed:
    for code in to_run:
      code_locals={
        "status": status,
        "task": t,
        "proplist": proplist,
        "lmemlist": lmemlist,
        "gmemlist": gmemlist,
      }
      compiled = compile(
        code,
        "<snippet from ASP task '{}' in {}>".format(name, source),
        'exec'
      )
      exec(
        compiled,
        {},
        code_locals
      )
      status = code_locals["status"]
      proplist = code_locals["proplist"]
      lmemlist = code_locals["lmemlist"]
      gmemlist = code_locals["gmemlist"]

    # Process the new story and memory elements:
    t.empty_story()
    for p in proplist:
      t.add_story_fact(ans.Pr("story", ans.Pr("current"), p))

    for (addr, val) in lmemlist:
      t.mem[addr] = val

    for (addr, val) in gmemlist:
      t.net.mem[addr] = val

    # Spawn tasks:
    for id in to_spawn:
      spawn_task(
        t.net,
        to_spawn[id]["name"],
        **to_spawn[id]["args"]
      )

    # We're finally done, so yield the indicated status:
    yield status
  # end of gen function

  gen.__name__ = name
  mem = obj.Obj()
  mem.code = ans.ruleset(ans.parse_asp(code))
  return StoryTask(gen, mem=mem, source=source)

def load_tasks(dir):
  """
  Loads tasks from files in the given directory. Walks the directory tree
  recursively, loading .lp files as ASP tasks (using asptask() on their
  contents) and .py files as python tasks by looking for a run function defined
  in the file and using it to construct a Task. Yields loaded Task objects one
  by one.
  """
  for f in walk_files(
    dir,
    include=lambda f: (f.endswith(".lp") or f.endswith(".py"))
  ):
    filename = os.path.split(f)[-1]
    taskname = '.'.join(filename.split('.')[:-1])

    if f.endswith(".py"):
      with open(f) as fin:
        contents = fin.read()
      code_locals = {}
      try:
        code = compile(
          contents,
          f,
          'exec',
        )
        exec(
          code,
          globals(),
          code_locals
        )
      except Exception as e:
        sys.stderr.write(
          "Error while loading Python task file '{}':\n".format(f)
        )
        raise e
      func = code_locals["run"]
      func.__name__ = taskname
      yield StoryTask(func, source=f)
    elif f.endswith(".lp"):
      with open(f) as fin:
        contents = fin.read()
      try:
        result = asptask(taskname, contents, source=f)
      except Exception as e:
        sys.stderr.write(
          "Error while loading ASP task file '{}':\n".format(f)
        )
        raise e
      yield result

def asp_task_dumper(result_filter):
  """
  Builds a maintenance function for a task network that dumps all tasks whose
  result passes the given filter. Matching tasks are dumped into a file in the
  "dumps" directory.
  """
  counter = 0
  def dump_asp_tasks(net, task, result):
    """
    A maintenance function for a task network that dumps all tasks to a file in
    the "dumps" directory.
    """
    nonlocal counter, result_filter
    if (result_filter(result) and task.mem.code):
      rname = tn.TaskStatus.names[result]
      if not os.path.exists("dumps"):
        os.mkdir("dumps")
      filename = "dumps/{}-{}-{}.td".format(
        rname,
        task.name,
        counter
      )
      with open(filename, 'w') as fout:
        fout.write(
          "% Dump of {} task:\n% {}\n% {}\n".format(
            rname,
            task,
            '\n% '.join(str(task.error).split('\n'))
          )
        )
        fout.write("% vim: syn=gringo\n")
        fout.write(assemble_problem(task))
      sys.stderr.write(
        "Dumped {} task to file '{}'.\n".format(rname, filename)
      )
      counter += 1
  return dump_asp_tasks

# Load tasks from the default directory on module load (the loading function is
# defined in utils.py):
for t in load_tasks(TASK_DIRECTORY):
  register_task(t)

def _test_add_character_task():
  import ans, storytasks
  net = tn.TaskNet()
  net.mem.code.universal = ans.load_logic_dir("rules")
  net.mem.code.story = set()
  storytasks.spawn_task(net, "add_character")
  leftovers = net.run(maintenance=asp_task_dumper(lambda r: True))
  unfinished = [t for t in leftovers if t.status != tn.TaskStatus.Final.Crashed]
  crashed = [t for t in leftovers if t.status == tn.TaskStatus.Final.Crashed]
  if unfinished:
    print('-'*80)
    print("Unfinished:")
    print('-'*80)
    print('\n'.join(str(t) for t in unfinished))
  if crashed:
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
  return not (unfinished or crashed)

def _test_add_event_task():
  import ans, storytasks
  net = tn.TaskNet()
  net.mem.code.universal = ans.load_logic_dir("rules")
  net.mem.code.story = set()
  storytasks.spawn_task(net, "add_event")
  leftovers = net.run(maintenance=asp_task_dumper(lambda r: True))
  unfinished = [t for t in leftovers if t.status != tn.TaskStatus.Final.Crashed]
  crashed = [t for t in leftovers if t.status == tn.TaskStatus.Final.Crashed]
  if unfinished:
    print('-'*80)
    print("Unfinished:")
    print('-'*80)
    print('\n'.join(str(t) for t in unfinished))
  if crashed:
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
  return not (unfinished or crashed)

_test_cases = [
  (_test_add_character_task, True),
  (_test_add_event_task, True),
]
