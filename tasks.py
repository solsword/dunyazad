"""
tasks.py
Defines some default tasks and task building blocks.
"""

import tasknet
import asp
import parser

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
  for key, value in mem.contents():
    if any(rx.match(key) for rx in exclude):
      continue
    yield Predicate(
      basename,
      Predicate(quote(key)),
      as_predicate(value),
    )

class ASPTaskError(Exception):
  pass

# Schemas for binding active predicates:
active_schemas = {
  "story_add": Pr("story_add", SbT("Predicate")),
  "story_remove": Pr("story_remove", SbT("Predicate")),
  "run_code": Pr( "run_code", Vr("QuotedCode")),
  "error": Pr("error", Vr("Message")),
  "status": Pr("status", Vr("String")),
}

def asptask(name, asp):
  """
  Takes a name and a string (full of ASP predicates and/or constraints) and
  returns a Task object which does the following when run:
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
       accordingly, updating the code in net.mem.code.story if directed to do
       so and yielding an appropriate status:
     error("error message")
       If any error predicates are generated, an exception will be raised and
       none of the other predicates in this list will be heeded.
     status(completed|failed|inprogress|blocked)
       The task will yield the given status. If either "completed" or "failed"
       is the result, it will automatically be removed from the active tasks
       list. If multiple status() predicates are detected an error is
       generated.
     story_add(<predicate>)
       The given predicate structure will be added to the story predicates.
       Note that add actions are processed before remove actions, so if a
       predicate is listed for both removal and addition it will be absent from
       the story after modifications have been processed.
     story_remove(<predicate>)
       The given predicate will be removed from the story predicates. The
       predicate must exactly match an entire story predicate.
     run_code(code)
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
  """
  def gen(t):
    if not t.mem.code:
      raise ASPTaskError(
        "Task '{}' has no code (missing t.mem.code)!".format(name)
      )
    if not t.net.mem.code.story:
      raise ASPTaskError("No story found (missing t.net.mem.code.story)!")
    if type(t.net.mem.code.universal) == EmptyObj:
      raise ASPTaskError(
        "No universal constraints object (missing t.net.mem.code.universal)!"
      )
    source = '\n'.join([
      "%%%%%%%%%%%%%%",
      "% Task code: %",
      "%%%%%%%%%%%%%%",
      '\n'.join(str(p) + '.' for p in t.mem.code),
      "%%%%%%%%%%%%%%%%%",
      "% Local memory: %",
      "%%%%%%%%%%%%%%%%%",
      '\n'.join(
        str(p) + '.' for p in get_mem_predicates(
          t.mem,
          exclude=[re.compile("code")]
        )
      )
      "%%%%%%%%%%%%%%%%%%",
      "% Global memory: %",
      "%%%%%%%%%%%%%%%%%%",
      '\n'.join(
        str(p) + '.' for p in get_mem_predicates(
          t.net.mem,
          exclude=[re.compile("code")],
          basename="glmem"
        )
      )
      "%%%%%%%%%%%%%%%%%%%",
      "% Universal code: %",
      "%%%%%%%%%%%%%%%%%%%",
      '\n'.join(str(p) + '.' for p in t.net.mem.code.universal),
      "%%%%%%%%%%%%%%%",
      "% Story code: %",
      "%%%%%%%%%%%%%%%",
      '\n'.join(str(p) + '.' for p in t.net.mem.code.story)
    ])
    results = asp.solve(source)
    # TODO: avoid this?
    predicates = parser.parse_facts(results)

    errors = []
    status = None
    addlist = []
    rmlist = []
    to_run = []
    for (schema, binding) in bindings(active_schemas, predicates):
      if schema == "error":
        errors.append(binding["error.Message"].name)
      elif schema == "status":
        if status == None:
          status = binding["status.String"].name
        else:
          status = status + " and " + binding["status.String"].name
      elif schema == "story_add":
        addlist.append(binding["story_add.Predicate"])
      elif schema == "story_remove":
        rmlist.append(binding["story_remove.Predicate"])
      elif schema == "run_code":
        to_run.append(unquote(binding["run_code.QuotedCode"]))

    if errors:
      raise ASPTaskError(
        "Error(s) while resolving answer set task:\n" + '\n'.join(errors)
      )

    if status in TaskStatus.statuses:
      status = TaskStatus.statuses[status]
    else:
      raise ASPTaskError(
        "Error: answer set produced invalid status '{}'.".format(status)
      )

    # Run code blocks before additions and removals are processed:
    for code in to_run:
      code_locals={
        "status": status,
        "task": t,
        "addlist": addlist,
        "rmlist": rmlist,
      }
      exec(
        code,
        globals = {},
        locals = code_locals
      )
      status = code_locals["status"]
      addlist = code_locals["addlist"]
      rmlist = code_locals["rmlist"]

    # Process additions and removals:
    for p in addlist:
      t.net.mem.code.story.add(p)
    t.net.mem.code.story = set(
      filter(
        t.net.mem.code.story,
        forbid=rmlist
      )
    )

    # We're finally done, so yield the indicated status:
    yield status
  # end of gen function

  gen.__name__ = name
  mem = Obj()
  # TODO: use actual predicates+constraints instead of strings?
  singledot = re.compile(
    # a period neither preceded nor followed by a period:
    r"(?<!\.)\.(?!\.)"
  )
  mem.code = singledot.split(asp.replace('\n', ' '))
  return Task(gen, mem=mem)
