arg_default(type, "unknown")

edit(append_event, l_new_event).
assert(arg(l_new_event, type, Type)) :- task_arg(type, Type).

local_mem( "result.id", N ) :- bound(l_new_event, id(evt, N)).
