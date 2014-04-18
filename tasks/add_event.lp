l_id(N+1) :- id(evt, N), not id(evt, N+1).
{ l_id(1) }.

:- { l_id(N) : l_id(N) } > 1.
:- { l_id(N) : l_id(N) } < 1.

l_type(Type) :- mem("args.type", Type).
{ l_type("unknown") }.

:- { l_type(Type) : l_type(Type) } > 1.
:- { l_type(Type) : l_type(Type) } < 1.

edit(append_event, l_new_event).
assert(at(1, property(l_new_event, type, Type))) :- l_type(Type).

local_mem( "result.id", N ) :- bound(l_new_event, id(evt, N)).
