l_id(N+1) :- id(evt, N), not id(evt, N+1).
{ l_id(1) }.

:- { l_id(N) : l_id(N) } > 1.
:- { l_id(N) : l_id(N) } < 1.

l_type(Type) :- mem("args.type", Type).
{ l_type("unknown") }.

:- { l_type(Type) : l_type(Type) } > 1.
:- { l_type(Type) : l_type(Type) } < 1.

story_add( id(evt, N) ) :- l_id(N).
story_add( intrinsic( id(evt, N), type, Type ) ) :- l_type(Type), l_id(N).

local_mem( "result.id", N ) :- l_id(N).
