l_id(N+1) :- character(N), not character(N+1).
{ l_id(1) }.

:- { l_id(N) : l_id(N) } > 1.
:- { l_id(N) : l_id(N) } < 1.

l_name(Name) :- mem("args.name", Name).
{ l_name("John Doe") }.

:- { l_name(Name) : l_name(Name) } > 1.
:- { l_name(Name) : l_name(Name) } < 1.

story_add( character(N) ) :- l_id(N).
story_add( intrinsic( id(chr,N), name, Name ) ) :- l_name(Name), l_id(N).

local_mem( "result.id", N ) :- l_id(N).
