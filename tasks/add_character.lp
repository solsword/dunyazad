next_chr_id(1) :- { character(N) } 0.
next_chr_id(N+1) :- character(N), { character(N+1) } 0.

l_name(Name) :- mem("args.name", Name).
l_name("John Doe") :- { mem("args.name", _) } 0.

l_id(N) :- next_chr_id(N).

story_add( character(N) ) :- l_id(N).
story_add( intrinsic( id(chr,1), name, Name ) ) :- l_name(Name).

local_mem( "result.id", N ) :- l_id(N).
