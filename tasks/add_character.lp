l_found_next_chr_id(N+1) :- character(N), { character(N+1) } = 0.

{ l_some_chr_id : l_found_next_chr_id(N); l_no_next_chr_id } = 1.

l_next_chr_id(N) :- l_found_next_chr_id(N).
l_next_chr_id(1) :- l_no_next_chr_id.

l_got_name(Name) :- mem("args.name", Name).

{ l_got_name : l_got_name(Name); l_no_name } = 1.

l_name(Name) :- l_got_name(Name).
l_name("John Doe") :- l_no_name.

l_id(N) :- l_next_chr_id(N).

story_add( character(N) ) :- l_id(N).
story_add( intrinsic( id(chr,1), name, Name ) ) :- l_name(Name).

local_mem( "result.id", N ) :- l_id(N).
