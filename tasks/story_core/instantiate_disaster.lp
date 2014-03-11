{
  l_prot(id(chr, N)) :
    id(chr, N),
    intrinsic( id(chr, N), role, protagonist )
} = 1.

{
  l_ant(id(chr, N)) :
    id(chr, N),
    intrinsic( id(chr, N), role, antagonist )
} = 1.


story_add( id(evt, N) ) :- l_id(N).
story_add( intrinsic( id(chr, N), name, Name ) ) :- l_name(Name), l_id(N).
story_add( intrinsic( id(chr, N), role, Role ) ) :- l_role(Role), l_id(N).
