1 = {
  l_prot(id(chr, N)) :
    id(chr, N),
    intrinsic( id(chr, N), role, protagonist )
}.

1 = {
  l_ant(id(chr, N)) :
    id(chr, N),
    intrinsic( id(chr, N), role, antagonist )
}.

edit(some, chr, l_prot).
ensure(at(1, property(l_prot, role, protagonist))).
edit(some, chr, l_ant).
ensure(at(1, property(l_ant, role, antagonist))).

edit(append_event, l_disaster).
assert(arg(l_disaster, subject, l_ant)).
assert(arg(l_disaster, object, l_prot)).
ensure(harmful(l_disaster, l_prot)).
