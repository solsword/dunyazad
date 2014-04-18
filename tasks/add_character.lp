% Locals

l_name(Name) :- mem("args.name", Name).
{ l_name("merquivest") }.

:- { l_name(Name) : l_name(Name) } > 1.
:- { l_name(Name) : l_name(Name) } < 1.

l_role(Role) :- mem("args.role", Role).
{ l_role(stranger) }.

:- { l_role(Role) : l_role(Role) } > 1.
:- { l_role(Role) : l_role(Role) } < 1.

% Edits

edit(new, chr, l_newchr).
assert(at(1, property(l_newchr, name, Name))) :- l_name(Name).
assert(at(1, property(l_newchr, role, Role))) :- l_role(Role).

local_mem( "result.id", N ) :- bound(l_newchr, id(chr, N)).
