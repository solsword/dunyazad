% Locals

arg_default(name, "merquivest").
arg_default(role, "stranger").

% Edits

edit(new, chr, l_newchr).
assert(at(1, property(l_newchr, name, Name))) :- task_arg(name, Name).
assert(at(1, property(l_newchr, role, Role))) :- task_arg(role, Role).

local_mem( "result.id", N ) :- bound(l_newchr, id(chr, N)).
