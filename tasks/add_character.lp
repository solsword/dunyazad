% Arguments

argument(name, "merquivest").
argument(role, "stranger").

% Edits

edit(new, chr, l_newchr).
assert(arg(l_newchr, name, Name)) :- arg_value(name, Name).
assert(arg(l_newchr, role, Role)) :- arg_value(role, Role).

% Results

result(id, N) :- bound(l_newchr, id(chr, N)).
