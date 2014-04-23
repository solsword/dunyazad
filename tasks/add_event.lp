% Arguments

argument(type, "unknown").

% Edits

edit(append_event, l_new_event).
assert(arg(l_new_event, type, Type)) :- arg_value(type, Type).

% Results

result(id, ID) :- bound(l_new_event, id(evt, ID)).
