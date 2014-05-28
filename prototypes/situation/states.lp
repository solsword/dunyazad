% ordering:

ordered(State, V1, V2) :-
  ordered(State, V1, Vo),
  ordered(State, Vo, V2).

next_higher(State, Lower, Higher) :-
  ordered(State, Lower, Higher),
  0 = { ordered(State, Lower, Inbetween) : ordered(State, Inbetween, Higher) }.

next_lower(State, Higher, Lower) :-
  next_higher(State, Lower, Higher).

% statuses

1 = {
  error(m("Unable to assign status.", T, id(Type, ID), Status));
  at(T, status(id(Type, ID), Status));
  at(T, status(id(Type, ID), _not(Status)))
} :-
  status(Type, Status),
  id(Type, ID),
  time(T).

at(B, status(id(Type, ID), Default)) :-
  beginning(B),
  id(Type, ID),
  default(Type, Default).

changed(T, status(id(Type, ID), Status)) :-
  at(T, status(id(Type, ID), Status)),
  at(T+1, status(id(Type, ID), _not(Status))),
  time(T), time(T+1).

changed(T, status(id(Type, ID), _not(Status))) :-
  at(T, status(id(Type, ID), _not(Status))),
  at(T+1, status(id(Type, ID), Status)),
  time(T), time(T+1).

% states

1 = {
  error(m("Unable to assign state.", T, id(Type, ID), State));
  at(T, state(id(Type, ID), State, Value)) : value(State, Value)
} :-
  state(Type, State),
  id(Type, ID),
  time(T).

1 = {
  error(m("Unable to assign default state.", B, id(Type, ID), State));
  at(B, state(id(Type, ID), State, Default)) : default(Type, State, Default)
} :-
  id(Type, ID),
  state(Type, State),
  beginning(B).

changed(T, state(id(Type, ID), State, Value)) :-
  at(T, state(id(Type, ID), State, Value)),
  at(T+1, state(id(Type, ID), State, Other)),
  Value != Other,
  time(T), time(T+1).

% relations

1 = {
  error(m("Unable to assign relation.", T, id(T1, ID), id(T2, To), Rel));
  at(T, rel(id(T1, ID), id(T2, To), Rel, Value)) : value(Rel, Value)
} :-
  id(T1, ID),
  id(T2, To),
  relation(T1, T2, Rel),
  time(T),
  1 <= { id(T1, ID) != id(T2, To); not nonreflexive(T1, T2, Rel) }.

1 = {
  error(m("Unable to assign default relation.", B, id(T1, ID), id(T2, To),Rel));
  at(B, rel(id(T1, ID), id(T2, To), Rel, Default))
  : default(T1, T2, Rel, Default)
} :-
  id(T1, ID),
  id(T2, To),
  relation(T1, T2, Rel),
  beginning(B),
  1 <= { id(T1, ID) != id(T2, To); not nonreflexive(T1, T2, Rel) }.

changed(T, rel(id(T1, ID), id(T2, To), Rel, Value)) :-
  id(T1, ID),
  id(T2, To),
  at(T, rel(id(Type, ID), id(T2, To), Rel, Value)),
  at(T+1, rel(id(Type, ID), id(T2, To), Rel, Other)),
  Value != Other,
  time(T), time(T+1).

%error(
%  m(
%    "Reciprocality of relationship broken.",
%    T, id(Type, From), Rel, Value, id(Type, To)
%  )
%) :-
error(m("Reciprocality of relationship broken.")) :-
  at(T, rel(id(Type, From), id(Type, To), Rel, Value)),
  at(T, rel(id(Type, To), id(Type, From), Rel, Other)),
  reciprocal(Type, Type, Rel),
  Value != Other.

% there shouldn't be uncaused changes:

error(m("Uncaused change.", T, Fluent)) :-
  changed(T, Fluent),
  0 = { caused_by_something(changed(T, Fluent)) }.
