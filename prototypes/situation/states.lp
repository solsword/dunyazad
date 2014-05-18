%%%%%%%%%%%%%%%%%%%%%
% State Definitions %
%%%%%%%%%%%%%%%%%%%%%

status(chr, sick).
default(chr, _not(sick)).

status(chr, injured).
default(chr, _not(injured)).

state(chr, wealth).
value(wealth, peasant).
value(wealth, merchant).
value(wealth, noble).
value(wealth, king).

default(chr, wealth, peasant).
default(chr, wealth, merchant).

ordered(wealth, king, noble).
ordered(wealth, noble, merchant).
ordered(wealth, merchant, peasant).

state(chr, status).
value(status, peasant).
value(status, merchant).
value(status, noble).
value(status, king).

default(chr, status, peasant).
default(chr, status, merchant).

ordered(status, king, noble).
ordered(status, noble, merchant).
ordered(status, merchant, peasant).

relation(chr, chr, feelings).
value(feelings, ambivalent).
value(feelings, hates).
value(feelings, likes).
value(feelings, loves).

nonreflexive(chr, chr, feelings).
default(chr, chr, feelings, ambivalent).

relation(chr, chr, relationship).
value(relationship, strangers).
value(relationship, acquaintances).
value(relationship, enemies).
value(relationship, friends).
value(relationship, lovers).
value(relationship, family).

nonreflexive(chr, chr, relationship).
reciprocal(chr, chr, relationship).
default(chr, chr, relationship, strangers).

%%%%%%%%%%%%%%%%%
% General Rules %
%%%%%%%%%%%%%%%%%

% statuses

ordered(State, V1, V2) :-
  ordered(State, V1, Vo),
  ordered(State, Vo, V2).

1 = {
  error(m("Unable to assign status.", T, id(Type, ID), Status));
  at(T, status, id(Type, ID), Status);
  at(T, status, id(Type, ID), _not(Status))
} :-
  status(Type, Status),
  id(Type, ID),
  time(T).

at(B, status, id(Type, ID), Default) :-
  beginning(B),
  id(Type, ID),
  default(Type, Default).

changed(T, status, id(Type, ID), Status) :-
  at(T, status, id(Type, ID), Status),
  at(T+1, status, id(Type, ID), _not(Status)),
  time(T), time(T+1).

changed(T, status, id(Type, ID), _not(Status)) :-
  at(T, status, id(Type, ID), _not(Status)),
  at(T+1, status, id(Type, ID), Status),
  time(T), time(T+1).

% states

1 = {
  error(m("Unable to assign state.", T, id(Type, ID), State));
  at(T, state, id(Type, ID), State, Value) : value(State, Value)
} :-
  state(Type, State),
  id(Type, ID),
  time(T).

1 = {
  error(m("Unable to assign default state.", B, id(Type, ID), State));
  at(B, state, id(Type, ID), State, Default) : default(Type, State, Default)
} :-
  id(Type, ID),
  state(Type, State),
  beginning(B).

changed(T, state, id(Type, ID), State) :-
  at(T, state, id(Type, ID), State, Value),
  at(T+1, state, id(Type, ID), State, Other),
  Value != Other,
  time(T), time(T+1).

% relations

1 = {
  error(m("Unable to assign relation.", T, id(T1, ID), id(T2, To), Rel));
  at(T, rel, id(T1, ID), Rel, Value, id(T2, To)) : value(Rel, Value)
} :-
  id(T1, ID),
  id(T2, To),
  relation(T1, T2, Rel),
  time(T),
  1 <= { id(T1, ID) != id(T2, To); not nonreflexive(T1, T2, Rel) }.

1 = {
  error(m("Unable to assign default relation.", B, id(T1, ID), id(T2, To),Rel));
  at(B, rel, id(T1, ID), Rel, Default, id(T2, To))
  : default(T1, T2, Rel, Default)
} :-
  id(T1, ID),
  id(T2, To),
  relation(T1, T2, Rel),
  beginning(B),
  1 <= { id(T1, ID) != id(T2, To); not nonreflexive(T1, T2, Rel) }.

changed(T, rel, id(T1, ID), Rel, id(T2, To)) :-
  id(T1, ID),
  id(T2, To),
  at(T, rel, id(Type, ID), Rel, Value, id(T2, To)),
  at(T+1, rel, id(Type, ID), Rel, Other, id(T2, To)),
  Value != Other,
  time(T), time(T+1).

%error(
%  m(
%    "Reciprocality of relationship broken.",
%    T, id(Type, From), Rel, Value, id(Type, To)
%  )
%) :-
error(m("Reciprocality of relationship broken.")) :-
  at(T, rel, id(Type, From), Rel, Value, id(Type, To)),
  at(T, rel, id(Type, To), Rel, Other, id(Type, From)),
  reciprocal(Type, Type, Rel),
  Value != Other.

% there shouldn't be uncaused changes:

error(m("Uncaused status change.", T, Subj, Status)) :-
  changed(T, status, Subj, Status),
  0 = { caused_by(changed(T, status, Subj, Status), Cause) }.

error(m("Uncaused state change.", T, Subj, State, Value, Other)) :-
  changed(T, state, Subj, State),
  at(T, state, Subj, State, Value),
  at(T+1, state, Subj, State, Other),
  0 = { caused_by(changed(T, state, Subj, State, Value), Cause) }.

error(m("Uncaused state change.", T, From, Rel, Val, To, Other)) :-
  changed(T, rel, From, Rel, To),
  at(T, rel, From, Rel, Val, To),
  at(T+1, rel, From, Rel, Other, To),
  0 = { caused_by(changed(T, rel, From, Rel, To), Cause) }.
