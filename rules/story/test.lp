% test.lp
% Contains extra statements for testing the system without running the whole
% python task loop.

% test code:

%:- 0 = { arg(evt(X), witnesses, Y) : evt(X) }.
%:- 0 = { happens(Evt, act(m_gossip)) }.

% Situation setup:

% The series of events:
evt(0..6).

% The party of characters:
id(chr, 0..4).

%name(id(chr, 0), "Arel").
holds(before, evt(0), fl(rel(chr_at, true), a(id(chr, 0), id(loc, 0)))).
holds(before, evt(0), fl(rel(chr_at, true), a(id(chr, 1), id(loc, 0)))).
holds(before, evt(0), fl(rel(chr_at, true), a(id(chr, 2), id(loc, 0)))).
holds(before, evt(0), fl(rel(chr_at, true), a(id(chr, 3), id(loc, 0)))).
holds(before, evt(0), fl(rel(chr_at, true), a(id(chr, 4), id(loc, 0)))).

% There's a single monster:
id(monster, 0).
holds(before, evt(0), fl(rel(mnst_at, true), a(id(monster, 0), id(loc, 3)))).

% There's a key item:
id(itm, 0).
holds(before, evt(0), fl(rel(itm_at, true), a(id(itm, 0), id(loc, 2)))).

% And some locations:
id(loc, 0..5).

% Defaults:

holds(before, Beginning, fl(status(Default), a(id(Type, ID)))) :-
  first_event(Beginning),
  id(Type, ID),
  status(Type, Status),
  default_status(Status, Default),
  0 = {
    holds(before, Beginning, fl(status(NonDefault), a(id(Type, ID))))
      : status_opposite(Default, NonDefault)
  }.

holds(before, Beginning, fl(state(State, Default), a(id(Type, ID)))) :-
  first_event(Beginning),
  id(Type, ID),
  state(Type, State),
  default_state_value(State, Default),
  0 = {
    holds(before, Beginning, fl(state(State, NonDefault), a(id(Type, ID))))
      : state_value(State, NonDefault), NonDefault != Default
  }.

holds(before, Beginning, fl(rel(Rel, Default), a(id(T1, I1), id(T2, I2)))) :-
  first_event(Beginning),
  id(T1, I1),
  id(T2, I2),
  relationship(T1, T2, Rel),
  default_rel_value(Rel, Default),
  0 = {
    holds(
      before,
      Beginning,
      fl(rel(Rel, NonDefault), a(id(T1, I1), id(T2, I2)))
    )
      : rel_value(Rel, NonDefault), NonDefault != Default
  }.
