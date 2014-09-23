% test.lp
% Contains extra statements for testing the system without running the whole
% python task loop.

evt(1..6).

% Defeaults:

holds(before, Beginning, fl(status(Default), a(id(Type, ID)))) :-
  first_event(Beginning),
  id(Type, ID),
  status(Type, Status),
  default_status(Status, Default).

holds(before, Beginning, fl(state(State, Default), a(id(Type, ID)))) :-
  first_event(Beginning),
  id(Type, ID),
  state(Type, State),
  default_state_value(State, Default).

holds(before, Beginning, fl(rel(Rel, Default), a(id(T1, I1), id(T2, I2)))) :-
  first_event(Beginning),
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  default_rel_value(Rel, Default).
