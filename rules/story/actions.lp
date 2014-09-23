% actions.lp
% Action rules.

1 = {
  happens(evt(X), act(Act)) : action(Act)
} :-
  evt(X).

% Action arguments:

1 = {
  arg(evt(X), Arg, id(Type, Id)) : argument(Act, Arg, Type), id(Type, Id)
} :-
  happens(evt(X), act(Act)).

% status/state/rel change operations: initiation and termination

initiates(evt(X), fl(Fl, a(Concrete))) :-
  happens(evt(X), act(Act)),
  causes(Act, Fl, a(Arg)),
  arg(evt(X), Arg, Concrete).

initiates(evt(X), fl(Fl, a(C1, C2))) :-
  happens(evt(X), act(Act)),
  causes(Act, Fl, a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2).

terminates(evt(X), fl(status(Prev), a(Concrete))) :-
  happens(evt(X), act(Act)),
  causes(Act, status(Status), a(Arg)),
  arg(evt(X), Arg, Concrete),
  status_opposite(Status, Prev),
  holds(before, evt(X), fl(status(Prev), a(Concrete))).

terminates(evt(X), fl(state(State, PVal), a(Concrete))) :-
  happens(evt(X), act(Act)),
  causes(Act, state(State, Value), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))),
  PVal != Value.

terminates(evt(X), fl(rel(Rel, PVal), a(C1, C2))) :-
  happens(evt(X), act(Act)),
  causes(Act, rel(Rel, Value), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))),
  PVal != Value.

% state/rel relative operations: initiation and termination

initiates(evt(X), fl(state(State, NVal), a(Concrete))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, increase, state(State), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))),
  state_value_order(State, PVal, NVal).

initiates(evt(X), fl(state(State, NVal), a(Concrete))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, decrease, state(State), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))),
  state_value_order(State, NVal, PVal).

initiates(evt(X), fl(rel(Rel, NVal), a(C1, C2))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, increase, rel(Rel), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))),
  rel_value_order(Rel, PVal, NVal).

initiates(evt(X), fl(rel(Rel, NVal), a(C1, C2))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, decrease, rel(Rel), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))),
  rel_value_order(Rel, NVal, PVal).

terminates(evt(X), fl(state(State, PVal), a(Concrete))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, Direction, state(State), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))).

terminates(evt(X), fl(rel(Rel, PVal), a(C1, C2))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, Direction, rel(Rel), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))).

error(m("Can't increase state above max.", evt(X), State, PVal)) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, increase, state(State), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))),
  0 = { state_value_order(State, PVal, AnyVal) : state_value(State, AnyVal) }.

error(m("Can't decrease state below min.", evt(X), State, PVal)) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, decrease, state(State), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))),
  0 = { state_value_order(State, AnyVal, PVal) : state_value(State, AnyVal) }.

error(m("Can't increase rel above max.", evt(X), Rel, PVal)) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, increase, rel(Rel), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))),
  0 = { rel_value_order(Rel, PVal, AnyVal) : rel_value(Rel, AnyVal) }.

error(m("Can't decrease rel below min.", evt(X), Rel, PVal)) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, decrease, rel(Rel), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))),
  0 = { rel_value_order(Rel, AnyVal, PVal) : rel_value(Rel, AnyVal) }.

% Conditions:

condition_holds(When, evt(X), fl(Fl, a(Arg))) :-
  arg(evt(X), Arg, Val),
  holds(When, evt(X), fl(Fl, a(Val))).

condition_holds(When, evt(X), fl(Fl, a(A1, A2))) :-
  arg(evt(X), A1, V1),
  arg(evt(X), A2, V2),
  holds(When, evt(X), fl(Fl, a(V1, V2))).

% Automatic conditions:

condition_holds(When, evt(X), fl(colocated, a(Arg1, Arg2))) :-
  arg(evt(X), Arg1, Chr1),
  arg(evt(X), Arg2, Chr2),
  Chr1 != Chr2,
  holds(When, evt(X), fl(rel(located_at, true), a(Chr1, Loc))),
  holds(When, evt(X), fl(rel(located_at, true), a(Chr2, Loc))).

% Precondition requirements:

error(m("Positive precondition unsatisfied.", evt(X))) :-
  happens(evt(X), act(Act)),
  precondition(Act, fl(Fl, Args)),
  not condition_holds(before, evt(X), fl(Fl, Args)).

error(m("Negative precondition unsatisfied.", evt(X))) :-
  happens(evt(X), act(Act)),
  precondition_not(Act, fl(Fl, Args)),
  condition_holds(before, evt(X), fl(Fl, Args)).
