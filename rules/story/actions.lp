% actions.lp
% Action rules.

1 = {
  happens(evt(X), act(Act)) : action(Act))
} :-
  evt(X).

% Action arguments:

1 = {
  arg(evt(X), Arg, id(Type, Id)) : argument(Act, Arg, Type), id(Type, Id)
} :-
  happens(evt(X), act(Act)).

% status/state/rel change operations: initiation and termination

initiates(evt(X), fl(Fl, a(Concrete)) :-
  happens(evt(X), act(Act)),
  causes(Act, Fl, a(Arg)),
  arg(evt(X), Arg, Concrete).

initiates(evt(X), fl(Fl, a(C1, C2)) :-
  happens(evt(X), act(Act)),
  causes(Act, Fl, a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1).
  arg(evt(X), Arg2, C2).

terminates(evt(X), fl(status(Prev), a(Concrete))) :-
  happens(evt(X), act(Act)),
  causes(Act, status(Status), a(Arg)),
  arg(evt(X), Arg, Concrete),
  status_opposite(Status, Prev),
  holds(before(evt(X)), fl(status(Prev), a(Concrete))).

terminates(evt(X), fl(state(State, PVal), a(Concrete))) :-
  happens(evt(X), act(Act)),
  causes(Act, state(State, Value), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before(evt(X)), fl(state(State, PVal), a(Concrete))),
  PVal != Value.

terminates(evt(X), fl(rel(Rel, PVal), a(C1, C2))) :-
  happens(evt(X), act(Act)),
  causes(Act, rel(Rel, Value), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before(evt(X)), fl(rel(Rel, PVal), a(C1, C2))),
  PVal != Value.

% Conditions:

condition_holds(before(evt(X)), fl(Fl, a(Arg))) :-
  arg(evt(X), Arg, Val),
  holds(before(evt(X)), fl(Fl, a(Val))).

condition_holds(after(evt(X)), fl(Fl, a(Arg))) :-
  arg(evt(X), Arg, Val),
  holds(after(evt(X)), fl(Fl, a(Val))).

condition_holds(before(evt(X)), fl(Fl, a(A1, A2))) :-
  arg(evt(X), A1, V1),
  arg(evt(X), A2, V2),
  holds(before(evt(X)), fl(Fl, a(V1, V2))).

condition_holds(after(evt(X)), fl(Fl, a(A1, A2))) :-
  arg(evt(X), A1, V1),
  arg(evt(X), A2, V2),
  holds(after(evt(X)), fl(Fl, a(V1, V2))).

% Precondition requirements:

error(m("Positive precondition unsatisfied.", evt(X))) :-
  happens(evt(X), act(Act)),
  precondition(Act, fl(Fl, Args)),
  not condition_holds(before(evt(X)), fl(Fl, Args)).

error(m("Negative precondition unsatisfied.", evt(X))) :-
  happens(evt(X), act(Act)),
  precondition_not(Act, fl(Fl, Args)),
  condition_holds(before(evt(X)), fl(Fl, Args)).
