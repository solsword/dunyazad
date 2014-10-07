% core rules:
% -----------

#const max_options=5.

2 <= {
  option(X) : X = 1..max_options
} <= max_options.

1 = {
  action(option(X), Act) : action(Act)
} :-
  option(X).

1 = {
  outcome(X, Outcome) : outcome(Action, Outcome)
} :-
  action(X, Action).

1 = {
  arg(X, Arg, inst(Type, Inst)) : inst(Type, Inst)
} :-
  action(X, Action),
  argument(Action, Arg, Type).

initiator(X, Initiator) :-
  action(X, Action),
  initiator(Action, InitArg),
  arg(X, InitArg, Initiator).
