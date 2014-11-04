% core rules:
% -----------

:- error(Message).

#const max_options=5.

opt_num(1..max_options).

2 <= {
  at(N, option(X)) : opt_num(X)
} <= max_options :-
  active_node(N).

1 = {
  at(N, action(option(X), Act)) : action(Act)
} :-
  at(N, option(X)),
  active_node(N).

1 = {
  at(N, outcome(X, Outcome)) : outcome(Action, Outcome)
} :-
  at(N, action(X, Action)),
  active_node(N).

1 = {
  at(N, arg(X, Arg, inst(Type, Inst))) : st(N, inst(Type, Inst))
} :-
  at(N, action(X, Action)),
  argument(Action, Arg, Type),
  active_node(N).

at(N, initiator(X, Initiator)) :-
  at(N, action(X, Action)),
  at(N, arg(X, InitArg, Initiator)),
  initiator(Action, InitArg),
  active_node(N).

exclusive(has_item).

at(N, consequence(X, _not, relation(Rel, Old, Obj))) :-
  at(N, consequence(X, relation(Rel, New, Obj))),
  exclusive(Rel),
  st(N, relation(Rel, Old, Obj)),
  Old != New,
  active_node(N).
