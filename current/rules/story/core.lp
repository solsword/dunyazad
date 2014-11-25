% core rules:
% -----------

:- error(Message).
%#minimize { 1@0, error(Message) : error(Message) }.

#const max_options=5.

opt_num(1..max_options).

2 <= {
  at(N, option(X)) : opt_num(X)
} <= max_options :-
  node_type(N, choice),
  story_op(N, build_options).

at(N, option(1)) :-
  node_type(N, event),
  story_op(N, build_options).

1 = {
  at(N, action(option(X), Act)) : action(Act)
} :-
  at(N, option(X)),
  story_op(N, build_options).

1 = {
  at(N, outcome(X, Outcome)) : outcome(Action, Outcome)
} :-
  at(N, action(X, Action)),
  story_op(N, build_options).

1 = {
  at(N, arg(X, Arg, inst(Type, Inst))) : st(N, inst(Type, Inst))
} :-
  at(N, action(X, Action)),
  argument(Action, Arg, Type),
  story_op(N, build_options).

at(N, initiator(X, Initiator)) :-
  at(N, action(X, Action)),
  at(N, arg(X, InitArg, Initiator)),
  initiator(Action, InitArg),
  story_op(N, build_options).

exclusive(has_item).

at(N, consequence(X, _not, relation(Rel, Old, Obj))) :-
  at(N, consequence(X, relation(Rel, New, Obj))),
  exclusive(Rel),
  st(N, relation(Rel, Old, Obj)),
  Old != New,
  story_op(N, build_options).

% Dealing or nullifying a problem/opportunity gets rid of it:
at(N, consequence(X, _not, Pt)) :-
  at(N, deals_with(X, Pt)),
  story_op(N, build_options).

at(N, consequence(X, _not, Pt)) :-
  at(N, nullifies(X, Pt)),
  story_op(N, build_options).
