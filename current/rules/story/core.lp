% core rules:
% -----------

:- error(Message).
%#minimize { 1@0, error(Message) : error(Message) }.
%:- error(m("Story too short", N)).

#const max_options=5.
#const min_story_length=7. % TODO: increase this

opt_num(1..max_options).


% Core node construction:
% -----------------------

% Choice nodes have at least two options:
2 <= {
  at(N, option(X)) : opt_num(X)
} <= max_options :-
  node_type(N, choice),
  story_op(N, build_options).

% Ending and event nodes have exactly one "option:"
at(N, option(1)) :-
  node_type(N, ending),
  story_op(N, build_options).

at(N, option(1)) :-
  node_type(N, event),
  story_op(N, build_options).

% Every option has an action:
1 = {
  at(N, action(option(X), Act)) : action(Act)
} :-
  at(N, option(X)),
  story_op(N, build_options).

% And every action has an outcome:
1 = {
  at(N, outcome(X, Outcome)) : outcome(Action, Outcome)
} :-
  at(N, action(X, Action)),
  story_op(N, build_options).

% And all action arguments are filled in:
1 = {
  at(N, arg(X, Arg, inst(Type, Inst))) : st(N, inst(Type, Inst))
} :-
  at(N, action(X, Action)),
  argument(Action, Arg, Type),
  story_op(N, build_options).

1 = {
  at(N, arg(X, Arg, Symbol)) : symbol(SymType, Symbol)
} :-
  at(N, action(X, Action)),
  argument_symbol(Action, Arg, SymType),
  story_op(N, build_options).


% Misc rules:
% -----------

% The concept of an "initiator:"
at(N, initiator(X, Initiator)) :-
  at(N, action(X, Action)),
  at(N, arg(X, InitArg, Initiator)),
  initiator(Action, InitArg),
  story_op(N, build_options).

% State and action rules:
% -----------------------

% Exclusive relationships:
exclusive(has_item).

at(N, consequence(X, _not, relation(Rel, Old, Obj))) :-
  at(N, consequence(X, relation(Rel, New, Obj))),
  exclusive(Rel),
  st(N, relation(Rel, Old, Obj)),
  Old != New,
  story_op(N, build_options).

% Most actions can't be performed reflexively:
error(m("Improperly reflexive action.", N, Opt)) :-
  at(N, action(Opt, Action)),
  at(N, arg(Opt, Arg1, inst(actor, ID))),
  at(N, arg(Opt, Arg2, inst(actor, ID))),
  Arg1 < Arg2,
  not reflexive(Action).
