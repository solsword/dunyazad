% actions.lp
% Rules about actions.

% Every option has an action:
1 = {
  at(N, action(option(X), Act)) : action(Act);
  error(m("Option without action.", N, option(X)))
} :-
  at(N, option(X)),
  story_op(N, build_options).

% And every action has a value for each outcome variable:
outcome_variable(Action, Var) :-
  outcome_val(Action, Var, Value).

1 = {
  at(N, outcome(X, o(Var, Val))) : outcome_val(Action, Var, Val);
  error(m("Unassigned outcome variable.", N, X, Action, Var))
} :-
  at(N, action(X, Action)),
  outcome_variable(Action, Var),
  story_op(N, build_options).

error(m("Exclusive outcome values.", N, X)) :-
  at(N, action(X, Action)),
  at(N, outcome(X, o(Var1, Val1))),
  at(N, outcome(X, o(Var2, Val2))),
  Var1 != Var2,
  outcome_excludes(Action, o(Var1, Val1), o(Var2, Val2)).

% And all action arguments are filled in:
1 = {
  at(N, arg(X, Arg, Inst)) : is_instance(N, Inst, Class);
  error(m("Unbound action argument.", N, Action, Arg, Class))
} :-
  at(N, action(X, Action)),
  argument(Action, Arg, Class),
  story_op(N, build_options).

% Actual consequences depend on which outcome variables have which values:
at(N, consequence(X, Consequence)) :-
  at(N, consequence_of(X, Outcome, Consequence)),
  at(N, outcome(X, Outcome)).

at(N, consequence(X, _not, Consequence)) :-
  at(N, consequence_of(X, Outcome, _not, Consequence)),
  at(N, outcome(X, Outcome)).

%at(N, abs_arg(X, c(Constant), Constant)) :-
%  at(N, option(X)),
%  needs_binding(N, X, c(Constant)).
%
%at(N, abs_arg(X, v(Arg), Value)) :-
%  at(N, arg(X, Arg, Value)),
%  needs_binding(N, X, v(Arg)).
%
%1 = {
%  at(N, abs_arg(X, a(Type), inst(Type, Inst))) : at(N, inst(Type, Inst));
%  error(m("Error binding abstract argument.", N, X, a(Type)))
%} :-
%  needs_binding(N, X, a(Type)).
%
%at(N, abs_arg(X, e(Type), inst(Type, Inst))) :-
%  at(N, inst(Type, Inst)),
%  needs_binding(N, X, e(Type)).
%
%needs_binding(N, X, SAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, state(SAA, IAA)).
%
%needs_binding(N, X, IAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, state(SAA, IAA)).
%
%at(N, consequence(X, state(State, Inst))) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, state(SAA, IAA)),
%  at(N, abs_arg(X, SAA, State)),
%  at(N, abs_arg(X, IAA, Inst)).
%
%needs_binding(N, X, SAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, _not, state(SAA, IAA)).
%
%needs_binding(N, X, IAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, _not, state(SAA, IAA)).
%
%at(N, consequence(X, _not, state(State, Inst))) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, _not, state(SAA, IAA)),
%  at(N, abs_arg(X, SAA, State)),
%  at(N, abs_arg(X, IAA, Inst)),
%  st(N, state(State, Inst)).
%
%needs_binding(N, X, PAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, property(PAA, IAA, VAA)).
%
%needs_binding(N, X, IAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, property(PAA, IAA, VAA)).
%
%needs_binding(N, X, VAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, property(PAA, IAA, VAA)).
%
%at(N, consequence(X, property(Property, Inst, Value))) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, property(PAA, IAA, VAA)),
%  at(N, abs_arg(X, PAA, Property)),
%  at(N, abs_arg(X, IAA, Inst)),
%  at(N, abs_arg(X, VAA, Value)).
%
%at(N, consequence(X, _not, property(Property, Inst, Value))) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, _not, property(PAA, IAA, VAA)),
%  at(N, abs_arg(X, PAA, Property)),
%  at(N, abs_arg(X, IAA, Inst)),
%  at(N, abs_arg(X, VAA, Value)),
%  st(N, property(Property, Inst, Value)).
%
%needs_binding(N, X, RAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, relation(RAA, FAA, TAA)).
%
%needs_binding(N, X, FAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, relation(RAA, FAA, TAA)).
%
%needs_binding(N, X, TAA) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, relation(RAA, FAA, TAA)).
%
%at(N, consequence(X, relation(Relation, From, To))) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, relation(RAA, FAA, TAA)),
%  at(N, abs_arg(X, RAA, Relation)),
%  at(N, abs_arg(X, FAA, From)),
%  at(N, abs_arg(X, TAA, To)).
%
%at(N, consequence(X, _not, relation(Relation, From, To))) :-
%  at(N, action(X, Action)),
%  at(N, outcome(X, Outcome)),
%  consequence(Action, Outcome, _not, relation(RAA, FAA, TAA)),
%  at(N, abs_arg(X, RAA, Relation)),
%  at(N, abs_arg(X, FAA, From)),
%  at(N, abs_arg(X, TAA, To)),
%  st(N, relation(Relation, From, To)).
