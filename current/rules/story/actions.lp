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
  outcome_excludes(Action, o(Var1, Val1), o(Var2, Val2)),
  story_op(N, build_options).

% And all action arguments are filled in:
1 = {
  at(N, arg(X, Arg, Inst)) : is_instance(N, Inst, Class);
  error(m("Unbound action argument.", N, Action, Arg, Class))
} :-
  at(N, action(X, Action)),
  argument(Action, Arg, Class),
  Class != action,
  story_op(N, build_options).

1 = {
  at(N, arg(X, Arg, ActType)) : action(ActType);
  error(m("Unbound action argument.", N, Action, Arg, action))
} :-
  at(N, action(X, Action)),
  argument(Action, Arg, action),
  story_op(N, build_options).

% The concept of an "initiator:"
at(N, initiator(X, Initiator)) :-
  at(N, action(X, Action)),
  at(N, arg(X, InitArg, Initiator)),
  initiator(Action, InitArg),
  story_op(N, build_options).

% An action without an initiator is an error:
error(m("Action without initiator.", N, X)) :-
  at(N, option(X)),
  0 = {
    at(N, initiator(option(X), Anyone))
  },
  story_op(N, build_options).

% Actual consequences depend on which outcome variables have which
% values:
at(N, consequence(X, Consequence)) :-
  at(N, consequence_of(X, Outcome, Consequence)),
  at(N, outcome(X, Outcome)),
  story_op(N, build_options).

at(N, consequence(X, Modifier, Consequence)) :-
  at(N, consequence_of(X, Outcome, Modifier, Consequence)),
  at(N, outcome(X, Outcome)),
  story_op(N, build_options).

% Being injured, polymorphed, dead, or off-stage limits what actions
% you can take and which actions can be taken on you:

error(m("Injured actor initiated an action.", N, X)) :-
  at(N, action(X, Action)),
  at(N, initiator(X, Initiator)),
  st(N, state(injured, Initiator)),
  not injured_can_initiate(Action),
  story_op(N, build_options).

error(m("Interaction with dead actor.", N, X)) :-
  at(N, action(X, Action)),
  at(N, arg(X, Arg, Inst)),
  st(N, state(dead, Inst)),
  not dead_okay(Action, Arg),
  story_op(N, build_options).

error(m("Interaction with off-stage instance.", N, X)) :-
  at(N, action(X, Action)),
  at(N, arg(X, Arg, Inst)),
  st(N, state(off_stage, Inst)),
  not off_stage_okay(Action, Arg),
  story_op(N, build_options).

% Outcomes are surprising if they're unlikely based on skills/tools:
at(N, surprising(X)) :-
  at(N, outcome(X, Outcome)),
  at(N, unlikely_outcome(X, Outcome)),
  story_op(N, build_options).

% Outcomes are also surprising if there's a likely outcome but
% something else happens:
at(N, surprising(X)) :-
  at(N, likely_outcome(X, o(OutVar, Likely))),
  at(N, outcome(X, o(OutVar, NotLikely))),
  NotLikely != Likely,
  story_op(N, build_options).

% Only chaotic actions may have surprising outcomes:
% Note: now any outcome may be surprising because outcome feel
% constraints can be used to constrain surprise if desired.

%error(m("Non-chaotic action had surprising outcome.", N, X)) :-
%  at(N, action(X, Action)),
%  at(N, surprising(X)),
%  not chaotic(Action).
