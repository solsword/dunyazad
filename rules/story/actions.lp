% actions.lp
% Action rules.

% Something happens at every event:

1 = {
  happens(evt(e(provocation, X)), act(Act)) : provocation_act(Act)
} :-
  evt(e(provocation, X)).

1 = {
  happens(evt(e(option, X)), act(Act)) : option_act(Act)
} :-
  evt(e(option, X)).

1 = {
  happens(evt(e(outcome, X)), act(Act)) : outcome_act(Act)
} :-
  evt(e(outcome, X)).

% Arguments:
% ----------

% Action arguments:

1 = {
  arg(evt(X), Arg, id(Type, Id)) : id(Type, Id)
} :-
  happens(evt(X), act(Act)),
  argument(Act, Arg, Type).

Min <= {
  arg(evt(X), Arg, id(Type, Id)) : id(Type, Id)
} <= Max :-
  happens(evt(X), act(Act)),
  variable_argument(Act, Arg, Type, Min, Max).

% automatic arguments:

arg(evt(X), Arg, id(Type, ID)) :-
  happens(evt(X), act(Act)),
  auto_argument(Act, Arg, Type, fl(status(Status), a(Arg))),
  holds(before, evt(X), fl(status(Status), a(id(Type, ID)))).

arg(evt(X), Arg, id(Type, ID)) :-
  happens(evt(X), act(Act)),
  auto_argument(Act, Arg, Type, fl(status(Status), a(OtherArg))),
  condition_holds(before, evt(X), fl(status(Status), a(OtherArg))),
  arg(evt(X), OtherArg, id(Type, ID)).

arg(evt(X), Arg, id(Type, ID)) :-
  happens(evt(X), act(Act)),
  auto_argument(Act, Arg, Type, fl(state(State, Value), a(Arg))),
  holds(before, evt(X), fl(state(State, Value), a(id(Type, ID)))).

arg(evt(X), Arg, id(Type, ID)) :-
  happens(evt(X), act(Act)),
  auto_argument(Act, Arg, Type, fl(state(State, Value), a(OtherArg))),
  condition_holds(before, evt(X), fl(state(State, Value), a(OtherArg))),
  arg(evt(X), OtherArg, id(Type, ID)).

arg(evt(X), Arg, id(Type, ID)) :-
  happens(evt(X), act(Act)),
  auto_argument(Act, Arg, Type, fl(rel(Rel, Value), a(Arg, OtherArg))),
  arg(evt(X), OtherArg, OtherConcrete),
  holds(before, evt(X), fl(rel(Rel, Value), a(id(Type, ID), OtherConcrete))).

arg(evt(X), Arg, id(Type, ID)) :-
  happens(evt(X), act(Act)),
  auto_argument(Act, Arg, Type, fl(rel(Rel, Value), a(OtherArg, Arg))),
  arg(evt(X), OtherArg, OtherConcrete),
  holds(before, evt(X), fl(rel(Rel, Value), a(OtherConcrete, id(Type, ID)))).

arg(evt(X), Arg, id(Type, ID)) :-
  happens(evt(X), act(Act)),
  auto_argument(Act, Arg, Type, subject, fl(rel(Rel,Value), a(SubjArg,ObjArg))),
  arg(evt(X), SubjArg, id(Type, ID)),
  condition_holds(before, evt(X), fl(rel(Rel, Value), a(SubjArg, ObjArg))).

arg(evt(X), Arg, id(Type, ID)) :-
  happens(evt(X), act(Act)),
  auto_argument(Act, Arg, Type, object, fl(rel(Rel, Value), a(SubjArg,ObjArg))),
  arg(evt(X), ObjArg, id(Type, ID)),
  condition_holds(before, evt(X), fl(rel(Rel, Value), a(SubjArg, ObjArg))).


% Causation:
% ----------
% generic 'causes' -> concrete 'caused'

caused(evt(X), fl(Fl, Args)) :-
  happens(evt(X), act(Act)),
  causes(Act, Fl, Args).

caused(evt(X), fl(Fl, Args)) :-
  happens(evt(X), act(Act)),
  causes_if(Act, Cond, Fl, Args),
  condition_holds(before, evt(X), Cond).

% state/rel relative causation

caused(evt(X), fl(state(State, NVal), a(Arg))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, increase, state(State), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))),
  state_value_order(State, PVal, NVal).

caused(evt(X), fl(state(State, NVal), a(Arg))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, decrease, state(State), a(Arg)),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))),
  state_value_order(State, NVal, PVal).

caused(evt(X), fl(rel(Rel, NVal), a(Arg1, Arg2))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, increase, rel(Rel), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))),
  rel_value_order(Rel, PVal, NVal).

caused(evt(X), fl(rel(Rel, NVal), a(Arg1, Arg2))) :-
  happens(evt(X), act(Act)),
  causes_relative(Act, decrease, rel(Rel), a(Arg1, Arg2)),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))),
  rel_value_order(Rel, NVal, PVal).

% TODO: conditional relative causation?

% state/rel relative operations min/max limits

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


% Initiation and termination:
% ---------------------------

initiates(evt(X), fl(Fl, a(Concrete))) :-
  caused(evt(X), fl(Fl, a(Arg))),
  arg(evt(X), Arg, Concrete).

initiates(evt(X), fl(Fl, a(C1, C2))) :-
  caused(evt(X), fl(Fl, a(Arg1, Arg2))),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2).

terminates(evt(X), fl(status(Prev), a(Concrete))) :-
  caused(evt(X), fl(status(Status), a(Arg))),
  arg(evt(X), Arg, Concrete),
  status_opposite(Status, Prev),
  holds(before, evt(X), fl(status(Prev), a(Concrete))).

terminates(evt(X), fl(state(State, PVal), a(Concrete))) :-
  caused(evt(X), fl(state(State, Value), a(Arg))),
  arg(evt(X), Arg, Concrete),
  holds(before, evt(X), fl(state(State, PVal), a(Concrete))),
  PVal != Value.

terminates(evt(X), fl(rel(Rel, PVal), a(C1, C2))) :-
  caused(evt(X), fl(rel(Rel, Value), a(Arg1, Arg2))),
  arg(evt(X), Arg1, C1),
  arg(evt(X), Arg2, C2),
  holds(before, evt(X), fl(rel(Rel, PVal), a(C1, C2))),
  PVal != Value.


% Conditions:
% -----------

condition_holds(When, evt(X), fl(Fl, a(Arg))) :-
  arg(evt(X), Arg, Val),
  holds(When, evt(X), fl(Fl, a(Val))).

condition_holds(When, evt(X), fl(Fl, a(A1, A2))) :-
  arg(evt(X), A1, V1),
  arg(evt(X), A2, V2),
  holds(When, evt(X), fl(Fl, a(V1, V2))).

% Threshold conditions:
% TODO: Are these too spammy?

condition_holds(
  When,
  evt(X),
  fl(greater_than(state(State, Threshold)), Args)
) :-
  condition_holds(When, evt(X), fl(state(State, Value), Args)),
  state_value_order_total(State, Threshold, Value).

condition_holds(
  When,
  evt(X),
  fl(less_than(state(State, Threshold)), Args)
) :-
  condition_holds(When, evt(X), fl(state(State, Value), Args)),
  state_value_order_total(State, Value, Threshold).

condition_holds(
  When,
  evt(X),
  fl(greater_than(rel(Rel, Threshold)), Args)
) :-
  condition_holds(When, evt(X), fl(rel(Rel, Value), Args)),
  rel_value_order_total(Rel, Threshold, Value).

condition_holds(
  When,
  evt(X),
  fl(less_than(rel(Rel, Threshold)), Args)
) :-
  condition_holds(When, evt(X), fl(rel(Rel, Value), Args)),
  rel_value_order_total(Rel, Value, Threshold).

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

% These are unnecessary as long as threshold conditions are around:
%
%error(m("'Greater than' precondition unsatisfied.", evt(X))) :-
%  happens(evt(X), act(Act)),
%  threshold_precondition(Act, greater_than, fl(state(State, Threshold), Args)),
%  0 = {
%    condition_holds(before, evt(X), fl(state(State, Value), Args))
%      : state_value_order_total(State, Threshold, Value)
%  }.
%
%error(m("'Less than' precondition unsatisfied.", evt(X))) :-
%  happens(evt(X), act(Act)),
%  threshold_precondition(Act, less_than, fl(state(State, Threshold), Args)),
%  0 = {
%    condition_holds(before, evt(X), fl(state(State, Value), Args))
%      : state_value_order_total(State, Value, Threshold)
%  }.
