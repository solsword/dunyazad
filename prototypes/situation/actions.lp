%%%%%%%%%%%%%%%%%%%%%%
% Action Definitions %
%%%%%%%%%%%%%%%%%%%%%%

action(fall_ill).
arg(fall_ill, subject, chr).
causes(fall_ill, success, status(subject, sick)).
precondition(fall_ill, subject, _not(sick)).
harmful(fall_ill, subject).
responsible(fall_ill, fate).

action(recover).
arg(recover, subject, chr).
causes(recover, success, status(subject, _not(sick))).
precondition(recover, subject, sick).
helpful(recover, subject).
responsible(recover, fate).

action(treat_disease).
arg(treat_disease, subject, chr).
arg(treat_disease, object, chr).
causes(treat_disease, success, status(object, _not(sick))).
causes(
  treat_disease,
  success,
  relation(subject, relationship, object, acquaintances)
).
causes(
  treat_disease,
  success,
  relation(object, relationship, subject, acquaintances)
).
causes(
  treat_disease,
  success,
  relation(object, feelings, subject, likes)
).
precondition(treat_disease, object, sick).
helpful(treat_disease, object).
responsible(treat_disease, subject).

action(accident).
arg(accident, subject, chr).
causes(accident, success, status(subject, injured)).
precondition(accident, subject, _not(injured)).
harmful(accident, subject).
responsible(accident, fate).

action(treat_injury).
arg(treat_injury, subject, chr).
arg(treat_injury, object, chr).
causes(treat_injury, success, status(object, _not(injured))).
causes(
  treat_injury,
  success,
  relation(subject, relationship, object, acquaintances)
).
causes(
  treat_injury,
  success,
  relation(object, relationship, subject, acquaintances)
).
causes(
  treat_injury,
  success,
  relation(object, feelings, subject, likes)
).
precondition(treat_injury, object, injured).
helpful(treat_injury, object).
responsible(treat_injury, subject).

%action(rob).
arg(rob, subject, chr).
arg(rob, object, chr).
causes(rob, success, state_increase(subject, wealth)).
causes(rob, success, state_decrease(object, wealth)).
causes(rob, success,relation(subject, feelings, object, hates)).
precondition(rob, object, wealth, _not(peasant)).
helpful(rob, subject).
harmful(rob, object).
responsible(rob, subject).
nonreflexive(rob).

action(meet).
arg(meet, subject, chr).
arg(meet, object, chr).
causes(meet, success, relation(subject, relationship, object, acquaintances)).
causes(meet, success, relation(object, relationship, subject, acquaintances)).
precondition(fall_for, subject, relationship, object, strangers).
precondition(fall_for, object, relationship, subject, strangers).
nonreflexive(meet).

action(fall_for).
arg(fall_for, subject, chr).
arg(fall_for, object, chr).
causes(fall_for, success, relation(subject, feelings, object, loves)).
precondition(fall_for, subject, feelings, object, _not(loves)).
precondition(fall_for, subject, relationship, object, _not(strangers)).
responsible(fall_for, subject).
nonreflexive(fall_for).

action(confess_to).
arg(confess_to, subject, chr).
arg(confess_to, object, chr).
causes(confess_to, success, relation(subject, relationship, object, lovers)).
causes(confess_to, success, relation(object, relationship, subject, lovers)).
precondition(confess_to, subject, relationship, object, _not(lovers)).
precondition(confess_to, subject, feelings, object, loves).
responsible(confess_to, subject).
nonreflexive(confess_to).

%%%%%%%%%%%%%%%%%
% General Rules %
%%%%%%%%%%%%%%%%%

% general constraints:

%error(m("Nonreflexive action applied to self.", T, Act, Self)) :-
error(m("Nonreflexive action applied to self.")) :-
  event(T, Act),
  nonreflexive(Act),
  argument(T, subject, Self),
  argument(T, object, Self).

% statuses:

caused_by(changed(T, status, Subj, Status), Act) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, status(Arg, NewStatus)),
  argument(T, Arg, Subj),
  at(T, status, Subj, Status),
  1 <= { NewStatus = _not(Status); Status = _not(NewStatus) }.

at(T+1, status, Subj, NewStatus) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, status(Arg, NewStatus)),
  argument(T, Arg, Subj).

error(m("Unsatisfied status precondition", T, Act, Subj, Status)) :-
  event(T, Act),
  precondition(Act, Arg, Status),
  argument(T, Arg, Subj),
  not at(T, status, Subj, Status).

% states:

caused_by(changed(T, state, Subj, State, Value), Act) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, state(Arg, State, NewValue)),
  argument(T, Arg, Subj),
  at(T, state, Subj, State, Value),
  NewValue != Value.

at(T+1, state, Subj, State, NewValue) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, state(Arg, State, NewValue)),
  argument(T, Arg, Subj).

% state increases/decreases:

caused_by(changed(T, state, Subj, State, Current), Act) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, state_increase(Arg, State)),
  argument(T, Arg, Subj),
  at(T, state, Subj, State, Current),
  1 <= { ordered(State, Above, Current) : value(State, Above) }.

caused_by(changed(T, state, Subj, State, Current), Act) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, state_decrease(Arg, State)),
  argument(T, Arg, Subj),
  at(T, state, Subj, State, Current),
  1 <= { ordered(State, Current, Below) : value(State, Below) }.

at(T+1, state, Subj, State, Above) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, state_increase(Arg, State)),
  argument(T, Arg, Subj),
  at(T, state, Subj, State, Current),
  ordered(State, Above, Current),
  0 = { ordered(State, Other, Current) : ordered(State, Above, Other) }.

at(T+1, state, Subj, State, Below) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, state_decrease(Arg, State)),
  argument(T, Arg, Subj),
  at(T, state, Subj, State, Current),
  ordered(State, Current, Below),
  0 = { ordered(State, Current, Other) : ordered(State, Other, Below) }.

error(
  m("Unsatisfied positive state precondition", T, Act, Subj, State, Value)
) :-
  event(T, Act),
  precondition(Act, Arg, State, Value),
  argument(T, Arg, Subj),
  value(State, Value),
  not at(T, state, Subj, State, Value).

error(
  m("Unsatisfied negative state precondition", T, Act, Subj, State, Value)
) :-
  event(T, Act),
  precondition(Act, Arg, State, _not(Value)),
  argument(T, Arg, Subj),
  value(State, Value),
  at(T, state, Subj, State, Value).

% relations:

caused_by(changed(T, rel, From, Rel, To), Act) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, relation(Arg, Rel, OArg, NewValue)),
  argument(T, Arg, From),
  argument(T, OArg, To),
  at(T, rel, From, Rel, Value, To),
  NewValue != Value.

at(T+1, rel, From, Rel, NewValue, To) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, relation(Arg, Rel, OArg, NewValue)),
  argument(T, Arg, From),
  argument(T, OArg, To).

error(
  m("Unsatisfied positive relation precondition", T, Act, Subj, Rel, Obj, Value)
) :-
  event(T, Act),
  precondition(Act, Arg, Rel, OArg, Value),
  argument(T, Arg, Subj),
  argument(T, OArg, Obj),
  value(Rel, Value),
  not at(T, rel, Subj, Rel, Value, Obj).

error(
  m("Unsatisfied negative relation precondition", T, Act, Subj, Rel, Obj, Value)
) :-
  event(T, Act),
  precondition(Act, Arg, Rel, OArg, _not(Value)),
  argument(T, Arg, Subj),
  argument(T, OArg, Obj),
  value(Rel, Value),
  at(T, rel, Subj, Rel, Value, Obj).

% harmfulness/helpfulness
harmful_to(T, Subj) :-
  event(T, Act),
  harmful(Act, Arg),
  argument(T, Arg, Subj).

helpful_to(T, Subj) :-
  event(T, Act),
  helpful(Act, Arg),
  argument(T, Arg, Subj).

% responsibility
responsible_for(T, Subj) :-
  event(T, Act),
  responsible(Act, Arg),
  argument(T, Arg, Subj).

responsible_for(T, fate) :-
  event(T, Act),
  responsible(Act, fate).
