% Shorthand for action setups.

% initiates_acquaintanceship causes the subject and object to become acquainted
causes_if(
  Action,
  Outcome,
  rel(object, subject, relationship, strangers),
  rel(object, subject, relationship, acquaintances)
) :-
  causes(Action, Outcome, Something),
  initiates_acquaintanceship(Action).

causes_if(
  Action,
  Outcome,
  rel(subject, object, relationship, strangers),
  rel(subject, object, relationship, acquaintances)
) :-
  causes(Action, Outcome, Something),
  initiates_acquaintanceship(Action).

% Action implementation.

% "conditions" as states over arguments:

condition_holds(T, status(Arg, Status)) :-
  argument(T, Arg, Subj),
  at(T, status(Subj, Status)).
condition_holds_after(T, status(Arg, Status)) :-
  argument(T, Arg, Subj),
  at(T+1, status(Subj, Status)).

condition_holds(T, state(Arg, State, Value)) :-
  argument(T, Arg, Subj),
  at(T, state(Subj, State, Value)).
condition_holds_after(T, state(Arg, State, Value)) :-
  argument(T, Arg, Subj),
  at(T+1, state(Subj, State, Value)).

condition_holds(T, state_increase(Arg, State)) :-
  argument(T, Arg, Subj),
  at(T-1, state(Subj, State, PVal)),
  at(T, state(Subj, State, NVal)),
  next_higher(State, PVal, NVal).
condition_holds_after(T, state_increase(Arg, State)) :-
  argument(T, Arg, Subj),
  at(T, state(Subj, State, PVal)),
  at(T+1, state(Subj, State, NVal)),
  next_higher(State, PVal, NVal).

condition_holds(T, state_decrease(Arg, State)) :-
  argument(T, Arg, Subj),
  at(T-1, state(Subj, State, PVal)),
  at(T, state(Subj, State, NVal)),
  next_lower(State, PVal, NVal).
condition_holds_after(T, state_decrease(Arg, State)) :-
  argument(T, Arg, Subj),
  at(T, state(Subj, State, PVal)),
  at(T+1, state(Subj, State, NVal)),
  next_lower(State, PVal, NVal).

condition_holds(T, rel(Arg, OArg, Rel, Value)) :-
  argument(T, Arg, Subj),
  argument(T, OArg, Obj),
  at(T, rel(Subj, Obj, Rel, Value)).
condition_holds_after(T, rel(Arg, OArg, Rel, Value)) :-
  argument(T, Arg, Subj),
  argument(T, OArg, Obj),
  at(T+1, rel(Subj, Obj, Rel, Value)).

condition_holds(T, rel_category(Arg, OArg, Rel, Category)) :-
  argument(T, Arg, id(T1, S)),
  argument(T, OArg, id(T2, O)),
  at(T, rel(id(T1, S), id(T2, O), Rel, Value)),
  category(T1, T2, Rel, Value, Category).
condition_holds_after(T, rel_category(Arg, OArg, Rel, Category)) :-
  argument(T, Arg, id(T1, S)),
  argument(T, OArg, id(T2, O)),
  at(T+1, rel(id(T1, S), id(T2, O), Rel, Value)),
  category(T1, T2, Rel, Value, Category).

% causality (note: this is enforced below for now):
error(m("Violated causality.", T, Condition)) :-
  causes(T, Condition),
  not condition_holds_after(T, Condition).

% TODO: This shouldn't be necessary:
%:- error(m("Violated causality.", T, Act, Condition)).
%:- error(m("Violated causality.", T, Act, Condition)).
%:- 3 < {
%  error(m("Violated causality.", T, Act, Condition)) :
%    error(m("Violated causality.", T, Act, Condition))
%}.

% general constraints:

%error(m("Nonreflexive action applied to self.", T, Act, Self)) :-
error(m("Nonreflexive action applied to self.", T)) :-
  event(T, Act),
  nonreflexive(Act),
  argument(T, subject, Self),
  argument(T, object, Self).

% positive preconditions:
% general
error(m("Unsatisfied precondition", T, Act, Condition)) :-
  event(T, Act),
  precondition(Act, Condition),
  not condition_holds(T, Condition).
% outcome-specific
error(m("Unsatisfied outcome precondition", T, Act, Outcome, Condition)) :-
  event(T, Act),
  outcome(T, Outcome),
  precondition(Act, Outcome, Condition),
  not condition_holds(T, Condition).

% negative preconditions:
% general
error(m("Unsatisfied negative precondition", T, Act, _not(Condition))) :-
  event(T, Act),
  precondition_not(Act, Condition),
  condition_holds(T, Condition).
% outcome-specific
error(
  m(
    "Unsatisfied negative outcome precondition",
    T,
    Act,
    Outcome,
    _not(Condition)
  )
) :-
  event(T, Act),
  outcome(T, Outcome),
  precondition_not(Act, Outcome, Condition),
  not condition_holds(T, Condition).

% statuses:

caused_by_something(changed(T, status(Subj, Status))) :-
  causes(T, status(Arg, NewStatus)),
  argument(T, Arg, Subj),
  at(T, status(Subj, Status)),
  1 <= { NewStatus = _not(Status); Status = _not(NewStatus) }.

%at(T+1, status(Subj, NewStatus)) :-
%  causes(T, status(Arg, NewStatus)),
%  argument(T, Arg, Subj).

% states:

caused_by_something(changed(T, state(Subj, State, Value))) :-
  causes(T, state(Arg, State, NewValue)),
  argument(T, Arg, Subj),
  at(T, state(Subj, State, Value)),
  NewValue != Value.

%at(T+1, state(Subj, State, NewValue)) :-
%  causes(T, state(Arg, State, NewValue)),
%  argument(T, Arg, Subj).

% state increases/decreases:

caused_by_something(changed(T, state(Subj, State, Current))) :-
  causes(T, state_increase(Arg, State)),
  argument(T, Arg, Subj),
  at(T, state(Subj, State, Current)),
  at(T+1, state(Subj, State, Above)),
  next_higher(State, Current, Above).

caused_by_something(changed(T, state(Subj, State, Current))) :-
  causes(T, state_decrease(Arg, State)),
  argument(T, Arg, Subj),
  at(T, state(Subj, State, Current)),
  at(T+1, state(Subj, State, Below)),
  next_lower(State, Current, Below).

%at(T+1, state(Subj, State, Above)) :-
%  causes(T, state_increase(Arg, State)),
%  argument(T, Arg, Subj),
%  at(T, state(Subj, State, Current)),
%  next_higher(State, Current, Above).
%
%at(T+1, state(Subj, State, Below)) :-
%  causes(T, state_decrease(Arg, State)),
%  argument(T, Arg, Subj),
%  at(T, state(Subj, State, Current)),
%  next_lower(State, Current, Below).

% relations:

caused_by_something(changed(T, rel(From, To, Rel, Value))) :-
  causes(T, rel(Arg, OArg, Rel, NewValue)),
  argument(T, Arg, From),
  argument(T, OArg, To),
  at(T, rel(From, To, Rel, Value)),
  NewValue != Value.

%at(T+1, rel(From, To, Rel, NewValue)) :-
%  causes(T, rel(Arg, OArg, Rel, NewValue)),
%  argument(T, Arg, From),
%  argument(T, OArg, To).

% conditional causality

% unconditional:
causes(T, Result) :-
  event(T, Act),
  outcome(T, Outcome),
  causes(Act, Outcome, Result).

% conditional:
causes(T, Result) :-
  event(T, Act),
  condition_holds(T, Condition),
  causes_if(Act, Outcome, Condition, Result).
causes(T, Result) :-
  event(T, Act),
  not condition_holds(T, Condition),
  causes_ifnot(Act, Outcome, Condition, Result).

% nondeterministic:
0 <= { causes(T, Result) } <= 1 :-
  event(T, Act),
  causes_maybe(Act, Outcome, Result).

% conditional nondeterministic:
0 <= { causes(T, Result) } <= 1 :-
  event(T, Act),
  condition_holds(T, Condition),
  causes_maybe_if(Act, Outcome, Condition, Result).
0 <= { causes(T, Result) } <= 1 :-
  event(T, Act),
  not condition_holds(T, Condition),
  causes_maybe_ifnot(Act, Outcome, Condition, Result).

% harmfulness/helpfulness
harmful_to(T, Subj) :-
  event(T, Act),
  harmful(Act, Arg),
  argument(T, Arg, Subj).

helpful_to(T, Subj) :-
  event(T, Act),
  helpful(Act, Arg),
  argument(T, Arg, Subj).

harmful_to(T, Subj) :-
  event(T, Act),
  outcome(T, Outcome),
  harmful(Act, Outcome, Arg),
  argument(T, Arg, Subj).

helpful_to(T, Subj) :-
  event(T, Act),
  outcome(T, Outcome),
  harmful(Act, Outcome, Arg),
  argument(T, Arg, Subj).

% solving/creating problems
causes_problem_for(T, id(Type, O)) :-
  changed(T, status(id(Type, O), Status)),
  at(T+1, status(id(Type, O), _not(Status))),
  negative(Type, _not(Status)).

causes_problem_for(T, id(Type, O)) :-
  changed(T, status(id(Type, O), _not(Status))),
  at(T+1, status(id(Type, O), Status)),
  negative(Type, Status).

causes_problem_for(T, id(Type, O)) :-
  changed(T, state(id(Type, O), State, Value)),
  at(T+1, state(id(Type, O), State, NewValue)),
  negative(Type, State, NewValue).

causes_problem_for(T, id(T1, S)) :-
  changed(T, rel(id(T1, S), id(T2, O), Rel, Value)),
  at(T+1, rel(id(T1, S), id(T2, O), Rel, NewValue)),
  negative(T1, T2, Rel, NewValue).

solves_problem_for(T, id(Type, O)) :-
  changed(T, status(id(Type, O), Status)),
  at(T+1, status(id(Type, O), _not(Status))),
  negative(Type, Status),
  not negative(Type, _not(Status)).

solves_problem_for(T, id(Type, O)) :-
  changed(T, status(id(Type, O), _not(Status))),
  at(T+1, status(id(Type, O), Status)),
  negative(Type, _not(Status)),
  not negative(Type, Status).

solves_problem_for(T, id(Type, O)) :-
  changed(T, state(id(Type, O), State, Value)),
  at(T+1, state(id(Type, O), State, NewValue)),
  negative(Type, State, Value),
  not negative(Type, State, NewValue).

solves_problem_for(T, id(T1, S)) :-
  changed(T, rel(id(T1, S), id(T2, O), Rel, Value)),
  at(T+1, rel(id(T1, S), id(T2, O), Rel, NewValue)),
  negative(T1, T2, Rel, Value),
  not negative(T1, T2, Rel, NewValue).

% responsibility
responsible_for(T, Subj) :-
  event(T, Act),
  responsible(Act, Arg),
  argument(T, Arg, Subj).

responsible_for(T, fate) :-
  event(T, Act),
  responsible(Act, fate).
