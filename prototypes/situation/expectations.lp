% Expectations

% satisfaction/dissatisfaction:

satisfied(expectation(T, Condition)) :-
  expectation(T, Condition),
  at(X, Condition),
  X > T.

unmet(expectation(T, Condition)) :-
  expectation(T, Condition),
  not satisfied(expectation(T, Condition)).

% limit how many unmet expectations are allowed:

undesirable(10, m("Surplus unmet expectation", T, C)) :-
  unmet(expectation(T, C)),
  unmet_expectation_limit(L),
  L < { unmet(expectation(X, Y)) : expectation(X, Y) }.

% we expect things will get better:
expectation(T, status(id(Type, ID), _not(Status))) :-
  at(T, status(id(Type, ID), Status)),
  negative(Type, Status).

% we expect that money is borrowed for a reason:
expectation(T, state(Subj, wealth, Original)) :-
  at(T, state(Subj, wealth, Original)),
  event(T, borrow_money_from),
  argument(T, subject, Subj).
