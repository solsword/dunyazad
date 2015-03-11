% eval.lp
% Rules dealing with how choice structures are evaluated.

% Expectations:
%--------------

option_relevance(irrelevant).
option_relevance(threatens).
option_relevance(enables).
option_relevance(fails).
option_relevance(achieves).

% Options which have no other expectation are expected to be irrelevant:
at(N, expectation(X, irrelevant, G)) :-
  at(N, option(X)),
  at(N, player_goal(G)),
  0 = {
    at(N, expectation(X, threatens, G));
    at(N, expectation(X, enables, G));
    at(N, expectation(X, fails, G));
    at(N, expectation(X, achieves, G))
  }.

% Threatens and enables based on possible outcomes:

at(N, expectation(X, threatens, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_fails(Goal, State).

at(N, expectation(X, threatens, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_achieves(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_achieves(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_fails(Goal, State).

% Fails and achieves based on 'likely' outcomes:

at(N, expectation(X, fails, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_fails(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_achieves(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_achieves(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_fails(Goal, State),
  at(N, likely_outcome(X, Outcome)).


% Outcomes:
%----------

outcome_perception(irrelevant).
outcome_perception(hinders).
outcome_perception(advances).
outcome_perception(fails).
outcome_perception(achieves).

% TODO: hinders and advances!

at(N, outcome_perception(X, fails, Goal)) :-
  at(N, consequence(X, State)),
  state_fails(Goal, State).

at(N, outcome_perception(X, fails, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_achieves(Goal, State).

at(N, outcome_perception(X, achieves, Goal)) :-
  at(N, consequence(X, State)),
  state_achieves(Goal, State).

at(N, outcome_perception(X, achieves, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_fails(Goal, State).
