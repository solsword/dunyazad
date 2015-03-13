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
  state_hinders(Goal, State).

at(N, expectation(X, threatens, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_fails(Goal, State).

at(N, expectation(X, threatens, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_advances(Goal, State).

at(N, expectation(X, threatens, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_achieves(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_advances(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_achieves(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_hinders(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_fails(Goal, State).

% Fails and achieves based on 'likely' outcomes:

at(N, expectation(X, fails, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_hinders(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_fails(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_advances(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_achieves(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_advances(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_achieves(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_hinders(Goal, State),
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

% hinders and advances

at(N, outcome_perception(X, hinders, Goal)) :-
  at(N, consequence(X, State)),
  state_hinders(Goal, State).

at(N, outcome_perception(X, hinders, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_advances(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, consequence(X, State)),
  state_advances(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_hinders(Goal, State).

at(N, outcome_perception(X, hinders, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_achieves(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_fails(Goal, State).

% fails and achieves

at(N, outcome_perception(X, fails, Goal)) :-
  at(N, consequence(X, State)),
  state_fails(Goal, State).

at(N, outcome_perception(X, achieves, Goal)) :-
  at(N, consequence(X, State)),
  state_achieves(Goal, State).

% different types of option feels:

at(N, option_feel(X, sure_thing)) :-
  at(N, option(O)),
  X = option(O),
  1 <= {
    at(N, player_goal(G1)) : at(N, expectation(X, achieves, G1))
  },
  0 = {
    at(N, player_goal(G2)) : at(N, expectation(X, threatens, G2));
    at(N, player_goal(G3)) : at(N, expectation(X, fails, G3))
  }.

at(N, option_feel(X, safe)) :-
  at(N, option(O)),
  X = option(O),
  1 <= {
    at(N, player_goal(G1)) : at(N, expectation(X, achieves, G1));
    at(N, player_goal(G2)) : at(N, expectation(X, enables, G2))
  },
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(X, threatens, G3));
    at(N, player_goal(G4)) : at(N, expectation(X, fails, G4))
  }.

at(N, option_feel(X, hopeful)) :-
  at(N, option(O)),
  X = option(O),
  1 <= {
    at(N, player_goal(G1)) : at(N, expectation(X, achieves, G1))
  },
  1 <= {
    at(N, player_goal(G2)) : at(N, expectation(X, threatens, G2))
  },
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(X, fails, G3))
  }.

at(N, option_feel(X, risky)) :-
  at(N, option(O)),
  X = option(O),
  1 <= {
    at(N, player_goal(G1)) : at(N, expectation(X, enables, G1))
  },
  1 <= {
    at(N, player_goal(G2)) : at(N, expectation(X, threatens, G2))
  },
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(X, achieves, G3));
    at(N, player_goal(G4)) : at(N, expectation(X, fails, G4))
  }.

at(N, option_feel(X, tradeoff)) :-
  at(N, option(O)),
  X = option(O),
  1 <= {
    at(N, player_goal(G1)) : at(N, expectation(X, achieves, G1))
  },
  1 <= {
    at(N, player_goal(G2)) : at(N, expectation(X, fails, G2))
  }.

at(N, option_feel(X, boring)) :-
  at(N, option(O)),
  X = option(O),
  0 = {
    at(N, player_goal(G1)) : at(N, expectation(X, enables, G1));
    at(N, player_goal(G2)) : at(N, expectation(X, achieves, G2));
    at(N, player_goal(G3)) : at(N, expectation(X, threatens, G3));
    at(N, player_goal(G4)) : at(N, expectation(X, fails, G4))
  }.

at(N, option_feel(X, longshot)) :-
  at(N, option(O)),
  X = option(O),
  1 <= {
    at(N, player_goal(G1)) : at(N, expectation(X, fails, G1))
  },
  1 <= {
    at(N, player_goal(G2)) : at(N, expectation(X, enables, G2))
  },
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(X, achieves, G3))
  }.

at(N, option_feel(X, unpleasant)) :-
  at(N, option(O)),
  X = option(O),
  1 <= {
    at(N, player_goal(G1)) : at(N, expectation(X, threatens, G1));
    at(N, player_goal(G2)) : at(N, expectation(X, fails, G2))
  },
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(X, enables, G3));
    at(N, player_goal(G4)) : at(N, expectation(X, achieves, G4))
  }.

at(N, option_feel(X, doomed)) :-
  at(N, option(O)),
  X = option(O),
  1 <= {
    at(N, player_goal(G1)) : at(N, expectation(X, fails, G1))
  },
  0 = {
    at(N, player_goal(G2)) : at(N, expectation(X, enables, G2));
    at(N, player_goal(G3)) : at(N, expectation(X, achieves, G3))
  }.

% choice feelings:

% TODO: choice feelings
