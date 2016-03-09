% as intended

at(N, goal_stakes(as_intended(Actor), low)) :-
  at(N, initiator(X, Actor)).

at(N, expectation(X, advances, as_intended(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  default_intent(Action, Outcome),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, hinders, as_intended(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  default_intent(Action, Outcome),
  at(N, unlikely_outcome(X, Outcome)).

at(N, outcome_perception(X, great_for, as_intended(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  default_intent(Action, Outcome),
  at(N, outcome(X, Outcome)).

at(N, outcome_perception(X, awful_for, as_intended(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  default_intent(Action, o(OutVar, OutVal)),
  not at(N, outcome(X, o(OutVar, OutVal))).
