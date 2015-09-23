% as intended

at(N, goal_stakes(as_intended(Actor), low)) :-
  at(N, initiator(X, Actor)).

at(N, expectation(X, achieves, as_intended(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  default_intent(Action, Outcome),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, as_intended(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  default_intent(Action, Outcome),
  at(N, unlikely_outcome(X, Outcome)).

at(N, outcome_perception(X, achieves, as_intended(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  default_intent(Action, Outcome),
  at(N, outcome(X, Outcome)).

at(N, outcome_perception(X, fails, as_intended(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  default_intent(Action, Outcome),
  not at(N, outcome(X, Outcome)).
