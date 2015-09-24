% avoid threats to

at(N, goal_stakes(avoid_threats_to(inst(actor, ID)), high)) :-
  st(N, inst(actor, ID)).

state_hinders(
  avoid_threats_to(inst(actor, Key)),
  relation(threatening, inst(actor, ThreatKey), inst(actor, Key))
) :-
  st(N, inst(actor, Key)),
  st(N, inst(actor, ThreatKey)).

at(N, expectation(X, fails, avoid_threats_to(Actor))) :-
  at(N, initiator(X, Actor)),
  at(N, action(X, Action)),
  at(N, unlikely_outcome(X, Unlikely)),
  at(N, likely_outcome(X, Likely)),
  at(
    N,
    consequence_of(
      X,
      Unlikely,
      _not,
      relation(threatening, SomeThreat, Actor)
    )
  ),
  0 = {
    at(
      N,
      consequence_of(
        X,
        Likely,
        _not,
        relation(threatening, AnyThreat, Actor)
      )
    )
  }.
