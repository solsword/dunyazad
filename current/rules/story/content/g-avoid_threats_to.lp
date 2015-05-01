% avoid threats to

at(N, stakes(avoid_threats_to(Actor), high)) :-
  at(N, expectation(X, AnyExp, avoid_threats_to(Actor))),
  AnyExp != irrelevant.

state_hinders(
  avoid_threats_to(inst(actor, Key)),
  relation(threatening, inst(ThreatType, ThreatKey), inst(actor, Key))
) :-
  st(N, inst(actor, Key)),
  st(N, inst(ThreatType, ThreatKey)).
