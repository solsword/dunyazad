% avoid threats to

state_hinders(
  avoid_threats_to(inst(actor, Key)),
  relation(threatening, inst(ThreatType, ThreatKey), inst(actor, Key))
) :-
  st(N, inst(actor, Key)),
  st(N, inst(ThreatType, ThreatKey)).
