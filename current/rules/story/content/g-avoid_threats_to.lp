% avoid threats to

state_hinders(
  avoid_threats_to(inst(Type, Key)),
  relation(threatening, inst(ThreatType, ThreatKey), inst(Type, Key))
) :-
  st(N, inst(Type, Key)),
  st(N, inst(ThreatType, ThreatKey)).
