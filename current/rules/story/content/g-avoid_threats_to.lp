% avoid threats to

at(N, goal_stakes(avoid_threats_to(inst(actor, ID)), high)) :-
  st(N, inst(actor, ID)).

state_hinders(
  avoid_threats_to(inst(actor, Key)),
  relation(threatening, inst(actor, ThreatKey), inst(actor, Key))
) :-
  st(N, inst(actor, Key)),
  st(N, inst(actor, ThreatKey)).

% If there's a threat, it being likely to persist seems dangerous:
at(N, expectation(X, hinders, avoid_threats_to(inst(actor, Victim)))) :-
  at(N, action(X, Action)),
  st(N, relation(threatening, Threat, inst(actor, Victim))),
  0 = { % There's no likely or neutral outcome which will resolve the threat:
    at(N, likely_outcome(X, Likely)) :
      at(N, likely_outcome(X, Likely)),
      at(
        N,
        consequence_of(
          X,
          Likely,
          _not,
          relation(threatening, Threat, inst(actor, Victim))
        )
      );
    at(N, neutral_outcome(X, Neutral)) :
      at(N, neutral_outcome(X, Neutral)),
      at(
        N,
        consequence_of(
          X,
          Neutral,
          _not,
          relation(threatening, Threat, inst(actor, Victim))
        )
      )
  }.

% If there's a threat, just letting it persist is enough to hinder the goal of
% avoiding it:
at(
  N,
  outcome_perception(
    option(X),
    bad_for,
    avoid_threats_to(inst(actor, Key))
  )
) :-
  at(N, option(X)),
  st(N, relation(threatening, Threat, inst(actor, Key))),
  0 = {
    at(N, outcome(option(X), Happened)) :
      at(N, outcome(option(X), Happened)),
      at(
        N,
        consequence_of(
          option(X),
          Happened,
          _not,
          relation(threatening, Threat, inst(actor, Key))
        )
      )
  }.
