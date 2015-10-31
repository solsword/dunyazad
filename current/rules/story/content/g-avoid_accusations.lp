% avoid accusations

at(N, goal_stakes(avoid_accusations(inst(actor, ID)), high)) :-
  st(N, inst(actor, ID)).

state_hinders(
  avoid_accusations(inst(actor, Key)),
  relation(accusing, inst(actor, ThreatKey), inst(actor, Key))
) :-
  st(N, inst(actor, Key)),
  st(N, inst(actor, ThreatKey)).

% If there's an accusation, it being likely to persist seems dangerous:
at(N, expectation(X, fails, avoid_accusations(inst(actor, Victim)))) :-
  at(N, action(X, Action)),
  st(N, relation(accusing, Threat, inst(actor, Victim))),
  0 = { % There's no likely or neutral outcome which will resolve the threat:
    at(N, likely_outcome(X, Likely)) :
      at(N, likely_outcome(X, Likely)),
      at(
        N,
        consequence_of(
          X,
          Likely,
          _not,
          relation(accusing, Threat, inst(actor, Victim))
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
          relation(accusing, Threat, inst(actor, Victim))
        )
      )
  }.

% If there's an accusation, just letting it persist is enough to hinder the
% goal of avoiding it:
at(
  N,
  outcome_perception(
    option(X),
    hinders,
    avoid_accusations(inst(actor, Key))
  )
) :-
  at(N, option(X)),
  st(N, relation(accusing, Threat, inst(actor, Key))),
  0 = {
    at(N, outcome(option(X), Happened)) :
      at(N, outcome(option(X), Happened)),
      at(
        N,
        consequence_of(
          option(X),
          Happened,
          _not,
          relation(accusing, Threat, inst(actor, Key))
        )
      )
  }.
