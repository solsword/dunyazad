% treat_injury
action(treat_injury).

reflexive(treat_injury).
injured_can_initiate(treat_injury). % TODO: really this?
chaotic(treat_injury).

% arguments

argument(treat_injury, doctor, actor).
argument(treat_injury, patient, actor).
initiator(treat_injury, doctor).
default_intent(treat_injury, o(success, healed)).

% outcomes

outcome_val(treat_injury, success, healed).
outcome_val(treat_injury, success, still_injured).
outcome_val(treat_injury, success, killed).

% skills

skill_link(
  healing, required, tool,
  treat_injury, doctor,
  o(success, healed)
).

skill_link(
  healing, avoids, no_tool,
  treat_injury, doctor,
  o(success, killed)
).

% Patients must be injured:
error(m("Treated uninjured patient.", N, X)) :-
  at(N, action(X, treat_injury)),
  at(N, arg(X, patient, Patient)),
  not st(N, state(injured, Patient)),
  story_op(N, build_options).

error(m("Unintelligent doctor.", N, X)) :-
  at(N, action(X, treat_injury)),
  at(N, arg(X, doctor, Unintelligent)),
  st(N, property(has_skill, Unintelligent, unintelligent)),
  story_op(N, build_options).

% Treating injuries gets rid of them
at(
  N,
  consequence_of(
    X,
    o(success, healed),
    resolves,
    potential(problem, state(injured, Patient))
  )
) :-
  at(N, action(X, treat_injury)),
  at(N, arg(X, doctor, Doctor)),
  at(N, arg(X, patient, Patient)),
  st(N, state(injured, Patient)),
  story_op(N, build_options).

% However, when treating injuries there's a risk of death:
at(N, consequence_of(X, o(success, killed), state(dead, Patient))) :-
  at(N, action(X, treat_injury)),
  at(N, arg(X, patient, Patient)),
  story_op(N, build_options).
