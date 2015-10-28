% explain_innocence
action(explain_innocence).

% arguments

argument(explain_innocence, explainer, actor).
argument(explain_innocence, accuser, actor).
argument(explain_innocence, victim, actor).
initiator(explain_innocence, explainer).
default_intent(explain_innocence, o(success, exonerated)).

% outcomes

outcome_val(explain_innocence, success, exonerated).
outcome_val(explain_innocence, success, ignored).

% skills:

skill_link(
  storytelling, promotes, no_tool,
  explain_innocence,
  explainer,
  o(success, exonerated)
).

% explaining innocence is unlikely to work if the accused is guilty:
% TODO: More specific accusations and crimes!

at(N, unlikely_outcome(X, o(success, exonerated))) :-
  at(N, action(X, explain_innocence)),
  at(N, arg(X, accuser, Justified)),
  at(N, arg(X, victim, Guilty)),
  st(N, relation(has_item, Guilty, Stolen)),
  st(N, relation(stolen_from, Anyone, Stolen)).

at(N, relevant_factor(X, Guilty, has_stolen_item, Stolen)) :-
  at(N, action(X, explain_innocence)),
  at(N, arg(X, accuser, Justified)),
  at(N, arg(X, victim, Guilty)),
  st(N, relation(has_item, Guilty, Stolen)),
  st(N, relation(stolen_from, Anyone, Stolen)).

% constraints:

error(m("Unintelligent blame.", N, X)) :-
  at(N, action(X, explain_innocence)),
  at(N, arg(X, Any, ShouldBeSmart)),
  st(N, property(has_skill, ShouldBeSmart, unintelligent)),
  story_op(N, build_options).

reflexive(explain_innocence).

error(m("Accuser is also victim.", N, X)) :-
  at(N, action(X, explain_innocence)),
  at(N, arg(X, accuser, SamePerson)),
  at(N, arg(X, victim, SamePerson)).

error(m("Accuser is also explainer.", N, X)) :-
  at(N, action(X, explain_innocence)),
  at(N, arg(X, explainer, SamePerson)),
  at(N, arg(X, accuser, SamePerson)).

% explaining innocence is one way to get rid of an accusation:

at(
  N,
  consequence_of(
    X,
    o(success, exonerated),
    resolves,
    potential(problem, relation(accusing, Accuser, Victim))
  )
) :-
  at(N, action(X, explain_innocence)),
  at(N, arg(X, accuser, Accuser)),
  at(N, arg(X, victim, Victim)),
  at(N, potential(problem, relation(accusing, Accuser, Victim))),
  story_op(N, build_options).
