% deny_blame
action(deny_blame).

% arguments

argument(deny_blame, denier, actor).
argument(deny_blame, accuser, actor).
argument(deny_blame, victim, actor).
initiator(deny_blame, denier).
default_intent(deny_blame, o(success, exonerated)).

% outcomes

outcome_val(deny_blame, success, exonerated).
outcome_val(deny_blame, success, ignored).

% skills:

skill_link(
  elocution, contest, no_tool,
  deny_blame,
  between(denier, accuser),
  either(o(success, exonerated), o(success, ignored))
).

% constraints:

error(m("Unintelligent blame.", N, X)) :-
  at(N, action(X, deny_blame)),
  at(N, arg(X, Any, ShouldBeSmart)),
  st(N, property(has_skill, ShouldBeSmart, unintelligent)),
  story_op(N, build_options).

reflexive(deny_blame).

error(m("Accuser is also victim.", N, X)) :-
  at(N, action(X, deny_blame)),
  at(N, arg(X, accuser, SamePerson)),
  at(N, arg(X, victim, SamePerson)).

error(m("Accuser is also denier.", N, X)) :-
  at(N, action(X, deny_blame)),
  at(N, arg(X, denier, SamePerson)),
  at(N, arg(X, accuser, SamePerson)).

% denying blame is one way to get rid of an accusation:

at(
  N,
  consequence_of(
    X,
    o(success, exonerated),
    resolves,
    potential(problem, relation(accusing, Accuser, Victim))
  )
) :-
  at(N, action(X, deny_blame)),
  at(N, arg(X, accuser, Accuser)),
  at(N, arg(X, victim, Victim)),
  at(N, potential(problem, relation(accusing, Accuser, Victim))),
  story_op(N, build_options).
