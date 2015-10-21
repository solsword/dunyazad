% accuse
action(accuse).

% arguments

argument(accuse, accuser, actor).
argument(accuse, target, actor).
initiator(accuse, accuser).
default_intent(accuse, o(success, accused)).

% outcomes

outcome_val(accuse, success, accused).
outcome_val(accuse, success, ignored).

% skills:

skill_link(
  elocution, contest, no_tool,
  accuse,
  between(accuser, target),
  either(o(success, accused), o(success, ignored))
).

% accuseing if successful develops an "accusing" relation

at(
  N,
  consequence_of(X, o(success, accused), relation(accusing, Accuser, Target))
) :-
  at(N, action(X, accuse)),
  at(N, arg(X, accuser, Accuser)),
  at(N, arg(X, target, Target)),
  story_op(N, build_options).
