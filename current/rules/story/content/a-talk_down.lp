% talk_down
action(talk_down).

% arguments

argument(talk_down, asking, actor).
argument(talk_down, listening, actor).
argument(talk_down, victim, actor).
initiator(talk_down, asking).
default_intent(talk_down, o(attitude, convinced)).

% outcomes

outcome_val(talk_down, attitude, convinced).
outcome_val(talk_down, attitude, unconvinced).

% skills

skill_link(
  elocution, required, no_tool,
  talk_down, asking,
  o(attitude, convinced)
).

error(m("Unintelligent asker.", N, X)) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, asking, Asking)),
  st(N, property(has_skill, Asking, unintelligent)),
  story_op(N, build_options).

error(m("Unintelligent listener.", N, X)) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, listening, Listening)),
  st(N, property(has_skill, Listening, unintelligent)),
  story_op(N, build_options).

error(m("Victim not being threatened.", N, X)) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, victim, Victim)),
  not st(N, relation(threatening, Listening, Victim)).

% success convinces the threatener to back down
at(
  N,
  consequence_of(
    X,
    o(attitude, convinced),
    resolves,
    potential(problem, relation(threatening, Listening, Victim))
  )
) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, victim, Victim)),
  story_op(N, build_options).
