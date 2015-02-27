% talk_down
action(talk_down).

argument(talk_down, asking, actor).
argument(talk_down, listening, actor).
argument(talk_down, victim, actor).
initiator(talk_down, asking).

outcome_val(talk_down, attitude, convinced).
outcome_val(talk_down, attitude, unconvinced).

skill_link(talk_down, elocution).

error(m("Unintelligent asker.", N, X)) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, asking, Asking)),
  st(N, state(unintelligent, Asking)),
  story_op(N, build_options).

error(m("Unintelligent listener.", N, X)) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, listening, Listening)),
  st(N, state(unintelligent, Listening)),
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
    _not,
    relation(threatening, Listening, Victim)
  )
) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, victim, Victim)),
  story_op(N, build_options).
