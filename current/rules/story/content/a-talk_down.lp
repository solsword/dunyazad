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

outcome_val(talk_down, enraged, enraged).
outcome_val(talk_down, enraged, not_enraged).

outcome_excludes(
  talk_down,
  o(attitude, convinced),
  o(enraged, enraged)
).

% skills

skill_link(
  elocution, promotes, no_tool,
  talk_down, asking,
  o(attitude, convinced)
).

skill_link(
  storytelling, promotes, no_tool,
  talk_down, asking,
  o(attitude, convinced)
).

skill_link(
  elocution, promotes, no_tool,
  talk_down, asking,
  o(enraged, not_enraged)
).

skill_link(
  storytelling, promotes, no_tool,
  talk_down, asking,
  o(enraged, not_enraged)
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

error(m("Victim not being threatened or accused.", N, X)) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, victim, Victim)),
  0 = {
    st(N, relation(threatening, Listening, Victim));
    st(N, relation(accusing, Listening, Victim))
  },
  story_op(N, build_options).

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
  st(N, relation(threatening, Listening, Victim)),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(attitude, convinced),
    resolves,
    potential(problem, relation(accusing, Listening, Victim))
  )
) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, victim, Victim)),
  st(N, relation(accusing, Listening, Victim)),
  story_op(N, build_options).

% but the the target might become enraged instead
at(
  N,
  consequence_of(
    X,
    o(enraged, enraged),
    relation(threatening, Listening, Asking)
  )
) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, victim, Victim)),
  st(N, relation(threatening, Listening, Victim)),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(enraged, enraged),
    relation(accusing, Listening, Asking)
  )
) :-
  at(N, action(X, talk_down)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, victim, Victim)),
  st(N, relation(accusing, Listening, Victim)),
  story_op(N, build_options).

