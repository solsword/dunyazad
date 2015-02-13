% talk_down
action(talk_down).

argument(talk_down, asking, actor).
argument(talk_down, listening, actor).
initiator(talk_down, asking).

outcome(talk_down, convinced).
outcome(talk_down, unconvinced).

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

1 = {
  at(N, deals_with(X, relation(threatening, Listening, Someone)))
    : st(N, relation(threatening, Listening, Someone));
  error(m("Talk down didn't deal with existing threat.", N, X))
  % TODO: other problems that can be solved by paying someone off?
} :-
  at(N, action(X, talk_down)),
  at(N, outcome(X, convinced)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, listening, Listening)),
  story_op(N, build_options).

1 = {
  at(N, emphasizes(X, relation(threatening, Listening, Someone)))
    : st(N, relation(threatening, Listening, Someone));
  error(m("Talk down didn't emphasize existing threat.", N, X))
  % TODO: other problems here as well
} :-
  at(N, action(X, talk_down)),
  at(N, outcome(X, unconvinced)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, listening, Listening)),
  story_op(N, build_options).
