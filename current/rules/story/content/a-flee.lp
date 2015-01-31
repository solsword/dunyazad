% flee
action(flee).
argument(flee, fearful, actor).
argument(flee, from, actor).
initiator(flee, fearful).

outcome(flee, escape).
outcome(flee, failure).

at(N, skill_link(flee, acrobatics)) :-
  1 <= {
    setting(N, city);
    setting(N, town)
  },
  story_node(N).
at(N, skill_link(flee, wilderness_lore)) :-
  1 <= {
    setting(N, road);
    setting(N, wilderness)
  },
  story_node(N).

at(N, deals_with(X, relation(threatening, From, Fearful))) :-
  at(N, action(X, flee)),
  at(N, outcome(X, escape)),
  at(N, arg(X, fearful, Fearful)),
  at(N, arg(X, from, From)),
  st(N, relation(threatening, From, Fearful)),
  story_op(N, build_options).

at(N, emphasizes(X, relation(threatening, From, Fearful))) :-
  at(N, action(X, flee)),
  at(N, outcome(X, failure)),
  at(N, arg(X, fearful, Fearful)),
  at(N, arg(X, from, From)),
  st(N, relation(threatening, From, Fearful)),
  story_op(N, build_options).

0 <= {
  at(N, consequence(X, state(injured, Fearful)))
} <= 1 :-
  at(N, action(X, flee)),
  at(N, arg(X, fearful, Fearful)),
  at(N, outcome(X, failure)),
  story_op(N, build_options).
% TODO: something about the attacker's intent
