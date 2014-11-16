% flee
action(flee).

argument(flee, fearful, actor).
argument(flee, from, actor).
initiator(flee, fearful).

outcome(flee, escape).
outcome(flee, failure).

at(N, deals_with(X, threatening(From, Fearful))) :-
  at(N, action(X, flee)),
  at(N, outcome(X, escape)),
  at(N, arg(X, fearful, Fearful)),
  at(N, arg(X, from, From)),
  st(N, problem(threatening(From, Fearful))),
  active_node(N).

0 <= {
  at(N, consequence(X, state(injured, Fearful)))
} <= 1 :-
  at(N, action(X, flee)),
  at(N, arg(X, fearful, Fearful)),
  at(N, outcome(X, failure)),
  active_node(N).
% TODO: something about the attacker's intent
