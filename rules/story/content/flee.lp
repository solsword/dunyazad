% flee
action(flee).

argument(flee, fearful, actor).
argument(flee, from, actor).
initiator(flee, fearful).

outcome(flee, escape).
outcome(flee, failure).

deals_with(X, threatening(From, Fearful)) :-
  action(X, flee),
  outcome(X, escape),
  arg(X, fearful, Fearful),
  arg(X, from, From),
  problem(threatening(From, Fearful)).

0 <= {
  consequence(X, injured(Fearful))
} <= 1 :-
  action(X, flee),
  arg(X, fearful, Fearful),
  outcome(X, failure).
% TODO: something about the attacker's intent
