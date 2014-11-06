% bargain
action(bargain).

argument(bargain, asking, actor).
argument(bargain, listening, actor).
argument(bargain, price, item).
initiator(bargain, asking).

outcome(bargain, deal).
outcome(bargain, no_deal).

error(m("Bargain without price.", N, X, Asking, Price)) :-
  at(N, action(X, bargain)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, price, Price)),
  0 = {
    st(N, relation(has_item, Asking, Price))
  },
  active_node(N).

error(m("Unintelligent bargainer.", N, X, Asking)) :-
  at(N, action(X, bargain)),
  at(N, arg(X, asking, Asking)),
  st(N, state(unintelligent, Asking)),
  active_node(N).

error(m("Unintelligent listener.", N, X, Listening)) :-
  at(N, action(X, bargain)),
  at(N, arg(X, listening, Listening)),
  st(N, state(unintelligent, Listening)),
  active_node(N).

1 = {
  at(N, deals_with(X, threatening(Listening, Someone)))
    : st(N, problem(threatening(Listening, Someone)))
  % TODO: other problems that can be solved by bargaining?
} :-
  at(N, action(X, bargain)),
  at(N, outcome(X, deal)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, listening, Listening)),
  active_node(N).

at(N, consequence(X, relation(has_item, Listening, Item))) :-
  at(N, action(X, bargain)),
  at(N, outcome(X, deal)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, price, Item)),
  active_node(N).
