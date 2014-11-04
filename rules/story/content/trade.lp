% trade
action(trade).

argument(trade, buyer, actor).
argument(trade, seller, actor).
argument(trade, price, item).
argument(trade, goods, item).
initiator(trade, buyer).

outcome(trade, deal).
outcome(trade, no_deal).

error(m("Unintelligent buyer.", N, X, Buyer)) :-
  at(N, action(X, trade)),
  at(N, arg(X, buyer, Buyer)),
  st(N, state(unintelligent, Buyer)),
  active_node(N).

error(m("Unintelligent seller.", N, X, Seller)) :-
  at(N, action(X, trade)),
  at(N, arg(X, seller, Seller)),
  st(N, state(unintelligent, Seller)),
  active_node(N).

error(m("Buyer doesn't have price.", N, X, Buyer, Price)) :-
  at(N, action(X, bargain)),
  at(N, arg(X, buyer, Buyer)),
  at(N, arg(X, price, Price)),
  0 = {
    st(N, relation(has_item, Buyer, Price))
  },
  active_node(N).

error(m("Seller doesn't have goods.", N, X, Seller, Goods)) :-
  at(N, action(X, bargain)),
  at(N, arg(X, seller, Seller)),
  at(N, arg(X, goods, Goods)),
  0 = {
    st(N, relation(has_item, Seller, Goods))
  },
  active_node(N).

at(N, consequence(X, relation(has_item, Buyer, Goods))) :-
  at(N, action(X, trade)),
  at(N, outcome(X, deal)),
  at(N, arg(X, buyer, Buyer)),
  at(N, arg(X, goods, Goods)),
  active_node(N).

at(N, consequence(X, relation(has_item, Seller, Price))) :-
  at(N, action(X, trade)),
  at(N, outcome(X, deal)),
  at(N, arg(X, seller, Seller)),
  at(N, arg(X, price, Price)),
  active_node(N).
