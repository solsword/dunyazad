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
  story_op(N, build_options).

error(m("Unintelligent seller.", N, X, Seller)) :-
  at(N, action(X, trade)),
  at(N, arg(X, seller, Seller)),
  st(N, state(unintelligent, Seller)),
  story_op(N, build_options).

error(m("Buyer doesn't have price.", N, X, Buyer, Price)) :-
  at(N, action(X, trade)),
  at(N, arg(X, buyer, Buyer)),
  at(N, arg(X, price, Price)),
  0 = {
    st(N, relation(has_item, Buyer, Price))
  },
  story_op(N, build_options).

error(m("Seller doesn't have goods.", N, X, Seller, Goods)) :-
  at(N, action(X, trade)),
  at(N, arg(X, seller, Seller)),
  at(N, arg(X, goods, Goods)),
  0 = {
    st(N, relation(has_item, Seller, Goods))
  },
  story_op(N, build_options).

at(N, consequence(X, relation(has_item, Buyer, Goods))) :-
  at(N, action(X, trade)),
  at(N, outcome(X, deal)),
  at(N, arg(X, buyer, Buyer)),
  at(N, arg(X, goods, Goods)),
  story_op(N, build_options).

at(N, consequence(X, relation(has_item, Seller, Price))) :-
  at(N, action(X, trade)),
  at(N, outcome(X, deal)),
  at(N, arg(X, seller, Seller)),
  at(N, arg(X, price, Price)),
  story_op(N, build_options).

% trading is how you buy stuff:
at(N, deals_with(X, relation(selling, Merchant, Goods))) :-
  at(N, action(X, trade)),
  at(N, outcome(X, deal));
  at(N, arg(X, seller, Merchant)),
  at(N, arg(X, goods, Goods)),
  st(N, relation(selling, Merchant, Goods)),
  story_op(N, build_options).


% Surface details
% ---------------

option_text(
  N,
  X,
  @fmt(
    "N#{}/they offer N#{}/them to V#trade/inf/{} N#{}/them for N#{}/them.",
    Buyer, Seller, Buyer, Price, Goods
  )
) :-
  at(N, action(X, trade)),
  at(N, arg(X, buyer, inst(T1, Buyer))),
  at(N, arg(X, seller, inst(T3, Seller))),
  at(N, arg(X, price, inst(T2, Price))),
  at(N, arg(X, goods, inst(T4, Goods))),
  story_op(N, add_surface).

action_text(
  N,
  X,
  @fmt(
    "N#{}/they and N#{}/them V#agree/prs/_plural to V#trade/inf/_plural N#{}/them for N#{}/them.",
    Buyer, Seller, Price, Goods
  )
) :-
  at(N, action(X, trade)),
  at(N, arg(X, buyer, inst(T1, Buyer))),
  at(N, arg(X, seller, inst(T3, Seller))),
  at(N, arg(X, price, inst(T2, Price))),
  at(N, arg(X, goods, inst(T4, Goods))),
  at(N, outcome(X, deal)),
  story_op(N, add_surface).

action_text(
  N,
  X,
  @fmt(
    "N#{}/they and N#{}/them V#haggle/prs/_plural over N#{}/them and N#{}/them, but cannot V#agree/prs/_plural to the trade.",
    Buyer, Seller, Price, Goods
  )
) :-
  at(N, action(X, trade)),
  at(N, arg(X, buyer, inst(T1, Buyer))),
  at(N, arg(X, seller, inst(T3, Seller))),
  at(N, arg(X, price, inst(T2, Price))),
  at(N, arg(X, goods, inst(T4, Goods))),
  at(N, outcome(X, no_deal)),
  story_op(N, add_surface).
