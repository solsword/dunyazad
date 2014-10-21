% trade
action(trade).

argument(trade, buyer, actor).
argument(trade, seller, actor).
argument(trade, price, item).
argument(trade, goods, item).
initiator(trade, buyer).

outcome(trade, deal).
outcome(trade, no_deal).

:-
  action(X, trade),
  arg(X, asking, Asking),
  unintelligent(Asking).

:-
  action(X, trade),
  arg(X, listening, Listening),
  unintelligent(Listening).

:-
  action(X, bargain),
  arg(X, buyer, Buyer),
  arg(X, price, Price),
  0 = {
    has_item(Buyer, Price)
  }.

:-
  action(X, bargain),
  arg(X, seller, Seller),
  arg(X, goods, Goods),
  0 = {
    has_item(Seller, Goods)
  }.

consequence(X, obtains(Buyer, Goods)) :-
  action(X, trade),
  outcome(X, deal),
  arg(X, buyer, Buyer),
  arg(X, goods, Goods).

consequence(X, obtains(Seller, Price)) :-
  action(X, trade),
  outcome(X, deal),
  arg(X, seller, Seller),
  arg(X, price, Price).

consequence(X, relinquishes(Buyer, Price)) :-
  action(X, trade),
  outcome(X, deal),
  arg(X, buyer, Buyer),
  arg(X, price, Price).

consequence(X, relinquishes(Seller, Goods)) :-
  action(X, trade),
  outcome(X, deal),
  arg(X, seller, Seller),
  arg(X, goods, Goods).
