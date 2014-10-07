% actions:
% --------

% attack
action(attack).

argument(attack, aggressor, actor).
argument(attack, target, actor).
initiator(attack, aggressor).

outcome(attack, victory).
outcome(attack, defeat).
outcome(attack, parley).

:-
  outcome(X, parley),
  argument(X, target, Target),
  unintelligent(Target).

:-
  outcome(X, parley),
  argument(X, aggressor, Aggressor),
  unintelligent(Aggressor).

deals_with(X, threatening(Target, Someone)) :-
  action(X, attack),
  1= {
    outcome(X, parley);
    outcome(X, victory)
  },
  arg(X, target, Target),
  problem(threatening(Target, Someone)).

% fighting is dangerous:
0 <= {
  consequence(X, injured(Aggressor));
  consequence(X, injured(Target))
} <= 2 :-
  action(X, attack),
  arg(X, aggressor, Aggressor),
  arg(X, target, Target).

1 = {
  consequence(X, injured(Aggressor));
  consequence(X, killed(Aggressor))
} :-
  action(X, attack),
  arg(X, aggressor, Aggressor),
  outcome(X, defeat).

1 = {
  consequence(X, injured(Target));
  consequence(X, killed(Target))
} :-
  action(X, attack),
  arg(X, target, Target),
  outcome(X, victory).

0 <= {
  consequence(X, obtains(Aggressor, Item))
} <= 1 :-
  action(X, attack),
  arg(X, aggressor, Aggressor),
  arg(X, target, Target),
  trophy(Target, Item),
  1 = {
    outcome(X, victory);
    outcome(X, parley)
  }.

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

% bargain
action(bargain).

argument(bargain, asking, actor).
argument(bargain, listening, actor).
argument(bargain, price, item).
initiator(bargain, asking).

outcome(bargain, deal).
outcome(bargain, no_deal).

:-
  action(X, bargain),
  arg(X, asking, Asking),
  arg(X, price, Price),
  0 = {
    has_item(Asking, Price)
  }.

1 = {
  deals_with(X, threatening(Listening, Someone))
    : problem(threatening(Listening, Someone))
  % TODO: other problems that can be solved by bargaining?
} :-
  action(X, bargain),
  outcome(X, deal),
  arg(X, asking, Asking),
  arg(X, listening, Listening).

consequence(X, obtains(Listening, Item)) :-
  action(X, bargain),
  outcome(X, deal),
  arg(X, listening, Listening),
  arg(X, price, Item).

consequence(X, relinquishes(Asking, Item)) :-
  action(X, bargain),
  outcome(X, deal),
  arg(X, asking, Asking),
  arg(X, price, Item).

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

% TODO: captured?
