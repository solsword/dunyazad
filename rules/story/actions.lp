% actions:
% --------

% flee
action(flee).

argument(flee, fearful, actor).
argument(flee, from, actor).
initiator(flee, fearful).

outcome(flee, escape).
outcome(flee, failure).

option_text(X, @join("Flee from ", TheFrom, ".")) :-
  action(X, flee),
  arg(X, from, From),
  det(From, TheFrom).

1 = {
  action_text(
    X,
    @capitalize(
      @join(
        ProFearful, " ", ConjFlee, " from ", TheFrom,
        ", narrowly managing to escape."
      )
    )
%  );
%  action_text(
%    X,
%    @capitalize(
%      @join(
%        ProAsking, " ", ConjOffer, " ", TheListening, " ", ThePrice,
%        ", and ", ProListening, " ", ConjAgree, " to ", ProAskingPossessive,
%        " offer."
%      )
%    )
  )
} :-
  action(X, flee),
  outcome(X, escape),
  arg(X, fearful, Fearful),
  arg(X, from, From),
  number(Fearful, FearfulNumber),
  number(From, FromNumber),
  conjugation(flee, present, FearfulNumber, second, ConjFlee),
  pro(Fearful, second, subjective, object, ProFearful),
  det(From, TheFrom),
  pro(From, third, subjective, object, ProFrom).

1 = {
  action_text(
    X,
    @capitalize(
      @join(
        ProFearful, " ", ConjFlee, " blindly from ", TheFrom,
        ", but ", ProFearful, " ", ConjFail, " to escape."
      )
    )
%  );
%  action_text(
%    X,
%    @capitalize(
%      @join(
%        ProAsking, " ", ConjOffer, " ", TheListening, " ", ThePrice,
%        ", and ", ProListening, " ", ConjAgree, " to ", ProAskingPossessive,
%        " offer."
%      )
%    )
  )
} :-
  action(X, flee),
  outcome(X, failure),
  arg(X, fearful, Fearful),
  arg(X, from, From),
  number(Fearful, FearfulNumber),
  number(From, FromNumber),
  conjugation(flee, present, FearfulNumber, second, ConjFlee),
  conjugation(fail, present, FearfulNumber, third, ConjFail),
  pro(Fearful, second, subjective, object, ProFearful),
  det(From, TheFrom),
  pro(From, third, subjective, object, ProFrom).

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

:-
  action(X, bargain),
  arg(X, asking, Asking),
  unintelligent(Asking).

:-
  action(X, bargain),
  arg(X, listening, Listening),
  unintelligent(Listening).

% TODO: Better text here
option_text(X, @join("Bargain with ", TheListening, ".")) :-
  action(X, bargain),
  arg(X, listening, Listening),
  det(Listening, TheListening).

1 = {
  action_text(
    X,
    @capitalize(
      @join(
        ProAsking, " ", ConjOffer, " ", TheListening, " ", ThePrice,
        ", which ", ProListening, " ", ConjFind, " fair."
      )
    )
  );
  action_text(
    X,
    @capitalize(
      @join(
        ProAsking, " ", ConjOffer, " ", TheListening, " ", ThePrice,
        ", and ", ProListening, " ", ConjAgree, " to ", ProAskingPossessive,
        " offer."
      )
    )
  )
} :-
  action(X, bargain),
  outcome(X, deal),
  arg(X, asking, Asking),
  arg(X, listening, Listening),
  arg(X, price, Price),
  number(Asking, AskNumber),
  number(Listening, ListNumber),
  conjugation(offer, present, AskNumber, second, ConjOffer),
  conjugation(find, present, ListNumber, third, ConjFind),
  conjugation(agree, present, ListNumber, third, ConjAgree),
  pro(Asking, second, subjective, object, ProAsking),
  pro(Asking, second, possessive, modifier, ProAskingPossessive),
  det(Listening, TheListening),
  pro(Listening, third, subjective, object, ProListening),
  det(Price, ThePrice).

1 = {
  action_text(
    X,
    @capitalize(
      @join(
        ProAsking, " ", ConjOffer, " ", TheListening, " ", ThePrice,
        ", but ", ProListening, " refuse ", ProAskingPossessive, " offer."
      )
    )
%  );
%  action_text(
%    X,
%    @join(
%      ""
%    )
  )
} :-
  action(X, bargain),
  outcome(X, no_deal),
  arg(X, asking, Asking),
  arg(X, listening, Listening),
  arg(X, price, Price),
  number(Asking, AskNumber),
  number(Listening, ListNumber),
  conjugation(offer, present, AskNumber, second, ConjOffer),
  conjugation(find, present, ListNumber, third, ConjFind),
  pro(Asking, second, subjective, object, ProAsking),
  pro(Asking, second, possessive, modifier, ProAskingPossessive),
  det(Listening, TheListening),
  pro(Listening, third, subjective, object, ProListening),
  det(Price, ThePrice).

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
  action(X, trade),
  arg(X, asking, Asking),
  unintelligent(Asking).

:-
  action(X, trade),
  arg(X, listening, Listening),
  unintelligent(Listening).

option_text(X, @join("Trade with ", TheSeller, ".")) :-
  action(X, trade),
  arg(X, seller, Seller),
  det(Seller, TheSeller).

1 = {
  action_text(
    X,
    @capitalize(
      @join(
        ProBuyer, " ", ConjOffer, " ", ThePrice, " in exchange for ",
        TheGoods, ", and ", TheSeller, " ", ConjAgree, " to ",
        ProBuyerPossessive, " offer."
      )
    )
%  );
%  action_text(
%    X,
%    @capitalize(
%      @join(
%        ProAsking, " ", ConjOffer, " ", TheListening, " ", ThePrice,
%        ", and ", ProListening, " ", ConjAgree, " to ", ProAskingPossessive,
%        " offer."
%      )
%    )
  )
} :-
  action(X, trade),
  outcome(X, deal),
  arg(X, buyer, Buyer),
  arg(X, seller, Seller),
  arg(X, price, Price),
  arg(X, goods, Goods),
  number(Buyer, BuyerNumber),
  number(Seller, SellerNumber),
  conjugation(offer, present, BuyerNumber, second, ConjOffer),
  conjugation(agree, present, SellerNumber, third, ConjAgree),
  pro(Buyer, second, subjective, object, ProBuyer),
  pro(Buyer, second, possessive, modifier, ProBuyerPossessive),
  det(Seller, TheSeller),
  pro(Seller, third, subjective, object, ProSeller),
  det(Price, ThePrice),
  det(Goods, TheGoods).

1 = {
  action_text(
    X,
    @capitalize(
      @join(
        ProBuyer, " ", ConjOffer, " ", ThePrice, " in exchange for ",
        TheGoods, ", but ", TheSeller, " ", ConjRefuse, " ",
        ProBuyerPossessive, " offer."
      )
    )
%  );
%  action_text(
%    X,
%    @capitalize(
%      @join(
%        ProAsking, " ", ConjOffer, " ", TheListening, " ", ThePrice,
%        ", and ", ProListening, " ", ConjAgree, " to ", ProAskingPossessive,
%        " offer."
%      )
%    )
  )
} :-
  action(X, trade),
  outcome(X, no_deal),
  arg(X, buyer, Buyer),
  arg(X, seller, Seller),
  arg(X, price, Price),
  arg(X, goods, Goods),
  number(Buyer, BuyerNumber),
  number(Seller, SellerNumber),
  conjugation(offer, present, BuyerNumber, second, ConjOffer),
  conjugation(refuse, present, SellerNumber, third, ConjRefuse),
  pro(Buyer, second, subjective, object, ProBuyer),
  pro(Buyer, second, possessive, modifier, ProBuyerPossessive),
  det(Seller, TheSeller),
  pro(Seller, third, subjective, object, ProSeller),
  det(Price, ThePrice),
  det(Goods, TheGoods).

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
