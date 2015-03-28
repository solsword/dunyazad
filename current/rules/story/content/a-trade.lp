% trade
action(trade).

% arguments

argument(trade, buyer, actor).
argument(trade, seller, actor).
argument(trade, price, item).
argument(trade, goods, item).
initiator(trade, buyer).
default_intent(trade, o(deal, deal)).

% outcomes

outcome_val(trade, deal, deal).
outcome_val(trade, deal, no_deal).

% skills

skill_link(
  elocution, promotes, no_tool,
  trade, buyer,
  o(deal, deal)
).

error(m("Unintelligent buyer.", N, X, Buyer)) :-
  at(N, action(X, trade)),
  at(N, arg(X, buyer, Buyer)),
  st(N, property(has_skill, Buyer, unintelligent)),
  story_op(N, build_options).

error(m("Unintelligent seller.", N, X, Seller)) :-
  at(N, action(X, trade)),
  at(N, arg(X, seller, Seller)),
  st(N, property(has_skill, Seller, unintelligent)),
  story_op(N, build_options).

can_trade(N, Person, Thing) :-
  st(N, relation(has_item, Person, Thing)).

can_trade(N, Person, Thing) :-
  st(N, state(party_member, Person)),
  st(N, state(party_member, OtherMember)),
  st(N, relation(has_item, OtherMember, Thing)).

error(m("Buyer can't trade price.", N, X, Buyer, Price)) :-
  at(N, action(X, trade)),
  at(N, arg(X, buyer, Buyer)),
  at(N, arg(X, price, Price)),
  not can_trade(N, Buyer, Price),
  story_op(N, build_options).

error(m("Seller can't trade goods.", N, X, Seller, Goods)) :-
  at(N, action(X, trade)),
  at(N, arg(X, seller, Seller)),
  at(N, arg(X, goods, Goods)),
  not can_trade(N, Seller, Goods),
  story_op(N, build_options).

at(N, consequence_of(X, o(deal, deal), relation(has_item, Buyer, Goods))) :-
  at(N, action(X, trade)),
  at(N, arg(X, buyer, Buyer)),
  at(N, arg(X, goods, Goods)),
  story_op(N, build_options).

at(N, consequence_of(X, o(deal, deal), relation(has_item, Seller, Price))) :-
  at(N, action(X, trade)),
  at(N, arg(X, seller, Seller)),
  at(N, arg(X, price, Price)),
  story_op(N, build_options).

% trading is how you buy stuff:
at(
  N,
  consequence_of(X, o(deal, deal), _not, relation(selling, Seller, Goods))
) :-
  at(N, action(X, trade)),
  at(N, arg(X, seller, Seller)),
  at(N, arg(X, goods, Goods)),
  st(N, relation(selling, Seller, Goods)),
  story_op(N, build_options).
