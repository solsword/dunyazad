% buy_service
action(buy_service).

% TODO: buy_service makes no guarantee that the service is useful to the buyer.
% It works by creating an event where the person offering the service is the
% initiator of the offered action. There are no constraints placed on any other
% arguments that action might have.

% arguments

argument(buy_service, asking, actor).
argument(buy_service, offering, actor).
argument(buy_service, price, item).
argument(buy_service, service, action).
initiator(buy_service, asking).
default_intent(buy_service, o(deal, deal)).

% outcomes

outcome_val(buy_service, deal, deal).
outcome_val(buy_service, deal, no_deal).

% skills

skill_link(
  elocution, promotes, no_tool,
  buy_service, asking,
  o(deal, deal)
).

% conditions

error(m("Service wasn't being offered.", N, X)) :-
  at(N, action(X, buy_service)),
  at(N, arg(X, offering, Offering)),
  at(N, arg(X, service, Service)),
  not st(N, property(offering_service, Offering, Service)),
  story_op(N, build_options).

error(m("Bought service without price.", N, X)) :-
  at(N, action(X, buy_service)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, price, Price)),
  not at(N, can_trade(Asking, Price)),
  story_op(N, build_options).

error(m("Unintelligent asker.", N, X)) :-
  at(N, action(X, buy_service)),
  at(N, arg(X, asking, Asking)),
  st(N, property(has_skill, Asking, unintelligent)),
  story_op(N, build_options).

error(m("Unintelligent offerer.", N, X)) :-
  at(N, action(X, buy_service)),
  at(N, arg(X, offering, Offering)),
  st(N, property(has_skill, Offering, unintelligent)),
  story_op(N, build_options).

error(m("Injured offerer.", N, X)) :-
  at(N, action(X, buy_service)),
  at(N, arg(X, offering, Offering)),
  st(N, state(injured, Offering)),
  story_op(N, build_options).

% the service will be rendered:
service_deal_honored(N, X) :-
  at(N, action(X, buy_service)),
  at(N, outcome(X, o(deal, deal))),
  at(N, arg(X, offering, Offering)),
  at(N, arg(X, service, Service)),
  successor(N, X, Next),
  node_type(Next, event),
  at(Next, action(Opt, Service)),
  at(Next, initiator(Opt, Offering)),
  story_op(Next, build_options).

% The offering party is motivated to honor their deal:
at(Next, motivated(Y)) :-
  at(N, action(X, buy_service)),
  at(N, outcome(X, o(deal, deal))),
  at(N, arg(X, service, Service)),
  successor(N, X, Next),
  at(Next, action(Y, Service)),
  story_op(Next, build_options).

% TODO: don't paint ourselves into a corner here!!
error(m("Service deal not honored.", N, X)) :-
  at(N, action(X, buy_service)),
  at(N, outcome(X, o(deal, deal))),
  successor(N, X, Next),
  not service_deal_honored(N, X),
  story_op(Next, build_options).

% but the price must be paid:
at(N, consequence_of(X, o(deal, deal), relation(has_item, Offering, Item))) :-
  at(N, action(X, buy_service)),
  at(N, arg(X, offering, Offering)),
  at(N, arg(X, price, Item)),
  story_op(N, build_options).

% the service offer is withdrawn either way:
at(
  N,
  consequence_of(
    X,
    o(deal, deal),
    takes_advantage_of,
    potential(opportunity, property(offering_service, Offering, Service))
  )
) :-
  at(N, action(X, buy_service)),
  at(N, arg(X, offering, Offering)),
  at(N, arg(X, service, Service)),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(deal, deal),
    forfeits,
    potential(opportunity, property(offering_service, Offering, Service))
  )
) :-
  at(N, action(X, buy_service)),
  at(N, arg(X, offering, Offering)),
  at(N, arg(X, service, Service)),
  story_op(N, build_options).
