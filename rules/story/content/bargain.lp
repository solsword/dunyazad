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
