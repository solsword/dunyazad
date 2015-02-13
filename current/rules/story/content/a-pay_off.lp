% pay_off
action(pay_off).

argument(pay_off, asking, actor).
argument(pay_off, listening, actor).
argument(pay_off, price, item).
initiator(pay_off, asking).

outcome(pay_off, deal).
outcome(pay_off, no_deal).

error(m("Pay off without price.", N, X)) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, price, Price)),
  not st(N, relation(has_item, Asking, Price)),
  story_op(N, build_options).

error(m("Price is not a treasure.", N, X)) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, price, Price)),
  not is_instance(N, Price, treasure),
  story_op(N, build_options).

error(m("Unintelligent asker.", N, X)) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, asking, Asking)),
  st(N, state(unintelligent, Asking)),
  story_op(N, build_options).

error(m("Unintelligent listener.", N, X)) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, listening, Listening)),
  st(N, state(unintelligent, Listening)),
  story_op(N, build_options).

1 = {
  at(N, deals_with(X, relation(threatening, Listening, Someone)))
    : st(N, relation(threatening, Listening, Someone));
  error(m("Pay off didn't deal with existing threat.", N, X))
  % TODO: other problems that can be solved by paying someone off?
} :-
  at(N, action(X, pay_off)),
  at(N, outcome(X, deal)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, listening, Listening)),
  story_op(N, build_options).

1 = {
  at(N, emphasizes(X, relation(threatening, Listening, Someone)))
    : st(N, relation(threatening, Listening, Someone));
  error(m("Pay off didn't emphasize existing threat.", N, X))
  % TODO: other problems here as well
} :-
  at(N, action(X, pay_off)),
  at(N, outcome(X, no_deal)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, listening, Listening)),
  story_op(N, build_options).

at(N, consequence(X, relation(has_item, Listening, Item))) :-
  at(N, action(X, pay_off)),
  at(N, outcome(X, deal)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, price, Item)),
  story_op(N, build_options).
