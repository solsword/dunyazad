% pay_off
action(pay_off).

argument(pay_off, asking, actor).
argument(pay_off, listening, actor).
argument(pay_off, victim, actor).
argument(pay_off, price, item).
initiator(pay_off, asking).

outcome_val(pay_off, deal, deal).
outcome_val(pay_off, deal, no_deal).

% TODO: Skills?

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

% the threat is withdrawn:
at(
  N,
  consequence_of(
    X,
    o(deal, deal),
    _not,
    relation(threatening, Listening, Victim)
  )
) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, victim, Victim)),
  story_op(N, build_options).

% but the price must be paid:
at(N, consequence_of(X, o(deal, deal), relation(has_item, Listening, Item))) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, listening, Listening)),
  at(N, arg(X, price, Item)),
  story_op(N, build_options).
