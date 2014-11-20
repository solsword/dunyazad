% pay_off
action(pay_off).

argument(pay_off, asking, actor).
argument(pay_off, listening, actor).
argument(pay_off, price, item).
initiator(pay_off, asking).

outcome(pay_off, deal).
outcome(pay_off, no_deal).

error(m("Bargain without price.", N, X, Asking, Price)) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, price, Price)),
  0 = {
    st(N, relation(has_item, Asking, Price))
  },
  story_op(N, build_options).

error(m("Unintelligent bargainer.", N, X, Asking)) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, asking, Asking)),
  st(N, state(unintelligent, Asking)),
  story_op(N, build_options).

error(m("Unintelligent listener.", N, X, Listening)) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, listening, Listening)),
  st(N, state(unintelligent, Listening)),
  story_op(N, build_options).

1 = {
  at(N, deals_with(X, potential(problem, threatening(Listening, Someone))))
    : st(N, potential(problem, threatening(Listening, Someone)))
  % TODO: other problems that can be solved by paying someone off?
} :-
  at(N, action(X, pay_off)),
  at(N, outcome(X, deal)),
  at(N, arg(X, asking, Asking)),
  at(N, arg(X, listening, Listening)),
  story_op(N, build_options).

1 = {
  at(N, emphasizes(X, potential(problem, threatening(Listening, Someone))))
    : st(N, potential(problem, threatening(Listening, Someone)))
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

option_text(
  N,
  X,
  @fmt(
    "N#{}/they V#offer/prs/{} N#{}/them N#{}/them.",
    Ask, Ask, Lstn, Price
  )
) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, asking, inst(T1, Ask))),
  at(N, arg(X, listening, inst(T2, Lstn))),
  at(N, arg(X, price, inst(T3, Price))).

action_text(
  N,
  X,
  @fmt(
    "N#{}/they V#bargain/prs/{} with N#{}/them and V#reach/prs/{} a deal.",
    Ask, Ask, Lstn, Ask
  )
) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, asking, inst(T1, Ask))),
  at(N, arg(X, listening, inst(T2, Lstn))),
  at(N, outcome(X, deal)).

action_text(
  N,
  X,
  @fmt(
    "N#{}/they V#bargain/prs/{} with N#{}/them, but can't seem to V#reach/inf/{} a deal.",
    Ask, Ask, Lstn, Ask
  )
) :-
  at(N, action(X, pay_off)),
  at(N, arg(X, asking, inst(T1, Ask))),
  at(N, arg(X, listening, inst(T2, Lstn))),
  at(N, outcome(X, no_deal)).
