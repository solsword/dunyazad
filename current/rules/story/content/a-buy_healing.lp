% buy_healing
action(buy_healing).

reflexive(buy_healing).
injured_can_initiate(buy_healing).

% arguments

argument(buy_healing, doctor, actor).
argument(buy_healing, patient, actor).
argument(buy_healing, buyer, actor).
argument(buy_healing, price, item).
initiator(buy_healing, buyer).
default_intent(buy_healing, o(success, healed)).

% outcomes

outcome_val(buy_healing, success, healed).
outcome_val(buy_healing, success, still_injured).
outcome_val(buy_healing, success, killed).

outcome_val(buy_healing, deal, deal).
outcome_val(buy_healing, deal, no_deal).

outcome_excludes(
  buy_healing,
  o(deal, no_deal),
  o(success, healed)
).

outcome_excludes(
  buy_healing,
  o(deal, no_deal),
  o(success, killed)
).

% skills

skill_link(
  healing, required, tool,
  buy_healing, doctor,
  o(success, healed)
).

skill_link(
  healing, avoids, no_tool,
  buy_healing, doctor,
  o(success, killed)
).

skill_link(
  elocution, promotes, no_tool,
  buy_healing, buyer,
  o(deal, deal)
).

% Patients must be injured:
error(m("Bought healing from unwilling doctor.", N, X)) :-
  at(N, action(X, buy_healing)),
  at(N, arg(X, doctor, Doctor)),
  not st(N, property(offering_service, Doctor, treat_injury)),
  story_op(N, build_options).

error(m("Bought healing for uninjured patient.", N, X)) :-
  at(N, action(X, buy_healing)),
  at(N, arg(X, patient, Patient)),
  not st(N, state(injured, Patient)),
  story_op(N, build_options).

% Trade constraints:
error(m("Unintelligent buyer.", N, X, Buyer)) :-
  at(N, action(X, buy_healing)),
  at(N, arg(X, buyer, Buyer)),
  st(N, property(has_skill, Buyer, unintelligent)),
  story_op(N, build_options).

error(m("Unintelligent doctor.", N, X, Doctor)) :-
  at(N, action(X, buy_healing)),
  at(N, arg(X, doctor, Doctor)),
  st(N, property(has_skill, Doctor, unintelligent)),
  story_op(N, build_options).

error(m("Buyer can't trade price.", N, X, Buyer, Price)) :-
  at(N, action(X, buy_healing)),
  at(N, arg(X, buyer, Buyer)),
  at(N, arg(X, price, Price)),
  not at(N, can_trade(Buyer, Price)),
  story_op(N, build_options).

% Buying healing gets rid of injuries:
at(
  N,
  consequence_of(
    X,
    o(success, healed),
    resolves,
    potential(problem, state(injured, Patient))
  )
) :-
  at(N, action(X, buy_healing)),
  at(N, arg(X, doctor, Doctor)),
  at(N, arg(X, patient, Patient)),
  st(N, state(injured, Patient)),
  story_op(N, build_options).

% However, when buying healing there's a risk of death:
at(N, consequence_of(X, o(success, killed), state(dead, Patient))) :-
  at(N, action(X, buy_healing)),
  at(N, arg(X, patient, Patient)),
  story_op(N, build_options).

% And the doctor takes the price:
at(
  N,
  consequence_of(
    X,
    o(deal, deal),
    relation(has_item, Doctor, Price)
  )
) :-
  at(N, action(X, buy_healing)),
  at(N, arg(X, doctor, Doctor)),
  at(N, arg(X, price, Price)),
  story_op(N, build_options).

% Buying heailng resolves offers to sell healing:
at(
  N,
  consequence_of(
    X,
    o(deal, deal),
    resolves,
    potential(
      opportunity,
      property(offering_service, Doctor, treat_injury)
    )
  )
) :-
  at(N, action(X, trade)),
  at(N, arg(X, seller, Doctor)),
  st(N, property(offering_service, Doctor, treat_injury)),
  story_op(N, build_options).
