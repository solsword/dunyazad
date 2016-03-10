% steal
action(steal).

chaotic(steal).

% arguments

argument(steal, thief, actor).
argument(steal, victim, actor).
argument(steal, target, item).
initiator(steal, thief).
default_intent(steal, o(get_item, get)).
default_intent(steal, o(get_caught, safe)).

% outcomes

outcome_val(steal, get_item, get).
outcome_val(steal, get_item, dont_get).

outcome_val(steal, get_caught, caught).
outcome_val(steal, get_caught, safe).

% skills:

skill_link(
  thievery, contest, no_tool,
  steal,
  between(thief, victim),
  either(o(get_item, get), o(get_item, dont_get))
).

skill_link(
  thievery, contest, no_tool,
  steal,
  between(thief, victim),
  either(o(get_caught, safe), o(get_caught, caught))
).

error(m("Stole from wrong victim.", N, X)) :-
  at(N, action(X, steal)),
  at(N, arg(X, victim, Victim)),
  at(N, arg(X, target, Item)),
  not st(N, relation(has_item, Victim, Item)).

% consequences

at(
  N,
  consequence_of(
    X,
    o(get_item, get),
    relation(has_item, Thief, Item)
  )
) :-
  at(N, action(X, steal)),
  at(N, arg(X, thief, Thief)),
  at(N, arg(X, target, Item)),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(get_item, get),
    relation(stolen_from, Victim, Item)
  )
) :-
  at(N, action(X, steal)),
  at(N, arg(X, victim, Victim)),
  at(N, arg(X, target, Item)),
  0 = {
    st(N, inst(actor, Anyone)) :
      st(N, inst(actor, Anyone)),
      st(N, relation(stolen_from, inst(actor, Anyone), Item))
  },
  story_op(N, build_options).

% if you get caught stealing something that doesn't belong to you
% you'll be either threatened or accused (TODO: accusations!).

at(
  N,
  consequence_of(
    X,
    o(get_caught, caught),
    relation(threatening, Victim, Thief)
  )
) :-
  at(N, action(X, steal)),
  at(N, arg(X, thief, Thief)),
  at(N, arg(X, victim, Victim)),
  at(N, arg(X, target, Item)),
  not st(N, relation(stolen_from, Thief, Item)),
  story_op(N, build_options).
