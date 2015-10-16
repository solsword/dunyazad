% gossip
action(gossip).

% arguments

argument(gossip, interested, actor).
argument(gossip, knowledgeable, actor).
initiator(gossip, interested).
default_intent(gossip, o(utility, useful)).

% outcomes

outcome_val(gossip, utility, useful).
outcome_val(gossip, utility, useless).

% skills

skill_link(
  elocution, required, no_tool,
  gossip, interested,
  o(utility, useful)
).

error(m("Unintelligent gossip.", N, X)) :-
  at(N, action(X, gossip)),
  at(N, arg(X, interested, Unintelligent)),
  st(N, property(has_skill, Unintelligent, unintelligent)),
  story_op(N, build_options).

error(m("Unintelligent gossip.", N, X)) :-
  at(N, action(X, gossip)),
  at(N, arg(X, knowledgeable, Unintelligent)),
  st(N, property(has_skill, Unintelligent, unintelligent)),
  story_op(N, build_options).

% Either way you can't gossip with the same actor again.
at(
  N,
  consequence_of(
    X,
    o(utility, useful),
    resolves,
    potential(opportunity, state(knows_gossip, Knowledgeable))
  )
) :-
  at(N, action(X, gossip)),
  outcome_val(gossip, utility, OVal),
  at(N, arg(X, knowledgeable, Knowledgeable)),
  st(N, state(knows_gossip, Knowledgeable)),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(utility, useless),
    nullifies,
    potential(opportunity, state(knows_gossip, Knowledgeable))
  )
) :-
  at(N, action(X, gossip)),
  outcome_val(gossip, utility, OVal),
  at(N, arg(X, knowledgeable, Knowledgeable)),
  st(N, state(knows_gossip, Knowledgeable)),
  story_op(N, build_options).


% TODO: More effects!
