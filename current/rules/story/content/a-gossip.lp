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
  elocution, promotes, no_tool,
  gossip, interested,
  o(utility, useful)
).
skill_link(
  storytelling, promotes, no_tool,
  gossip, interested,
  o(utility, useful)
).

% Either way you can't gossip with the same actor again.
at(
  N,
  consequence_of(
    X,
    o(utility, OVal),
    _not,
    state(knows_gossip, Knowledgeable)
  )
) :-
  at(N, action(X, gossip)),
  outcome_val(gossip, utility, OVal),
  at(N, arg(X, knowledgeable, Knowledgeable)),
  st(N, state(knows_gossip, Knowledgeable)),
  story_op(N, build_options).

% TODO: More effects!
