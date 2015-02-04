% gossip
action(gossip).
argument(gossip, interested, actor).
argument(gossip, knowledgeable, actor).
initiator(gossip, interested).

outcome(gossip, useful).
outcome(gossip, useless).

skill_link(gossip, elocution).
skill_link(gossip, storytelling).

at(N, deals_with(X, state(knows_gossip, Knowledgeable))) :-
  at(N, action(X, gossip)),
  at(N, outcome(X, useful)),
  at(N, arg(X, knowledgeable, Knowledgeable)),
  st(N, state(knows_gossip, Knowledgeable)),
  story_op(N, build_options).

at(N, nullifies(X, state(knows_gossip, Knowledgeable))) :-
  at(N, action(X, gossip)),
  at(N, outcome(X, useless)),
  at(N, arg(X, knowledgeable, Knowledgeable)),
  st(N, state(knows_gossip, Knowledgeable)),
  story_op(N, build_options).

% TODO: More effects?
