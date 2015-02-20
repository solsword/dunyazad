% knows_gossip
potential(opportunity, state, knows_gossip).

% Surface text:
% -------------

potential_text(
  N,
  @fmt(
    "N#{}/they V#seem/prs/{} knowledgeable",
    Knowledgeable, Knowledgeable
  )
) :-
  at(
    N,
    potential(
      opportunity,
      state(knows_gossip, inst(Knowledgeable))
    )
  ),
  story_op(N, add_surface).
