% bored
potential(opportunity, state, bored).

% Surface text:
% -------------

potential_text(
  N,
  @fmt(
    "N#{}/they V#be/prs/{} bored",
    Bored, Bored
  )
) :-
  at(
    N,
    potential(
      opportunity,
      state(bored, inst(Type, Bored))
    )
  ),
  story_op(N, add_surface).
