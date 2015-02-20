% offering
potential(opportunity, relation, selling).

% Surface text:
% -------------

potential_text(
  N,
  @fmt(
    "N#{}/they V#sell/prsc/{} N#{}/them",
    Merchant, Merchant, Goods
  )
) :-
  at(
    N,
    potential(
      opportunity,
      relation(selling, inst(Merchant), inst(Goods))
    )
  ),
  story_op(N, add_surface).
