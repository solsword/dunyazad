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
      relation(selling, inst(T1, Merchant), inst(T2, Goods))
    )
  ),
  story_op(N, add_surface).
