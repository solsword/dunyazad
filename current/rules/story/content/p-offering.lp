% offering
potential(opportunity, relation, selling).

% Surface text:
% -------------

potential_text(
  N,
  @fmt(
    "N#{}/they V#be/prs/{} V#selling/prp/{} N#{}/them",
    Merchant, Merchant, Merchant, Goods
  )
) :-
  at(
    N,
    potential(
      problem,
      relation(threatening, inst(T1, Merchant), inst(T2, Goods))
    )
  ),
  story_op(N, add_surface).
