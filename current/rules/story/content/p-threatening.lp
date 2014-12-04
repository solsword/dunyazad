% threatening
potential(problem, relation, threatening).

% Surface text:
% -------------

potential_text(
  N,
  @fmt(
    "N#{}/they V#be/prs/{} V#threaten/prp/{} N#{}/them",
    Threatening, Threatening, Threatening, Threatened
  )
) :-
  at(
    N,
    potential(
      problem,
      relation(threatening, inst(T1, Threatening), inst(T2, Threatened))
    )
  ),
  story_op(N, add_surface).
