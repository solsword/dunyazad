% threatening
potential(problem, relation, threatening).

% Surface text:
% -------------

potential_text(
  N,
  @fmt(
    "N#{}/they V#threaten/prsc/{} N#{}/them",
    Threatening, Threatening, Threatened
  )
) :-
  at(
    N,
    potential(
      problem,
      relation(threatening, inst(Threatening), inst(Threatened))
    )
  ),
  story_op(N, add_surface).
