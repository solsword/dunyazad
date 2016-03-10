% stolen_from
potential(problem, relation, stolen_from).
pcategory(potential(problem, relation, stolen_from), urgent).
pcategory(potential(problem, relation, stolen_from), immediate).
problematic_for(problem, relation, stolen_from, from).

% Whoever has the item is the thief:
at(
  N,
  initiated_by(
    potential(problem, relation(stolen_from, Victim, Item)),
    Thief
  )
) :-
  at(N, potential(problem, relation(stolen_from, Victim, Item))),
  st(N, relation(has_item, Thief, Item)).

% taking back the item resolves the stolen_from potential:
at(
  N,
  consequence_of(X, O, resolves, relation(stolen_from, Victim, Item))
) :-
  at(N, consequence_of(X, O, relation(has_item, Victim, Item))),
  st(N, relation(stolen_from, Victim, Item)),
  story_op(N, build_options).
