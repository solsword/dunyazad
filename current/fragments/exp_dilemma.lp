error(m("Root choice has wrong structure.")) :-
  node_status_reached(root, built),
  not at(root, choice_structure(negative_alternatives)).

error(m("Root choice doesn't have enough options.")) :-
  node_status_reached(root, built),
  optcount(root, N),
  N < 3.
