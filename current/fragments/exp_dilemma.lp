error(m("Root choice has wrong structure.")) :-
  node_status_reached(root, built),
  not at(root, option_structure(negative_alternatives)).

error(m("Root choice doesn't have three options.")) :-
  node_status_reached(root, built),
  optcount(root, N),
  N != 3.
