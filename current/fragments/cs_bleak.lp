error(m("Root choice has wrong structure.")) :-
  node_status_reached(root, built),
  not at(root, choice_structure(bleak)).
