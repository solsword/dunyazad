% nodelimit(2)

error(m("Choice isn't grim.")) :-
  node_status_reached(root, built),
  not at(root, choice_structure(grim)).

error(m("Wrong option 1")) :-
  node_status_reached(root, built),
  not at(root, action(option(1), attack)).

error(m("Wrong option 2")) :-
  node_status_reached(root, built),
  not at(root, action(option(2), pacify)).
