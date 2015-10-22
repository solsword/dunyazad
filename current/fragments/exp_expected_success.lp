error(m("Root choice has wrong option structure.")) :-
  node_status_reached(root, built),
  not at(root, option_structure(positive_alternatives)).

error(m("Root choice option has wrong outcome feel.", X)) :-
  at(root, option(X)),
  not at(root, outcome_feel(option(X), expected_success)).

error(m("Root choice doesn't have three options.")) :-
  node_status_reached(root, built),
  optcount(root, N),
  N != 3.

error(m("Root choice doesn't have high stakes.")) :-
  node_status_reached(root, built),
  not at(root, perceived_stakes(high)).

error(m("Root option stakes are inconsistent.")) :-
  node_status_reached(root, built),
  at(root, perceived_option_stakes(A, SomeLevel)),
  at(root, perceived_option_stakes(B, SomeOtherLevel)),
  SomeLevel != SomeOtherLevel.
