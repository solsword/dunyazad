error(m("Root choice has wrong option structure.")) :-
  node_status_reached(root, built),
  not at(root, option_structure(obvious)).

error(m("Root choice good option has wrong outcome feel.", X)) :-
  at(root, option(X)),
  1 <= {
    at(root, option_feel(option(X), hopeful));
    at(root, option_feel(option(X), safe))
  },
  not at(root, outcome_feel(option(X), unfair)).

error(m("Root choice bad option has wrong outcome feel.", X)) :-
  at(root, option(X)),
  0 = {
    at(root, option_feel(option(X), hopeful));
    at(root, option_feel(option(X), safe))
  },
  0 = {
    at(root, outcome_feel(option(X), miracle));
    at(root, outcome_feel(option(X), nice_gamble))
  }.

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
