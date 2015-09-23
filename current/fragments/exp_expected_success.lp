error(m("Root choice has wrong structure.")) :-
  node_status_reached(root, built),
  not at(root, option_structure(obvious)).

error(m("Root choice good option has wrong outcome feel.")) :-
  at(root, option(X)),
  1 = {
    at(root, option_feel(option(X), hopeful));
    at(root, option_feel(option(X), safe))
  },
  not at(root, outcome_feel(option(X), expected_success)).

error(m("Root choice bad option has wrong outcome feel.")) :-
  at(root, option(X)),
  not at(root, option_feel(option(X), hopeful)),
  not at(root, option_feel(option(X), safe)),
  at(root, outcome_feel(option(X), expected_success)).

error(m("Root choice doesn't have three options.")) :-
  node_status_reached(root, built),
  optcount(root, N),
  N != 3.
