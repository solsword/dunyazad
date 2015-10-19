error(m("Root choice has a non-hopeful, non-safe option.", X)) :-
  node_status_reached(root, built),
  at(root, option(X)),
  0 = {
    at(root, option_feel(option(X), safe));
    at(root, option_feel(option(X), hopeful))
  }.

error(m("Root choice option has wrong outcome feel.", X)) :-
  at(root, option(X)),
  not at(root, outcome_feel(option(X), unfair)).

error(m("Root choice doesn't have three options.")) :-
  node_status_reached(root, built),
  optcount(root, N),
  N != 3.

error(m("Root choice doesn't have high stakes.")) :-
  node_status_reached(root, built),
  not at(root, perceived_stakes(high)).
