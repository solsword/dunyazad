% nodelimit(2)

% Setup:

error(m("Wrong setup")) :-
  node_status_reached(root, initialized),
  0 = { setup(root, market); setup(root, tavern) }.

% Actions:

error(m("Wrong option 1")) :-
  node_status_reached(root, built),
  not at(root, action(option(1), shift_blame)).

error(m("Wrong option 2")) :-
  node_status_reached(root, built),
  not at(root, action(option(2), deny_blame)).

error(m("Wrong option 3")) :-
  node_status_reached(root, built),
  not at(root, action(option(3), explain_innocence)).

error(m("Wrong number of options")) :-
  node_status_reached(root, built),
  optcount(root, X),
  X != 3.
