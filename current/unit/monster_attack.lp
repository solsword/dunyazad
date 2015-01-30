% nodelimit(10)

% Setup:

error(m("Wrong setup")) :-
  node_status_reached(root, initialized),
  not setup(root, monster_attack).

% Actions:

error(m("Wrong option 1")) :-
  node_status_reached(root, built),
  not at(root, action(option(1), attack)).

error(m("Wrong option 2")) :-
  node_status_reached(root, built),
  not at(root, action(option(2), flee)).

error(m("Wrong option 3")) :-
  node_status_reached(root, built),
  not at(root, action(option(3), pacify)).

% Outcomes:

error(m("Wrong outcome 1")) :-
  node_status_reached(root, built),
  not at(root, outcome(option(1), defeat)).

error(m("Wrong outcome 2")) :-
  node_status_reached(root, built),
  not at(root, outcome(option(2), failure)).
