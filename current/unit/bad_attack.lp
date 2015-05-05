% nodelimit(2)

% Setup:

error(m("Wrong setup")) :-
  node_status_reached(root, initialized),
  not setup(root, monster_attack).

% Setting:

error(m("Wrong setting")) :-
  node_status_reached(root, initialized),
  not setting(root, wilderness).

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
  not at(root, outcome(option(1), o(success, defeat))).

error(m("Wrong outcome 2")) :-
  node_status_reached(root, built),
  not at(root, outcome(option(2), o(success, failure))).

error(m("Wrong outcome 3")) :-
  node_status_reached(root, built),
  not at(root, outcome(option(3), o(mood, enraged))).

% Choice structure:

error(m("Wrong choice structure")) :-
  node_status_reached(root, built),
  not at(root, choice_structure(negative_alternatives)).
