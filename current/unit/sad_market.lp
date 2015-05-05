% nodelimit(2)

% Party size:

error(m("Party too large")) :-
  party_size(N),
  N != 1.

% Setup:

error(m("Wrong setup")) :-
  node_status_reached(root, initialized),
  not setup(root, market).

% Setting:

error(m("Wrong setting")) :-
  node_status_reached(root, initialized),
  not setting(root, city).

% Actions:

error(m("Wrong option 1")) :-
  node_status_reached(root, built),
  not at(root, action(option(1), gossip)).

error(m("Wrong option 2")) :-
  node_status_reached(root, built),
  not at(root, action(option(2), play_song)).

error(m("Wrong option 3")) :-
  node_status_reached(root, built),
  not at(root, action(option(3), trade)).

% Outcomes:

error(m("Wrong outcome 1")) :-
  node_status_reached(root, built),
  not at(root, outcome(option(1), o(utility, useless))).

error(m("Wrong outcome 2")) :-
  node_status_reached(root, built),
  not at(root, outcome(option(2), o(quality, plain))).

error(m("Wrong outcome 3")) :-
  node_status_reached(root, built),
  not at(root, outcome(option(3), o(deal, no_deal))).

% Choice structure:

%error(m("Wrong choice structure")) :-
%  node_status_reached(root, built),
%  not at(root, choice_structure(negative_alternatives)).
