% nodelimit(2)

% Setup:

error(m("Wrong setup")) :-
  node_status_reached(root, initialized),
  not setup(root, tavern).

% Actions:

error(m("Too few options")) :-
  node_status_reached(root, built),
  4 > { at(root, option(X)) }.
