% nodelimit(10)

% Setup:

error(m("Wrong setup")) :-
  node_status_reached(root, initialized),
  not setup(root, threatened_innocents).
