% nodelimit(2)

% Outcome feel for all options should be "expected success:"

error(m("Wrong outcome feel.")) :-
  node_status_reached(root, built),
  at(root, option(X)),
  not at(root, outcome_feel(option(X), expected_success)).
