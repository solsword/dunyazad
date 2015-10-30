% nodelimit(2)

% Outcome feel for all options should be "expected success:"

error(m("Wrong outcome feel.")) :-
  node_status_reached(root, built),
  at(root, option(X)),
  not at(root, outcome_feel(option(X), expected_success)).

error(m("Wrong number of options.")) :-
  node_status_reached(root, built),
  3 != {
    at(root, option(X))
  }.

error(m("Wrong stakes.")) :-
  node_status_reached(root, built),
  not at(root, perceived_stakes(high)).

error(m("Wrong action 1.")) :-
  node_status_reached(root, built),
  not at(root, action(option(1), attack)).

error(m("Wrong action 2.")) :-
  node_status_reached(root, built),
  not at(root, action(option(2), polymorph)).

error(m("Wrong action 3.")) :-
  node_status_reached(root, built),
  not at(root, action(option(3), pacify)).
