% nodelimit(2) testshouldcrash

% Choice Structure:

error(m("Wrong choice structure")) :-
  node_status_reached(root, built),
  not at(root, choice_structure(relaxed)).

% Threat:

error(m("No threat")) :-
  node_status_reached(root, built),
  0 = {
    st(root, relation(threatening, A, B))
  }.
