% nodelimit(10)

:-
  node_status_reached(root, built),
  not node_type(root, choice).

:-
  node_status_reached(root_1, built),
  not node_type(root_1, choice).
