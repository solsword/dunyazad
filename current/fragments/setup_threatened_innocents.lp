error(m("Wrong initial setup.")) :-
  node_status_reached(root, built),
  not setup(root, threatened_innocents).
