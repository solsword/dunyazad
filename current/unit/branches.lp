% nodelimit(2)
:-
  node_status_reached(root, built),
  OPT = {
    at(root, option(Opt))
  },
  OPT < 3.
