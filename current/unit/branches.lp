% nodelimit(2)
:-
  node_status_reached(root, branched),
  SC = {
    successor(root, Opt, Child)
  },
  SC < 3.
