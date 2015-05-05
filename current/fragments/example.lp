error(m("Party not simplified.")) :-
  party_size(PS),
  PS != 1.

#minimize {
  N@0, setup(root, Setup) :
    setup(root, Setup),
    setup_priority(Setup, N)
}.
