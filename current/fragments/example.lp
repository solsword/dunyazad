error(m("Party not simplified.")) :-
  party_size(PS),
  PS != 1.

%error(m("Root choice has wrong setup.")) :-
%  node_status_reached(root, initialized),
%  not at(root, setup_approved).
%
%at(root, setup_approved) :- setup(root, market).
%at(root, setup_approved) :- setup(root, tavern).
