% nodelimit(2)

error(m("Too few options.")) :-
  node_status_reached(root, built),
  3 > {
    at(root, option(X))
  }.

error(m("Wrong choice structure.")) :-
  node_status_reached(root, built),
%  not at(root, choice_structure(mysterious)).
  not at(root, choice_structure(obvious)).
%  not at(root, choice_structure(relaxed)).
%  not at(root, choice_structure(multilemma)).
%  not at(root, choice_structure(pressured)).
%  not at(root, choice_structure(dangerous)).
%  not at(root, choice_structure(bleak)).
%  not at(root, choice_structure(doomed)).
