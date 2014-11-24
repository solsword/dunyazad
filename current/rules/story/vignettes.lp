% vignettes.lp
% Rules about vignette flow.

% Setups define the start of a new vignette:
vignette(N, N) :- setup(N, Setup).
vignette(N, PrV) :-
  0 = { setup(N, Setup) : setup(N, Setup) },
  successor(Prev, Opt, N),
  vignette(Prev, PrV).

unresolved_potential(N, Opt, potential(X, Pt)) :-
  st(N, potential(X, Pt)),
  0 = { at(N, consequence(Opt, _not, potential(X, Pt))) }.

unresolved_potential(N, Opt, potential(X, Pt)) :-
  at(N, consequence(Opt, potential(X, Pt))).

% An option which resolves the all remaining potentials at a given node
% resolves that node's vignette, since a new vignette will need to start to
% reintroduce potential.
resolves_vignette(N, option(O)) :-
  story_node(N),
  at(N, option(O)),
  0 = { unresolved_potential(N, option(O), Pt) }.
