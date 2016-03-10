% vignettes.lp
% Rules about vignette flow.

% Setups define the start of a new vignette:
vignette(N, N) :-
  setup(N, Setup),
  story_op(N, initialize_node).
vignette(N, PrV) :-
  0 = { setup(N, Setup) : possible_setup(Setup) },
  successor(Prev, Opt, N),
  vignette(Prev, PrV),
  story_op(N, initialize_node).

% An option which resolves the all remaining potentials at a given
% node resolves that node's vignette, since a new vignette will be
% needed to start to reintroduce potential. Travel onwards is an
% exception, as it happens between vignettes.
resolves_vignette(N, option(O)) :-
  story_node(N),
  at(N, option(O)),
  0 = {
    unresolved_potential(N, option(O), Pt)
      : unresolved_potential(N, option(O), Pt),
        not at(N, hidden(Pt)),
        not at(N, category(Pt, persistent))
  },
  not at(N, action(option(O), travel_onwards)),
  story_op(N, build_options).

% The first node of a new vignette should always be a choice node:
error(m("Vignette starts with an event!")) :-
  vignette(N, N),
  not node_type(N, choice).
