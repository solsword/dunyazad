% vignettes.lp
% Rules about node setup and vignette flow.

1 = {
  setup(N, Setup) : possible_setup(Setup)
} :-
  needs_activity(N),
  story_op(N, initialize_node).

needs_activity(N) :-
  story_node(N),
  0 = {
    st(N, problem(Pr)) : st(N, problem(P));
    st(N, opportunity(O)) : st(N, opportunity(O))
  }.

% Setups define the start of a new vignette:
vignette(N, N) :- setup(N, Setup).
vignette(N, PrV) :-
  0 = { setup(N, Setup) : setup(N, Setup) },
  successor(Prev, Opt, N),
  vignette(Prev, PrV).
