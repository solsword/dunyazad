% vignettes.lp
% Rules about node setup and vignette flow.

1 = {
  setup(N, Setup) : possible_setup(Setup)
} :-
  needs_activity(N),
  story_op(N, initialize_node).

needs_activity(N) :-
  story_op(N, initialize_node),
  not inherited_potential(N).

inherited_potential(N) :-
  story_op(N, initialize_node),
  successor(Prev, Opt, N),
  st(Prev, potential(X, Pt)),
  0 = { at(Prev, consequence(Opt, _not, potential(X, Pt))) }.

inherited_potential(N) :-
  story_op(N, initialize_node),
  successor(Prev, Opt, N),
  at(Prev, consequence(Opt, potential(X, Pt))).

% Setups define the start of a new vignette:
vignette(N, N) :- setup(N, Setup).
vignette(N, PrV) :-
  0 = { setup(N, Setup) : setup(N, Setup) },
  successor(Prev, Opt, N),
  vignette(Prev, PrV).

% setup implementation:
st(N, State) :-
  story_op(N, initialize_node),
  s_st(Setup, State),
  setup(N, Setup).

spontaneous(st(N, State)) :-
  story_op(N, X),
  s_st(Setup, State),
  setup(N, Setup).

1 = {
  intro_text(N, Txt) :
    s_intro_text(Setup, Txt)
} :-
  story_op(N, initialize_node),
  setup(N, Setup).
