% settings.lp
% Rules about different settings.

possible_setting(town).
possible_setting(city).
possible_setting(road).
possible_setting(wilderness).

1 = {
  setting(N, Setting) : possible_setting(Setting);
  error(m("Node without setting.", N))
} :-
  story_node(N),
  story_op(N, initialize_node).

setting(Next, Setting) :-
  setting(N, Setting),
  vignette(N, V),
  vignette(Next, V),
  successor(N, Opt, Next).
