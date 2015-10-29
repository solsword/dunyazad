#maximize {
  1@0,at_least_3_options(N) : at_least_3_options(N), node_type(N, choice)
}.

at_least_3_options(N) :- optcount(N, X), X >= 3.
