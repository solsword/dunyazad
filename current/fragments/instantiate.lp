story_op(N, initialize_node) :- target_node(N).
story_op(N, build_options) :- target_node(N).

at(N, text_mode(normal)) :-
  target_node(N),
  not at(N, text_mode(example)).
