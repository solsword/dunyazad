% vim: syn=gringo
% setup:
% ------

max_unique(0).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).

story_op(root, initialize_node).
story_op(root, build_options).

error(m("Music isn't a relevant skill.")) :-
  0 = {
    at(root, relevant_skill(Option, Actor, music))
  }.
