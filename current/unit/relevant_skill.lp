% nodelimit(2)

error(m("No relevant skills")) :-
  node_status_reached(root, built),
  0 = {
    at(root, relevant_skill(Option, Actor, Skill))
  }.
