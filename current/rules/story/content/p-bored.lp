% bored
potential(opportunity, state, bored).
initiated_by(opportunity, state, bored, inst).

error(m("Bored while embroiled in something interesting.")) :-
  st(N, state(bored, Actor)),
  1 <= {
    at(N, potential(PType, state(S, Actor)));
    at(N, potential(PType, property(P, Actor, V)));
    at(N, potential(PType, relation(R, Actor, Other)));
    at(N, potential(PType, relation(R, Other, Actor)))
  }
  story_op(N, initialize_node).
