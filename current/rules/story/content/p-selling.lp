% selling
potential(opportunity, relation, selling).
initiated_by(opportunity, relation, selling, from).

error(m("Selling more than one of the same item type.")) :-
  story_node(N),
  st(N, relation(selling, Seller, Item1)),
  st(N, relation(selling, Seller, Item2)),
  Item1 != Item2,
  st(N, property(type, Item1, SameType)),
  st(N, property(type, Item2, SameType)).
