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

at(
  N,
  consequence_of(
    X,
    O,
    nullifies,
    potential(
      opportunity,
      relation(selling, Merchant, Item)
    )
  )
) :-
  at(N, consequence_of(X, O, _not, relation(has_item, Merchant, Item))),
  at(N, potential(opportunity, relation(selling, Merchant, Item))),
  story_op(N, build_options).
