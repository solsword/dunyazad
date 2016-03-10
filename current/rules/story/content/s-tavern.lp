% a tavern

possible_setup(tavern).

setup_argument_create_n(tavern, merchant, merchant, 0, 1).
setup_argument_create_n(tavern, noble, aristocrat, 0, 1).
setup_argument_create_n(tavern, peasant, laborer, 0, 1).
setup_argument_create(tavern, innkeeper, innkeeper).

% The stuff that the merchant is selling:
setup_argument_n(tavern, goods, item, 0, 2).

error(m("Tavern population too low.", N)) :-
  setup(N, tavern),
  2 > {
    at(N, setup_arg(merchant, Merchant));
    at(N, setup_arg(noble, Noble));
    at(N, setup_arg(peasant, Peasant))
  }.

error(m("Too much boredom", N)) :-
  setup(N, tavern),
  3 <= {
    sp_st(N, state(bored, inst(actor, A))) : st(N, inst(actor, A))
  },
  story_op(N, initialize_node).

error(m("Too much gossip", N)) :-
  setup(N, tavern),
  3 <= {
    sp_st(N, state(knows_gossip, inst(actor, A)))
      : st(N, inst(actor, A))
  },
  story_op(N, initialize_node).

error(m("Noble accusing multiple parties.", N)) :-
  setup(N, tavern),
  at(N, setup_arg(noble, Noble)),
  2 <= {
    sp_st(N, relation(accusing, Noble, inst(actor, O)))
      : st(N, inst(actor, O))
  },
  story_op(N, initialize_node).

error(m("Merchant doesn't own the goods", N)) :-
  setup(N, tavern),
  at(N, setup_arg(merchant, Merchant)),
  at(N, setup_arg(goods, Goods)),
  not st(N, relation(has_item, Merchant, Goods)),
  story_op(N, initialize_node).

% Potentials:
s_o_st(tavern, relation(c(selling), v(merchant), v(goods))).
s_o_st(tavern, state(c(bored), v(merchant))).
s_o_st(tavern, state(c(bored), v(noble))).
s_o_st(tavern, state(c(bored), v(peasant))).
s_o_st(tavern, state(c(knows_gossip), v(noble))).
s_o_st(tavern, state(c(knows_gossip), v(peasant))).
s_o_st(tavern, state(c(knows_gossip), v(innkeeper))).
s_o_st(market, relation(c(accusing), v(noble), v(peasant))).
s_o_st(market, relation(c(accusing), v(noble), v(merchant))).
s_o_st(market, relation(c(accusing), v(noble), v(innkeeper))).
