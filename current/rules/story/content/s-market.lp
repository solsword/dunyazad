% a market

possible_setup(market).

setup_argument_create_n(market, merchant_one, merchant, 1, 1).
setup_argument_create_n(market, merchant_two, merchant, 0, 1).
setup_argument_create_n(market, noble, aristocrat, 0, 1).
setup_argument_create_n(market, peasant, laborer, 0, 1).
setup_argument_create_n(market, doctor, healer, 0, 1).
setup_argument_create_n(market, lowlife, bad_guy, 0, 1).

% Some stuff for the merchant(s) to sell:
setup_argument_n(market, goods_one, item, 0, 3).
setup_argument_n(market, goods_two, item, 0, 3).

error(m("Market population too low.", N)) :-
  setup(N, market),
  3 > {
    at(N, setup_arg(Arg, Anyone)) :
      at(N, setup_arg(Arg, Anyone)),
      setup_argument_create_n(market, Arg, Typ, L, U)
  }.

error(m("Too much boredom", N)) :-
  setup(N, market),
  3 <= { sp_st(N, state(bored, inst(actor, A))) : st(N, inst(actor, A)) },
  story_op(N, initialize_node).

error(m("Too much gossip", N)) :-
  setup(N, market),
  3 <= { sp_st(N, state(knows_gossip, inst(actor, A))) : st(N, inst(actor, A))},
  story_op(N, initialize_node).

error(m("Noble accusing multiple parties.", N)) :-
  setup(N, market),
  at(N, setup_arg(noble, Noble)),
  2 <= {
    sp_st(N, relation(accusing, Noble, inst(actor, O))) : st(N, inst(actor, O))
  },
  story_op(N, initialize_node).

error(m("Lowlife threatening multiple parties.", N)) :-
  setup(N, market),
  at(N, setup_arg(lowlife, Lowlife)),
  2 <= {
    sp_st(N, relation(threatening, Lowlife, inst(actor, O))) :
      st(N, inst(actor, O))
  },
  story_op(N, initialize_node).

error(m("Merchant one doesn't own their goods", N)) :-
  setup(N, market),
  at(N, setup_arg(merchant_one, Merchant)),
  at(N, setup_arg(goods_one, Goods)),
  not st(N, relation(has_item, Merchant, Goods)),
  story_op(N, initialize_node).

error(m("Merchant two doesn't own their goods", N)) :-
  setup(N, market),
  at(N, setup_arg(merchant_two, Merchant)),
  at(N, setup_arg(goods_two, Goods)),
  not st(N, relation(has_item, Merchant, Goods)),
  story_op(N, initialize_node).

% Potentials:
s_st(market, relation(c(selling), v(merchant_one), v(goods_one))).
s_st(market, relation(c(selling), v(merchant_two), v(goods_two))).
s_st(market, property(c(offering_service), v(doctor), c(treat_injury))).

s_o_st(market, state(c(bored), v(merchant_one))).
s_o_st(market, state(c(bored), v(merchant_two))).
s_o_st(market, state(c(bored), v(noble))).
s_o_st(market, state(c(bored), v(peasant))).
s_o_st(market, state(c(bored), v(lowlife))).
s_o_st(market, state(c(knows_gossip), v(merchant_one))).
s_o_st(market, state(c(knows_gossip), v(merchant_two))).
s_o_st(market, state(c(knows_gossip), v(peasant))).
s_o_st(market, state(c(knows_gossip), v(lowlife))).
s_o_st(market, relation(c(accusing), v(noble), c(inst(actor, you)))).
s_o_st(market, relation(c(accusing), v(noble), v(peasant))).
s_o_st(market, relation(c(accusing), v(noble), v(merchant_one))).
s_o_st(market, relation(c(accusing), v(noble), v(merchant_two))).
s_o_st(market, relation(c(threatening), v(lowlife), v(merchant_one))).
s_o_st(market, relation(c(threatening), v(lowlife), v(merchant_two))).
% TODO: find some way to run the text binding for four cases and then enable
% this.
%s_o_st(market, relation(c(threatening), v(lowlife), c(inst(actor, you)))).
