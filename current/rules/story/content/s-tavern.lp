% a tavern

possible_setup(tavern).

setup_argument_create_n(tavern, merchant, merchant, 0, 1).
setup_argument_create_n(tavern, noble, aristocrat, 0, 1).
setup_argument_create_n(tavern, peasant, laborer, 0, 1).
% TODO: More possibilities here?

setup_argument_create(tavern, innkeeper, innkeeper).

error(m("Tavern population too low.", N)) :-
  setup(N, tavern),
  2 > {
    at(N, setup_arg(merchant, Merchant));
    at(N, setup_arg(noble, Noble));
    at(N, setup_arg(peasant, Peasant))
  }.

% Some stuff for the merchant to sell:
setup_argument_create_n(tavern, goods, item, 0, 2).

error(m("Merchant but no goods.", N)) :-
  setup(N, tavern),
  at(N, setup_arg(merchant, Merchant)),
  0 = {
    at(N, setup_arg(goods, Goods))
  }.

% The merchant owns the goods:
s_st(tavern, relation(c(has_item), v(merchant), v(goods))).

% Potentials:
s_st(tavern, relation(c(selling), v(merchant), v(goods))).
s_st(tavern, state(c(bored), v(noble))).
s_st(tavern, state(c(bored), v(peasant))).
s_st(tavern, state(c(knows_gossip), v(innkeeper))).
