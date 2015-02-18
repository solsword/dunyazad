% goods for sale

possible_setup(on_sale).

setup_argument_create(on_sale, merchant, inst(actor, merchant)).
% TODO: better multi-args
setup_argument_create_opt(on_sale, goods, inst(item, treasure)).
setup_argument_create_opt(on_sale, goods, inst(item, charm)).
setup_argument_create_opt(on_sale, goods, inst(item, tool)).
setup_argument_create_opt(on_sale, goods, inst(item, book)).
setup_argument_create_opt(on_sale, goods, inst(item, instrument)).
setup_argument_create_opt(on_sale, goods, inst(item, weapon)).

error(m("Merchant selling nothing.", N)) :-
  setup(N, on_sale),
  1 > { i(Item) : at(N, setup_arg(goods, Item)) }.

error(m("Merchant selling too much.", N)) :-
  setup(N, on_sale),
  3 < { i(Item) : at(N, setup_arg(goods, Item)) }.

s_st(on_sale, relation(c(has_item), v(merchant), v(goods))).

s_st(on_sale, relation(c(selling), v(merchant), v(goods))).

intro_text(
  N,
  on_sale,
  @fmt(
    "N#{}/they V#show/prs/{} up and V#offer/prs/{} V#trade/inf/{} N#{}/them.",
    LMcht, LMcht, LMcht, LMcht, LGds
  )
) :-
  at(N, setup_arg(merchant, inst(actor, LMcht))),
  at(N, setup_arg(goods, inst(item, LGds))),
  setup(N, on_sale),
  story_op(N, initialize_node).
