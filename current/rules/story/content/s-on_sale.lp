% goods for sale

possible_setup(on_sale).

setup_argument_create(on_sale, merchant, businessperson).
% TODO: better multi-args
setup_argument_create_n(on_sale, goods, item, 2, 3).

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
  at(N, setup_arg(merchant, inst(LMcht))),
  at(N, setup_arg(goods, inst(LGds))),
  setup(N, on_sale),
  story_op(N, initialize_node).
