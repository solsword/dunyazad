% goods for sale

possible_setup(on_sale).

setup_argument_create(on_sale, merchant, inst(actor, merchant)).
setup_argument_symbol(on_sale, gender, person_gender).
setup_argument_create(on_sale, goods, inst(item, goods)).
% TODO: item type for these goods

% There is a merchant
s_st(on_sale, property(c(name), v(merchant), c("merchant"))).
s_st(on_sale, property(c(gender), v(merchant), v(gender))).
s_st(on_sale, property(c(type), v(merchant), c(merchant))).
% default number/person

s_st(on_sale, property(c(name), v(goods), c("goods"))).
s_st(on_sale, property(c(number), v(goods), c(plural))).
s_st(on_sale, property(c(type), v(goods), c(treasure))).
% default person/gender
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
