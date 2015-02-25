% goods for sale

possible_setup(on_sale).

setup_argument_create(on_sale, merchant, businessperson).
setup_argument_create_n(on_sale, goods, item, 2, 3).

s_st(on_sale, relation(c(has_item), v(merchant), v(goods))).

s_st(on_sale, relation(c(selling), v(merchant), v(goods))).
