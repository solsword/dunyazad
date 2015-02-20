% innocents being threatened

possible_setup(threatened_innocents).

setup_argument_create(threatened_innocents, attacker, tough).
setup_argument_create(threatened_innocents, victim, businessperson).
setup_argument_create(threatened_innocents, goods, treasure).
setup_argument_create_n(threatened_innocents, goods, item, 0, 1).

% The victim owns the goods:
s_st(threatened_innocents, relation(c(has_item), v(victim), v(goods))).

% The attacker is threatening the victim:
s_st(threatened_innocents, relation(c(threatening), v(attacker), v(victim))).

intro_text(
  N,
  threatened_innocents,
  @fmt(
    "N#{}/they V#come/prs/{} across N#{}/them V#attack/prp/{} N#{}/them.",
    you, you,
    LAttk, LAttk, LVict
  )
) :-
  at(N, setup_arg(attacker, inst(LAttk))),
  at(N, setup_arg(victim, inst(LVict))),
  setup(N, threatened_innocents),
  story_op(N, initialize_node).
