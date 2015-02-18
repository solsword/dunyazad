% innocents being threatened

possible_setup(threatened_innocents).

setup_argument_create(threatened_innocents, attacker, inst(actor, bandits)).
setup_argument_create(threatened_innocents, victim, inst(actor, merchants)).
setup_argument_create(threatened_innocents, goods, inst(item, treasure)).

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
  at(N, setup_arg(attacker, inst(actor, LAttk))),
  at(N, setup_arg(victim, inst(actor, LVict))),
  setup(N, threatened_innocents),
  story_op(N, initialize_node).
