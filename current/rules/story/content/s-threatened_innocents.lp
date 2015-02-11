% innocents being threatened

possible_setup(threatened_innocents).

setup_argument_create(threatened_innocents, attacker, inst(actor, attacker)).
setup_argument_create(threatened_innocents, victim, inst(actor, victim)).
setup_argument_create(threatened_innocents, goods, inst(item, goods)).

% The attacker:
s_st(threatened_innocents, property(c(name), v(attacker), c("bandits"))).
s_st(threatened_innocents, property(c(number), v(attacker), c(plural))).
s_st(threatened_innocents, property(c(type), v(attacker), c(bandit))).
% default person/gender

% The victim:
s_st(threatened_innocents, property(c(name), v(victim), c("merchants"))).
s_st(threatened_innocents, property(c(number), v(victim), c(plural))).
s_st(threatened_innocents, property(c(type), v(victim), c(merchant))).
% default person/gender

% The victim owns some goods:
s_st(threatened_innocents, property(c(name), v(goods), c("goods"))).
s_st(threatened_innocents, property(c(number), v(goods), c(plural))).
s_st(threatened_innocents, property(c(type), v(goods), c(treasure))).
% default number/person/gender
s_st(threatened_innocents, relation(c(has_item), v(victim), v(goods))).

% The attacker is threatening the victim:
s_st(threatened_innocents, relation(c(threatening), v(attacker), v(victim))).

intro_text(
  N,
  threatened_innocents,
  @fmt(
    "N#the_party/they V#come/prs/the_party across N#{}/them V#attack/prp/{} N#{}/them.",
    LAttk, LAttk, LVict
  )
) :-
  at(N, setup_arg(attacker, inst(actor, LAttk))),
  at(N, setup_arg(victim, inst(actor, LVict))),
  setup(N, threatened_innocents),
  story_op(N, initialize_node).
