% monster attack

possible_setup(monster_attack).

setup_argument_create(monster_attack, monster, monster).

s_st(
  monster_attack,
  relation(c(threatening), v(monster), c(inst(actor, you)))
).
