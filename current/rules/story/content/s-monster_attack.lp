% monster attack

possible_setup(monster_attack).

setup_argument_create(monster_attack, monster, monster).
setup_argument_create_n(monster_attack, trophy, trophy, 0, 1).

s_st(monster_attack, state(c(unintelligent), v(monster))).

s_st(monster_attack, relation(c(threatening), v(monster), c(inst(actor, you)))).

% This will evaporate silently if there's no trophy:
s_st(monster_attack, relation(c(has_item), v(monster), v(trophy))).
% The monster's trophy if created should be appropriate to the type of monster:
error(m("Monster trophy mismatch.", N)) :-
  setup(N, monster_attack),
  at(N, setup_arg(monster, Monster)),
  at(N, setup_arg(trophy, Trophy)),
  st(N, property(type, Monster, MType)),
  st(N, property(type, Trophy, TType)),
  not trophy(MType, TType).

intro_text(
  N,
  monster_attack,
  @fmt(
    "[[misc/as_you]], N#{}/they V#swoop/prs/{} down from the skies, clearly intent on V#attack/prp/{} N#you/them.",
    LMonst, LMonst, LMonst
  )
) :-
  at(N, setup_arg(monster, inst(MType, LMonst))),
  st(N, property(type, inst(MType, LMonst), dragon)),
  setup(N, monster_attack),
  story_op(N, initialize_node).
