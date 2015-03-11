% monster attack

possible_setup(monster_attack).

setup_argument_create(monster_attack, monster, monster).
setup_argument_create_n(monster_attack, trophy, trophy, 0, 1).

s_st(monster_attack, state(c(unintelligent), v(monster))).
s_st(monster_attack, property(c(has_skill), v(monster), c(fighting))).

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
