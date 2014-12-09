% monster attack

possible_setup(monster_attack).

setup_argument_c(monster_attack, monster, inst(actor, monster)).
setup_argument_c(monster_attack, trophy, inst(item, trophy)).

s_st(monster_attack, property(c(name), v(monster), c("dragon"))).
% default number/person/gender
s_st(monster_attack, state(c(unintelligent), v(monster))).

s_st(monster_attack, property(c(name), v(trophy), c("dragon scale"))).
% default number/person/gender
s_st(monster_attack, relation(c(has_item), v(monster), v(trophy))).

s_st(
  monster_attack,
  relation(c(threatening), v(monster), c(inst(actor, the_party)))
).

intro_text(
  N,
  monster_attack,
  @fmt(
    "Suddenly N#{}/they V#swoop/prs/{} down from the skies, clearly intent on V#attack/prp/{} N#the_party/them.",
    LMonst, LMonst, LMonst
  )
) :-
  at(N, setup_arg(monster, inst(actor, LMonst))),
  st(N, property(name, inst(actor, LMonst), "dragon")),
  setup(N, monster_attack),
  story_op(N, initialize_node).
