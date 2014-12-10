% a thief steals an item

%possible_setup(thief).

%setup_argument_c(thief, perp, inst(actor, thief)).
%setup_argument(thief, stolen, item).

% TODO: This setup!

%error("Thief stole wrong item.", N) :-
%  at(N, setup_arg(stolen, Item)),
%  X = {
%    successor(Prev, Opt, N)
%  },
%  X > {
%    st(Prev, relation(has_item, inst(actor, the_party), Item))
%      : successor(Prev, Opt, N),
%        not at(
%          Prev,
%          consequence(
%            Opt,
%            _not,
%            relation(has_item, inst(actor, the_party), Item)
%          )
%        );
%    at(Prev, consequence(Opt, relation(has_item, inst(actor, the_party), Item)))
%      : successor(Prev, Opt, N);
%  }.
%
%spontaneous(
%  at(N, setup_arg(stolen, Item)),
%  story_op(N, initialize_node).
%
%s_st(thief, property(c(name), v(monster), c("dragon"))).
%% default number/person/gender
%s_st(monster_attack, state(c(unintelligent), v(monster))).
%
%s_st(monster_attack, property(c(name), v(trophy), c("dragon scale"))).
%% default number/person/gender
%s_st(monster_attack, relation(c(has_item), v(monster), v(trophy))).
%
%s_st(
%  monster_attack,
%  relation(c(threatening), v(monster), c(inst(actor, the_party)))
%).
%
%intro_text(
%  N,
%  monster_attack,
%  @fmt(
%    "Suddenly N#{}/they V#swoop/prs/{} down from the skies, clearly intent on V#attack/prp/{} N#the_party/them.",
%    LMonst, LMonst, LMonst
%  )
%) :-
%  at(N, setup_arg(monster, inst(actor, LMonst))),
%  st(N, property(name, inst(actor, LMonst), "dragon")),
%  setup(N, monster_attack),
%  story_op(N, initialize_node).
