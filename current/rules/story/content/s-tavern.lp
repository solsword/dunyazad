% a tavern

possible_setup(tavern).

setup_argument_create(tavern, tinker, inst(actor, tinker)).
setup_argument_create(tavern, goods, inst(item, treasure)).
% TODO: Variety here

setup_argument_create(tavern, noble, inst(actor, noble)).

setup_argument_create(tavern, innkeeper, inst(actor, innkeeper)).

% The tinker's goods
s_st(tavern, relation(c(has_item), v(tinker), v(goods))).

% Potentials:
s_st(tavern, relation(c(selling), v(tinker), v(goods))).
s_st(tavern, state(c(bored), v(noble))).
s_st(tavern, state(c(knows_gossip), v(innkeeper))).

intro_text(
  N,
  tavern,
  @fmt(
    "N#{}/they V#enter/prs/{} a bustling tavern. @CAP@N#{}/they V#tend/prsc/{} the bar, where N#{}/they and N#{}/they V#relax/prsc/_plural. @CAP@N#{}/they V#look/prs/{} like N#{}/they V#might/prs/{} V#have/rinf/{} something to sell, while N#{}/they just V#look/prs/{} bored.",
    you, you,
    Innkeeper, Innkeeper,
    Tinker, Noble,
    Tinker, Tinker, Tinker, Tinker, Tinker,
    Noble, Noble
  )
) :-
  at(N, setup_arg(tinker, inst(actor, Tinker))),
  at(N, setup_arg(noble, inst(actor, Noble))),
  at(N, setup_arg(innkeeper, inst(actor, Innkeeper))),
  setup(N, tavern),
  story_op(N, initialize_node).
