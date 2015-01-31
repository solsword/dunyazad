% a tavern

possible_setup(tavern).

setup_argument_create(tavern, tinker, inst(actor, tinker)).
setup_argument_symbol(tavern, tinker_gender, person_gender).

setup_argument_create(tavern, goods, inst(item, goods)).
% TODO: item type for these goods

setup_argument_create(tavern, noble, inst(actor, noble)).
setup_argument_symbol(tavern, noble_gender, person_gender).

setup_argument_create(tavern, innkeeper, inst(actor, innkeeper)).
setup_argument_symbol(tavern, innkeeper_gender, person_gender).

% The tinker
s_st(tavern, property(c(name), v(tinker), c("tinker"))).
s_st(tavern, property(c(gender), v(tinker), v(tinker_gender))).
% default number/person

% The tinker's goods
s_st(tavern, property(c(name), v(goods), c("goods"))).
s_st(tavern, property(c(number), v(goods), c(plural))).
% default person/gender
s_st(tavern, relation(c(has_item), v(tinker), v(goods))).

% The noble
s_st(tavern, property(c(name), v(noble), c("noble"))).
s_st(tavern, property(c(gender), v(noble), v(noble_gender))).
% default number/person

% The innkeeper
s_st(tavern, property(c(name), v(innkeeper), c("innkeeper"))).
s_st(tavern, property(c(gender), v(innkeeper), v(innkeeper_gender))).
% default number/person

% Potentials:
s_st(tavern, relation(c(selling), v(tinker), v(goods))).
s_st(tavern, state(c(bored), v(noble))).
s_st(tavern, state(c(gossip), v(innkeeper))).

intro_text(
  N,
  tavern,
  @fmt(
    "N#{}/they V#enter/prs/{} a bustling tavern. @CAP@N#{}/they V#/tend/prsc/{} the bar, where N#{}/they and N#{}/they V#relax/prsc/_plural. @CAP@N#{}/they V#look/prs/{} like N#{}/they might V#have/prs/{} something to sell, while N#{}/they just V#look/prs/{} bored.",
    you, you,
    Innkeeper, Innkeeper,
    Tinker, Noble,
    Tinker, Tinker, Tinker, Tinker,
    Noble, Noble
  )
) :-
  at(N, setup_arg(tinker, inst(actor, Tinker))),
  at(N, setup_arg(noble, inst(actor, Noble))),
  at(N, setup_arg(innkeeper, inst(actor, Innkeeper))),
  setup(N, tavern),
  story_op(N, initialize_node).
