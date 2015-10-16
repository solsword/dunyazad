% a thief steals an item

%possible_setup(thief).

%setup_argument_create(thief, perp, inst(actor, thief)).
%setup_argument(thief, victim, actor).
%setup_argument(thief, goods, item).

%% The thief has the goods:
%s_st(thief, relation(c(has_item), v(perp), v(goods))).
%s_st(thief, relation(c(stolen_from), v(victim), v(goods))).
%
%error(m("Thief stole wrong item.")) :-
%  setup(N, thief),
%  at(N, setup_arg(goods, Goods)),
%  at(N, setup_arg(victim, Victim)),
%  0 != #sum {
%    1,story_node(Prev) : story_node(Prev), successor(Prev, Opt, N);
%    -1,story_node(Prev) :
%      story_node(Prev),
%      successor(Prev, Opt, N),
%      st(Prev, relation(has_item, Victim, Goods))
%  }.
