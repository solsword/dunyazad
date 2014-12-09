% a thief steals an item

%possible_setup(thief).
%
%1 = {
%  stolen(N, inst(item, Stolen))
%    : st(N, inst(item Stolen)),
%      st(N, relation(has_item, inst(actor, the_party), inst(item, Stolen)))
%} :-
%  setup(N, thief).
%
%% TODO: This setup!!
%
%s_st(thief, relation(has_item, linst(actor, thief), Stolen)) :-
%  setup(N, thief),
%  stolen(N, Stolen).
%
%s_st(thief, linst(actor, bandits)).
%s_st(thief, property(name, linst(actor, bandits), "bandits")).
%s_st(thief, property(number, linst(actor, bandits), plural)).
%% default person/gender
%
%s_st(thief, linst(actor, merchants)).
%s_st(thief, property(name, linst(actor,merchants), "merchants")).
%s_st(thief, property(number, linst(actor, merchants), plural)).
%% default person/gender
%
%s_st(thief, linst(item, goods)).
%s_st(thief, property(name, linst(item, goods), "goods")).
%% default number/person/gender
%s_st(
%  thief,
%  relation(has_item, linst(actor, merchants), linst(item, goods))
%).
%
%s_st(
%  thief,
%  relation(threatening, linst(actor, bandits), linst(actor,merchants))
%).
%
%s_intro_text(
%  thief,
%  "N#the_party/they V#continue/prs/the_party on N#the_party/their journey, when N#the_party/they V#come/prs/the_party across N#bandits/them V#attack/prp/bandits N#merchants/them."
%).
