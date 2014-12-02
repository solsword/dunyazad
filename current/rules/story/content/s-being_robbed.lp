% merchants being robbed

possible_setup(being_robbed).

s_st(being_robbed, linst(item, goods)).
s_st(being_robbed, property(name, linst(item, goods), "goods")).
% default number/person/gender
% TODO: give the goods to the merchants!
s_st(
  being_robbed,
  relation(has_item, inst(actor, the_party), linst(item, goods))
).

s_st(being_robbed, linst(actor, bandits)).
s_st(being_robbed, property(name, linst(actor, bandits), "bandits")).
s_st(being_robbed, property(number, linst(actor, bandits), plural)).
% default person/gender

s_st(being_robbed, linst(actor, merchants)).
s_st(being_robbed, property(name, linst(actor, merchants), "merchants")).
s_st(being_robbed, property(number, linst(actor, merchants), plural)).
% default person/gender

s_st(
  being_robbed,
  potential(problem, threatening(linst(actor, bandits), linst(actor,merchants)))
).

s_intro_text(
  being_robbed,
  "N#the_party/they V#continue/prs/the_party on N#the_party/their journey, when N#the_party/they V#come/prs/the_party across N#bandits/them V#attack/prp/bandits N#merchants/them."
).
