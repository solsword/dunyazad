% merchants being robbed

possible_setup(being_robbed).

s_st(being_robbed, inst(item, treasure)).
s_st(being_robbed, property(name, inst(item, treasure), "treasure")).
% default number/person/gender
s_st(
  being_robbed,
  property(has_item, inst(actor, the_party), inst(item, treasure))
).

s_st(being_robbed, inst(actor, bandits)).
s_st(being_robbed, property(name, inst(actor, bandits), "bandits")).
s_st(being_robbed, property(number, inst(actor, bandits), plural)).
% default person/gender

s_st(being_robbed, inst(actor, merchants)).
s_st(being_robbed, property(name, inst(actor, merchants), "merchants")).
s_st(being_robbed, property(number, inst(actor, merchants), plural)).
% default person/gender

s_st(
  being_robbed,
  potential(problem, threatening(inst(actor, bandits), inst(actor,merchants)))
).

s_intro_text(
  being_robbed,
  "N#the_party/they V#continue/prs/the_party on N#the_party/their journey, when N#the_party/they V#come/prs/the_party across N#bandits/them V#attack/prp/bandits N#merchants/them."
).
