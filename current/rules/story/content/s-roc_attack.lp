% roc attack

possible_setup(roc_attack).

s_st(roc_attack, linst(actor, roc)).
s_st(roc_attack, property(name, linst(actor, roc), "roc")).
% default number/person/gender
s_st(roc_attack, state(unintelligent, linst(actor, roc))).

s_st(roc_attack, linst(item, roc_feather)).
s_st(roc_attack, property(name, linst(item, roc_feather), "roc feather")).
% default number/person/gender
s_st(roc_attack, relation(has_item, linst(actor,roc), linst(item,roc_feather))).

s_st(
  roc_attack,
  potential(problem, threatening(linst(actor, roc), inst(actor, the_party)))
).

s_intro_text(
  roc_attack,
  "N#the_party/they V#continue/prs/the_party on N#the_party/their journey, when suddenly N#roc/they V#swoop/prs/roc down from the skies, clearly intent on V#attack/prp/roc N#the_party/them."
).
