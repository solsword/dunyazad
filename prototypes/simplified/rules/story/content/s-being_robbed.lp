% merchants being robbed

possible_setup(being_robbed).

st(N, inst(item, treasure)) :-
  setup(N, being_robbed).
st(N, property(name, inst(item, treasure), "treasure")) :-
  setup(N, being_robbed).
% default number/person/gender
st(N, property(has_item, inst(actor, the_party), inst(item, treasure))) :-
  setup(N, being_robbed).

st(N, inst(actor, bandits)) :-
  setup(N, being_robbed).
st(N, property(name, inst(actor, bandits), "bandits")) :-
  setup(N, being_robbed).
st(N, property(number, inst(actor, bandits), plural)) :-
  setup(N, being_robbed).
% default person/gender

st(N, inst(actor, merchants)) :-
  setup(N, being_robbed).
st(N, property(name, inst(actor, merchants), "merchants")) :-
  setup(N, being_robbed).
st(N, property(number, inst(actor, merchants), plural)) :-
  setup(N, being_robbed).
% default person/gender

st(
  N,
  potential(problem, threatening(inst(actor, bandits), inst(actor,merchants)))
) :-
    setup(N, being_robbed).

intro_text(
  N,
  "N#the_party/they V#continue/prs/the_party on N#the_party/their journey, when N#the_party/they V#come/prs/the_party across N#bandits/them V#attack/prp/bandits N#merchants/them."
) :-
  setup(N, being_robbed).
