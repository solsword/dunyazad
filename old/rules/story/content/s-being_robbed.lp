% merchants being robbed

possible_setup(being_robbed).

st(N, inst(item, treasure)) :-
  setup(N, being_robbed).
st(N, property(name, inst(item, treasure), "treasure")) :-
  setup(N, being_robbed).
st(N, property(number, inst(item, treasure), singular)) :-
  setup(N, being_robbed).
st(N, property(gender, inst(item, treasure), neuter)) :-
  setup(N, being_robbed).
st(N, property(has_item, inst(actor, the_party), inst(item, treasure))) :-
  setup(N, being_robbed).

st(N, inst(actor, bandits)) :-
  setup(N, being_robbed).
st(N, property(name, inst(actor, bandits), "bandits")) :-
  setup(N, being_robbed).
st(N, property(number, inst(actor, bandits), plural)) :-
  setup(N, being_robbed).
st(N, property(gender, inst(actor, bandits), neuter)) :-
  setup(N, being_robbed).

st(N, inst(actor, merchants)) :-
  setup(N, being_robbed).
st(N, property(name, inst(actor, merchants), "merchants")) :-
  setup(N, being_robbed).
st(N, property(number, inst(actor, merchants), plural)) :-
  setup(N, being_robbed).
st(N, property(gender, inst(actor, merchants), neuter)) :-
  setup(N, being_robbed).

st(
  N,
  potential(problem, threatening(inst(actor, bandits), inst(actor,merchants)))
) :-
    setup(N, being_robbed).
