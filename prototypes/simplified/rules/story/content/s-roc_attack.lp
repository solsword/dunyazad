% roc attack

possible_setup(roc_attack).

st(N, inst(actor, roc)) :-
  setup(N, roc_attack).
st(N, property(name, inst(actor, roc), "roc")) :-
  setup(N, roc_attack).
st(N, property(number, inst(actor, roc), singular)) :-
  setup(N, roc_attack).
st(N, property(gender, inst(actor, roc), neuter)) :-
  setup(N, roc_attack).
st(N, state(unintelligent, inst(actor, roc))) :-
  setup(N, roc_attack).

st(N, inst(item, roc_feather)) :-
  setup(N, roc_attack).
st(N, property(name, inst(item, roc_feather), "roc feather")) :-
  setup(N, roc_attack).
st(N, property(number, inst(item, roc_feather), singular)) :-
  setup(N, roc_attack).
st(N, property(gender, inst(item, roc_feather), neuter)) :-
  setup(N, roc_attack).
st(N, relation(has_item, inst(actor, roc), inst(item, roc_feather))) :-
  setup(N, roc_attack).

st(
  N,
  potential(problem, threatening(inst(actor, roc), inst(actor, the_party)))
) :-
  setup(N, roc_attack).
