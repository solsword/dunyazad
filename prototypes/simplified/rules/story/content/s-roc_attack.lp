% roc attack

possible_setup(roc_attack).

st(N, inst(actor, roc)) :-
  setup(N, roc_attack).
st(N, property(name, inst(actor, roc), "roc")) :-
  setup(N, roc_attack).
% default number/person/gender
st(N, state(unintelligent, inst(actor, roc))) :-
  setup(N, roc_attack).

st(N, inst(item, roc_feather)) :-
  setup(N, roc_attack).
st(N, property(name, inst(item, roc_feather), "roc feather")) :-
  setup(N, roc_attack).
% default number/person/gender
st(N, relation(has_item, inst(actor, roc), inst(item, roc_feather))) :-
  setup(N, roc_attack).

st(
  N,
  potential(problem, threatening(inst(actor, roc), inst(actor, the_party)))
) :-
  setup(N, roc_attack).

intro_text(
  N,
  "N#the_party/they V#continue/prs/the_party on N#the_party/their journey, when suddenly N#roc/they V#swoop/prs/roc down from the skies, clearly intent on V#attack/prp/roc N#the_party/them."
) :-
  setup(N, roc_attack).
