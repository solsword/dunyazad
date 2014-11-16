% merchants being robbed

tn(N) :- target_node(N).

setup(N, being_robbed) :- tn(N).

st(N, inst(item, treasure)) :- tn(N).
st(N, property(name, inst(item, treasure), "treasure")) :- tn(N).
st(N, property(number, inst(item, treasure), singular)) :- tn(N).
st(N, property(gender, inst(item, treasure), neuter)) :- tn(N).
st(N, property(has_item, inst(actor, the_party), inst(item, treasure))) :-tn(N).

st(N, inst(actor, bandits)) :- tn(N).
st(N, property(name, inst(actor, bandits), "bandits")) :- tn(N).
st(N, property(number, inst(actor, bandits), plural)) :- tn(N).
st(N, property(gender, inst(actor, bandits), neuter)) :- tn(N).

st(N, inst(actor, merchants)) :- tn(N).
st(N, property(name, inst(actor, merchants), "merchants")) :- tn(N).
st(N, property(number, inst(actor, merchants), plural)) :- tn(N).
st(N, property(gender, inst(actor, merchants), neuter)) :- tn(N).

st(N, problem(threatening(inst(actor, bandits), inst(actor,merchants)))):-tn(N).
