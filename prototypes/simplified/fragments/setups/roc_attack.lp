% roc attack

tn(N) :- target_node(N).

st(N, inst(actor, roc)) :- tn(N).
st(N, property(name, inst(actor, roc), "roc")) :- tn(N).
st(N, property(number, inst(actor, roc), singular)) :- tn(N).
st(N, property(gender, inst(actor, roc), neuter)) :- tn(N).
st(N, state(unintelligent, inst(actor, roc))) :- tn(N).

st(N, inst(item, roc_feather)) :- tn(N).
st(N, property(name, inst(item, roc_feather), "roc feather")) :- tn(N).
st(N, property(number, inst(item, roc_feather), singular)) :- tn(N).
st(N, property(gender, inst(item, roc_feather), neuter)) :- tn(N).
st(N, relation(has_item, inst(actor, roc), inst(item, roc_feather))) :- tn(N).

st(N, problem(threatening(inst(actor, roc), inst(actor, the_party)))):- tn(N).
