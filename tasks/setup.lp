status(completed).

story_node(root).

st(root, inst(actor, the_party)).
st(root, property(name, inst(actor, the_party), "party")).
st(root, property(number, inst(actor, the_party), plural)).
st(root, property(gender, inst(actor, the_party), neuter)).

st(root, inst(item, supplies)).
st(root, property(name, inst(item, supplies), "supplies")).
st(root, property(number, inst(item, supplies), plural)).
st(root, property(gender, inst(item, supplies), neuter)).
st(root, relation(has_item, inst(actor, the_party), inst(item, supplies))).

st(root, inst(party_member, "Arel")).
st(root, property(name, inst(party_member, "Arel"), "Arel")).
st(root, property(number, inst(party_member, "Arel"), singular)).
st(root, property(gender, inst(party_member, "Arel"), feminine)).
st(root, inst(party_member, "Jain")).
st(root, property(name, inst(party_member, "Jain"), "Jain")).
st(root, property(number, inst(party_member, "Jain"), singular)).
st(root, property(gender, inst(party_member, "Jain"), masculine)).

st(root, property(travel_mode, inst(actor, the_party), walking)).
st(root, property(environment, inst(actor, the_party), hills)).
