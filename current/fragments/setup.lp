story_node(root).

st(root, inst(actor, the_party)).
st(root, property(name, inst(actor, the_party), "party")).
st(root, property(person, inst(actor, the_party), second)).
st(root, property(number, inst(actor, the_party), plural)).
% default gender

st(root, inst(item, supplies)).
st(root, property(name, inst(item, supplies), "supplies")).
st(root, property(number, inst(item, supplies), plural)).
% default person/gender
st(root, relation(has_item, inst(actor, the_party), inst(item, supplies))).

st(root, inst(party_member, "Arel")).
st(root, property(name, inst(party_member, "Arel"), "Arel")).
st(root, property(gender, inst(party_member, "Arel"), feminine)).
st(root, property(proper, inst(party_member, "Arel"), false)).
% default person/number
st(root, inst(party_member, "Jain")).
st(root, property(name, inst(party_member, "Jain"), "Jain")).
st(root, property(gender, inst(party_member, "Jain"), masculine)).
st(root, property(proper, inst(party_member, "Jain"), false)).
% default person/number

st(root, property(travel_mode, inst(actor, the_party), walking)).
st(root, property(environment, inst(actor, the_party), hills)).
