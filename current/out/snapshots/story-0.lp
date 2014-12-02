story_node(root).
st(root, inst(actor, the_party)).
st(root, property(name, inst(actor, the_party), "party")).
st(root, property(person, inst(actor, the_party), second)).
st(root, property(number, inst(actor, the_party), plural)).
st(root, inst(item, supplies)).
st(root, property(name, inst(item, supplies), "supplies")).
st(root, property(number, inst(item, supplies), plural)).
st(root, relation(has_item, inst(actor, the_party), inst(item, supplies))).
st(root, inst(party_member, "Arel")).
st(root, property(name, inst(party_member, "Arel"), "Arel")).
st(root, property(gender, inst(party_member, "Arel"), feminine)).
st(root, property(proper, inst(party_member, "Arel"), false)).
st(root, inst(party_member, "Jain")).
st(root, property(name, inst(party_member, "Jain"), "Jain")).
st(root, property(gender, inst(party_member, "Jain"), masculine)).
st(root, property(proper, inst(party_member, "Jain"), false)).
st(root, property(travel_mode, inst(actor, the_party), walking)).
st(root, property(environment, inst(actor, the_party), hills)).
node_status_reached(root, uninitialized).
node_type(root, event).
story_op(root, build_options).
story_op(root, initialize_node).
at(root, option(1)).
at(root, arg(option(1), subject, inst(actor, the_party))).
at(root, action(option(1), travel_onwards)).
node_status_reached(root, initialized).
node_status_reached(root, built).
at(root, outcome(option(1), onwards)).
