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
story_op(root, add_branch_nodes).
successor(root, option(1), root_1).
story_node(root_1).
st(root_1, relation(has_item, inst(actor, the_party), inst(item, supplies))).
st(root_1, inst(actor, the_party)).
st(root_1, property(name, inst(actor, the_party), "party")).
st(root_1, property(person, inst(actor, the_party), second)).
st(root_1, property(number, inst(actor, the_party), plural)).
st(root_1, inst(item, supplies)).
st(root_1, property(name, inst(item, supplies), "supplies")).
st(root_1, property(number, inst(item, supplies), plural)).
st(root_1, inst(party_member, "Arel")).
st(root_1, property(name, inst(party_member, "Arel"), "Arel")).
st(root_1, property(gender, inst(party_member, "Arel"), feminine)).
st(root_1, property(proper, inst(party_member, "Arel"), false)).
st(root_1, inst(party_member, "Jain")).
st(root_1, property(name, inst(party_member, "Jain"), "Jain")).
st(root_1, property(gender, inst(party_member, "Jain"), masculine)).
st(root_1, property(proper, inst(party_member, "Jain"), false)).
st(root_1, property(travel_mode, inst(actor, the_party), walking)).
st(root_1, property(environment, inst(actor, the_party), hills)).
node_status_reached(root_1, uninitialized).
node_status_reached(root, branched).
story_op(root, add_surface).
node_status_reached(root, polished).
option_text(root, option(1), 0).
action_text(root, option(1), 0).
node_type(root_1, event).
story_op(root_1, build_options).
story_op(root_1, initialize_node).
at(root_1, option(1)).
vignette(root_1, root_1).
setup(root_1, roc_attack).
st(root_1, inst(actor, "roc_1")).
st(root_1, property(name, inst(actor, "roc_1"), "roc")).
st(root_1, state(unintelligent, inst(actor, "roc_1"))).
st(root_1, inst(item, "roc_feather_1")).
st(root_1, property(name, inst(item, "roc_feather_1"), "roc feather")).
st(root_1, relation(has_item, inst(actor, "roc_1"), inst(item, "roc_feather_1"))).
st(root_1, potential(problem, threatening(inst(actor, "roc_1"), inst(actor, the_party)))).
at(root_1, outcome(option(1), escape)).
at(root_1, action(option(1), flee)).
at(root_1, initiator(option(1), inst(actor, "roc_1"))).
at(root_1, arg(option(1), fearful, inst(actor, "roc_1"))).
at(root_1, arg(option(1), from, inst(actor, the_party))).
node_status_reached(root_1, initialized).
node_status_reached(root_1, built).
unresolved_potential(root_1, option(1), potential(problem, threatening(inst(actor, "roc_1"), inst(actor, the_party)))).
intro_text(root_1, "N#the_party/they V#continue/prs/the_party on N#the_party/their journey, when suddenly N#roc/they V#swoop/prs/roc down from the skies, clearly intent on V#attack/prp/roc N#the_party/them.").
story_op(root_1, add_branch_nodes).
successor(root_1, option(1), root_1_1).
story_node(root_1_1).
st(root_1_1, relation(has_item, inst(actor, the_party), inst(item, supplies))).
st(root_1_1, relation(has_item, inst(actor, "roc_1"), inst(item, "roc_feather_1"))).
st(root_1_1, inst(actor, the_party)).
st(root_1_1, property(name, inst(actor, the_party), "party")).
st(root_1_1, property(person, inst(actor, the_party), second)).
st(root_1_1, property(number, inst(actor, the_party), plural)).
st(root_1_1, inst(item, supplies)).
st(root_1_1, property(name, inst(item, supplies), "supplies")).
st(root_1_1, property(number, inst(item, supplies), plural)).
st(root_1_1, inst(party_member, "Arel")).
st(root_1_1, property(name, inst(party_member, "Arel"), "Arel")).
st(root_1_1, property(gender, inst(party_member, "Arel"), feminine)).
st(root_1_1, property(proper, inst(party_member, "Arel"), false)).
st(root_1_1, inst(party_member, "Jain")).
st(root_1_1, property(name, inst(party_member, "Jain"), "Jain")).
st(root_1_1, property(gender, inst(party_member, "Jain"), masculine)).
st(root_1_1, property(proper, inst(party_member, "Jain"), false)).
st(root_1_1, property(travel_mode, inst(actor, the_party), walking)).
st(root_1_1, property(environment, inst(actor, the_party), hills)).
st(root_1_1, inst(actor, "roc_1")).
st(root_1_1, property(name, inst(actor, "roc_1"), "roc")).
st(root_1_1, state(unintelligent, inst(actor, "roc_1"))).
st(root_1_1, inst(item, "roc_feather_1")).
st(root_1_1, property(name, inst(item, "roc_feather_1"), "roc feather")).
st(root_1_1, potential(problem, threatening(inst(actor, "roc_1"), inst(actor, the_party)))).
node_type(root_1_1, choice).
node_status_reached(root_1_1, uninitialized).
node_status_reached(root_1, branched).
story_op(root_1, add_surface).
node_status_reached(root_1, polished).
potential_text(root_1, "N#roc_1/they V#be/prs/roc_1 V#threaten/prp/roc_1 N#the_party/them").
option_text(root_1, option(1), "N#roc_1/they V#flee/prs/roc_1 from N#the_party/them.").
action_text(root_1, option(1), "N#roc_1/they V#flee/prs/roc_1 from N#the_party/them and escape.").
vignette(root_1_1, root_1).
story_op(root_1_1, build_options).
story_op(root_1_1, initialize_node).
at(root_1_1, action(option(2), flee)).
at(root_1_1, arg(option(2), from, inst(actor, "roc_1"))).
at(root_1_1, arg(option(2), fearful, inst(actor, the_party))).
at(root_1_1, emphasizes(option(2), potential(problem, threatening(inst(actor, "roc_1"), inst(actor, the_party))))).
at(root_1_1, emphasizes(option(3), potential(problem, threatening(inst(actor, "roc_1"), inst(actor, the_party))))).
at(root_1_1, arg(option(3), target, inst(actor, "roc_1"))).
at(root_1_1, outcome(option(3), defeat)).
at(root_1_1, action(option(3), attack)).
at(root_1_1, outcome(option(2), failure)).
at(root_1_1, relevant(option(2))).
at(root_1_1, option(2)).
at(root_1_1, relevant(option(3))).
at(root_1_1, option(3)).
at(root_1_1, initiator(option(2), inst(actor, the_party))).
at(root_1_1, initiator(option(3), inst(actor, the_party))).
at(root_1_1, arg(option(3), aggressor, inst(actor, the_party))).
node_status_reached(root_1_1, initialized).
node_status_reached(root_1_1, built).
unresolved_potential(root_1_1, option(2), potential(problem, threatening(inst(actor, "roc_1"), inst(actor, the_party)))).
unresolved_potential(root_1_1, option(3), potential(problem, threatening(inst(actor, "roc_1"), inst(actor, the_party)))).
at(root_1_1, consequence(option(3), state(injured, inst(actor, "roc_1")))).
at(root_1_1, consequence(option(3), state(killed, inst(actor, the_party)))).
