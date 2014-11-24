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
story_op(root, build_options).
story_op(root, initialize_node).
st(root, inst(item, treasure)).
setup(root, being_robbed).
st(root, property(name, inst(item, treasure), "treasure")).
st(root, property(has_item, inst(actor, the_party), inst(item, treasure))).
st(root, inst(actor, bandits)).
st(root, property(name, inst(actor, bandits), "bandits")).
st(root, property(number, inst(actor, bandits), plural)).
st(root, inst(actor, merchants)).
st(root, property(name, inst(actor, merchants), "merchants")).
st(root, property(number, inst(actor, merchants), plural)).
st(root, potential(problem, threatening(inst(actor, bandits), inst(actor, merchants)))).
st(root, property(person, inst(item, treasure), third)).
st(root, property(person, inst(actor, bandits), third)).
st(root, property(person, inst(actor, merchants), third)).
st(root, property(number, inst(item, treasure), singular)).
st(root, property(gender, inst(item, treasure), neuter)).
st(root, property(gender, inst(actor, bandits), neuter)).
st(root, property(gender, inst(actor, merchants), neuter)).
st(root, property(proper, inst(item, treasure), true)).
st(root, property(proper, inst(actor, bandits), true)).
st(root, property(proper, inst(actor, merchants), true)).
at(root, arg(option(1), price, inst(item, supplies))).
at(root, action(option(1), pay_off)).
at(root, arg(option(1), listening, inst(actor, bandits))).
at(root, outcome(option(5), defeat)).
at(root, action(option(5), attack)).
at(root, emphasizes(option(1), potential(problem, threatening(inst(actor, bandits), inst(actor, merchants))))).
at(root, emphasizes(option(5), potential(problem, threatening(inst(actor, bandits), inst(actor, merchants))))).
at(root, arg(option(5), target, inst(actor, bandits))).
at(root, relevant(option(1))).
at(root, option(1)).
at(root, relevant(option(5))).
at(root, option(5)).
at(root, initiator(option(1), inst(actor, the_party))).
at(root, arg(option(1), asking, inst(actor, the_party))).
at(root, initiator(option(5), inst(actor, the_party))).
at(root, arg(option(5), aggressor, inst(actor, the_party))).
vignette(root, root).
potential_text(root, "N#bandits/they V#be/prs/bandits V#threaten/prp/bandits N#merchants/them").
option_text(root, option(5), "N#the_party/they V#attack/prs/the_party N#bandits/them.").
action_text(root, option(5), "N#the_party/they V#attack/prs/the_party N#bandits/them but V#be/prs/the_party defeated.").
at(root, outcome(option(1), no_deal)).
option_text(root, option(1), "N#the_party/they V#offer/prs/the_party N#bandits/them N#supplies/them.").
action_text(root, option(1), "N#the_party/they V#bargain/prs/the_party with N#bandits/them, but can't seem to V#reach/inf/the_party a deal.").
at(root, consequence(option(5), state(killed, inst(actor, the_party)))).
intro_text(root, "N#the_party/they V#continue/prs/the_party on N#the_party/their journey, when N#the_party/they V#come/prs/the_party across N#bandits/them V#attack/prp/bandits N#merchants/them.").
