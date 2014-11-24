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
story_op(root, add_branch_nodes).
successor(root, option(1), "root:1").
successor(root, option(5), "root:5").
story_node("root:1").
story_node("root:5").
st("root:5", state(killed, inst(actor, the_party))).
st("root:1", relation(has_item, inst(actor, the_party), inst(item, supplies))).
st("root:5", relation(has_item, inst(actor, the_party), inst(item, supplies))).
st("root:1", inst(actor, the_party)).
st("root:5", inst(actor, the_party)).
st("root:1", property(name, inst(actor, the_party), "party")).
st("root:5", property(name, inst(actor, the_party), "party")).
st("root:1", property(person, inst(actor, the_party), second)).
st("root:5", property(person, inst(actor, the_party), second)).
st("root:1", property(number, inst(actor, the_party), plural)).
st("root:5", property(number, inst(actor, the_party), plural)).
st("root:1", inst(item, supplies)).
st("root:5", inst(item, supplies)).
st("root:1", property(name, inst(item, supplies), "supplies")).
st("root:5", property(name, inst(item, supplies), "supplies")).
st("root:1", property(number, inst(item, supplies), plural)).
st("root:5", property(number, inst(item, supplies), plural)).
st("root:1", inst(party_member, "Arel")).
st("root:5", inst(party_member, "Arel")).
st("root:1", property(name, inst(party_member, "Arel"), "Arel")).
st("root:5", property(name, inst(party_member, "Arel"), "Arel")).
st("root:1", property(gender, inst(party_member, "Arel"), feminine)).
st("root:5", property(gender, inst(party_member, "Arel"), feminine)).
st("root:1", property(proper, inst(party_member, "Arel"), false)).
st("root:5", property(proper, inst(party_member, "Arel"), false)).
st("root:1", inst(party_member, "Jain")).
st("root:5", inst(party_member, "Jain")).
st("root:1", property(name, inst(party_member, "Jain"), "Jain")).
st("root:5", property(name, inst(party_member, "Jain"), "Jain")).
st("root:1", property(gender, inst(party_member, "Jain"), masculine)).
st("root:5", property(gender, inst(party_member, "Jain"), masculine)).
st("root:1", property(proper, inst(party_member, "Jain"), false)).
st("root:5", property(proper, inst(party_member, "Jain"), false)).
st("root:1", property(travel_mode, inst(actor, the_party), walking)).
st("root:5", property(travel_mode, inst(actor, the_party), walking)).
st("root:1", property(environment, inst(actor, the_party), hills)).
st("root:5", property(environment, inst(actor, the_party), hills)).
st("root:1", inst(item, treasure)).
st("root:5", inst(item, treasure)).
st("root:1", property(name, inst(item, treasure), "treasure")).
st("root:5", property(name, inst(item, treasure), "treasure")).
st("root:1", property(has_item, inst(actor, the_party), inst(item, treasure))).
st("root:5", property(has_item, inst(actor, the_party), inst(item, treasure))).
st("root:1", inst(actor, bandits)).
st("root:5", inst(actor, bandits)).
st("root:1", property(name, inst(actor, bandits), "bandits")).
st("root:5", property(name, inst(actor, bandits), "bandits")).
st("root:1", property(number, inst(actor, bandits), plural)).
st("root:5", property(number, inst(actor, bandits), plural)).
st("root:1", inst(actor, merchants)).
st("root:5", inst(actor, merchants)).
st("root:1", property(name, inst(actor, merchants), "merchants")).
st("root:5", property(name, inst(actor, merchants), "merchants")).
st("root:1", property(number, inst(actor, merchants), plural)).
st("root:5", property(number, inst(actor, merchants), plural)).
st("root:1", potential(problem, threatening(inst(actor, bandits), inst(actor, merchants)))).
st("root:5", potential(problem, threatening(inst(actor, bandits), inst(actor, merchants)))).
st("root:1", property(person, inst(item, treasure), third)).
st("root:5", property(person, inst(item, treasure), third)).
st("root:1", property(person, inst(actor, bandits), third)).
st("root:5", property(person, inst(actor, bandits), third)).
st("root:1", property(person, inst(actor, merchants), third)).
st("root:5", property(person, inst(actor, merchants), third)).
st("root:1", property(number, inst(item, treasure), singular)).
st("root:5", property(number, inst(item, treasure), singular)).
st("root:1", property(gender, inst(item, treasure), neuter)).
st("root:5", property(gender, inst(item, treasure), neuter)).
st("root:1", property(gender, inst(actor, bandits), neuter)).
st("root:5", property(gender, inst(actor, bandits), neuter)).
st("root:1", property(gender, inst(actor, merchants), neuter)).
st("root:5", property(gender, inst(actor, merchants), neuter)).
st("root:1", property(proper, inst(item, treasure), true)).
st("root:5", property(proper, inst(item, treasure), true)).
st("root:1", property(proper, inst(actor, bandits), true)).
st("root:5", property(proper, inst(actor, bandits), true)).
st("root:1", property(proper, inst(actor, merchants), true)).
st("root:5", property(proper, inst(actor, merchants), true)).
potential_text("root:1", "N#bandits/they V#be/prs/bandits V#threaten/prp/bandits N#merchants/them").
potential_text("root:5", "N#bandits/they V#be/prs/bandits V#threaten/prp/bandits N#merchants/them").
vignette("root:1", root).
vignette("root:5", root).
story_op("root:5", build_options).
story_op("root:5", initialize_node).
at("root:5", arg(option(2), price, inst(item, supplies))).
at("root:5", action(option(2), pay_off)).
at("root:5", arg(option(2), listening, inst(actor, bandits))).
at("root:5", emphasizes(option(2), potential(problem, threatening(inst(actor, bandits), inst(actor, merchants))))).
at("root:5", emphasizes(option(5), potential(problem, threatening(inst(actor, bandits), inst(actor, merchants))))).
at("root:5", arg(option(5), target, inst(actor, bandits))).
at("root:5", outcome(option(5), defeat)).
at("root:5", action(option(5), attack)).
at("root:5", relevant(option(2))).
at("root:5", option(2)).
at("root:5", relevant(option(5))).
at("root:5", option(5)).
at("root:5", initiator(option(2), inst(actor, the_party))).
at("root:5", arg(option(2), asking, inst(actor, the_party))).
at("root:5", initiator(option(5), inst(actor, the_party))).
at("root:5", arg(option(5), aggressor, inst(actor, the_party))).
option_text("root:5", option(5), "N#the_party/they V#attack/prs/the_party N#bandits/them.").
action_text("root:5", option(5), "N#the_party/they V#attack/prs/the_party N#bandits/them but V#be/prs/the_party defeated.").
at("root:5", outcome(option(2), no_deal)).
option_text("root:5", option(2), "N#the_party/they V#offer/prs/the_party N#bandits/them N#supplies/them.").
action_text("root:5", option(2), "N#the_party/they V#bargain/prs/the_party with N#bandits/them, but can't seem to V#reach/inf/the_party a deal.").
at("root:5", consequence(option(5), state(killed, inst(actor, the_party)))).
story_op("root:5", add_branch_nodes).
successor("root:5", option(5), "root:5:5").
story_node("root:5:5").
st("root:5:5", state(killed, inst(actor, the_party))).
successor("root:5", option(2), "root:5:5").
st("root:5:5", relation(has_item, inst(actor, the_party), inst(item, supplies))).
st("root:5:5", inst(actor, the_party)).
st("root:5:5", property(name, inst(actor, the_party), "party")).
st("root:5:5", property(person, inst(actor, the_party), second)).
st("root:5:5", property(number, inst(actor, the_party), plural)).
st("root:5:5", inst(item, supplies)).
st("root:5:5", property(name, inst(item, supplies), "supplies")).
st("root:5:5", property(number, inst(item, supplies), plural)).
st("root:5:5", inst(party_member, "Arel")).
st("root:5:5", property(name, inst(party_member, "Arel"), "Arel")).
st("root:5:5", property(gender, inst(party_member, "Arel"), feminine)).
st("root:5:5", property(proper, inst(party_member, "Arel"), false)).
st("root:5:5", inst(party_member, "Jain")).
st("root:5:5", property(name, inst(party_member, "Jain"), "Jain")).
st("root:5:5", property(gender, inst(party_member, "Jain"), masculine)).
st("root:5:5", property(proper, inst(party_member, "Jain"), false)).
st("root:5:5", property(travel_mode, inst(actor, the_party), walking)).
st("root:5:5", property(environment, inst(actor, the_party), hills)).
st("root:5:5", inst(item, treasure)).
st("root:5:5", property(name, inst(item, treasure), "treasure")).
st("root:5:5", property(has_item, inst(actor, the_party), inst(item, treasure))).
st("root:5:5", inst(actor, bandits)).
st("root:5:5", property(name, inst(actor, bandits), "bandits")).
st("root:5:5", property(number, inst(actor, bandits), plural)).
st("root:5:5", inst(actor, merchants)).
st("root:5:5", property(name, inst(actor, merchants), "merchants")).
st("root:5:5", property(number, inst(actor, merchants), plural)).
st("root:5:5", potential(problem, threatening(inst(actor, bandits), inst(actor, merchants)))).
st("root:5:5", property(person, inst(item, treasure), third)).
st("root:5:5", property(person, inst(actor, bandits), third)).
st("root:5:5", property(person, inst(actor, merchants), third)).
st("root:5:5", property(number, inst(item, treasure), singular)).
st("root:5:5", property(gender, inst(item, treasure), neuter)).
st("root:5:5", property(gender, inst(actor, bandits), neuter)).
st("root:5:5", property(gender, inst(actor, merchants), neuter)).
st("root:5:5", property(proper, inst(item, treasure), true)).
st("root:5:5", property(proper, inst(actor, bandits), true)).
st("root:5:5", property(proper, inst(actor, merchants), true)).
potential_text("root:5:5", "N#bandits/they V#be/prs/bandits V#threaten/prp/bandits N#merchants/them").
vignette("root:5:5", root).
story_op("root:1", build_options).
story_op("root:1", initialize_node).
at("root:1", arg(option(3), price, inst(item, supplies))).
at("root:1", action(option(3), pay_off)).
at("root:1", arg(option(3), listening, inst(actor, bandits))).
at("root:1", emphasizes(option(3), potential(problem, threatening(inst(actor, bandits), inst(actor, merchants))))).
at("root:1", emphasizes(option(5), potential(problem, threatening(inst(actor, bandits), inst(actor, merchants))))).
at("root:1", arg(option(5), target, inst(actor, bandits))).
at("root:1", outcome(option(5), defeat)).
at("root:1", action(option(5), attack)).
at("root:1", relevant(option(3))).
at("root:1", option(3)).
at("root:1", relevant(option(5))).
at("root:1", option(5)).
at("root:1", initiator(option(3), inst(actor, the_party))).
at("root:1", arg(option(3), asking, inst(actor, the_party))).
at("root:1", initiator(option(5), inst(actor, the_party))).
at("root:1", arg(option(5), aggressor, inst(actor, the_party))).
option_text("root:1", option(5), "N#the_party/they V#attack/prs/the_party N#bandits/them.").
action_text("root:1", option(5), "N#the_party/they V#attack/prs/the_party N#bandits/them but V#be/prs/the_party defeated.").
at("root:1", outcome(option(3), no_deal)).
option_text("root:1", option(3), "N#the_party/they V#offer/prs/the_party N#bandits/them N#supplies/them.").
action_text("root:1", option(3), "N#the_party/they V#bargain/prs/the_party with N#bandits/them, but can't seem to V#reach/inf/the_party a deal.").
at("root:1", consequence(option(5), state(killed, inst(actor, the_party)))).
at("root:1", consequence(option(5), state(injured, inst(actor, bandits)))).
story_op("root:1", add_branch_nodes).
successor("root:1", option(3), "root:1:3").
successor("root:1", option(5), "root:1:5").
story_node("root:1:3").
story_node("root:1:5").
st("root:1:5", state(killed, inst(actor, the_party))).
st("root:1:5", state(injured, inst(actor, bandits))).
st("root:5", state(injured, inst(actor, bandits))).
st("root:5:5", state(injured, inst(actor, bandits))).
st("root:1:3", relation(has_item, inst(actor, the_party), inst(item, supplies))).
st("root:1:5", relation(has_item, inst(actor, the_party), inst(item, supplies))).
at(root, consequence(option(5), state(injured, inst(actor, bandits)))).
st("root:1:3", inst(actor, the_party)).
st("root:1:5", inst(actor, the_party)).
st("root:1:3", property(name, inst(actor, the_party), "party")).
st("root:1:5", property(name, inst(actor, the_party), "party")).
st("root:1:3", property(person, inst(actor, the_party), second)).
st("root:1:5", property(person, inst(actor, the_party), second)).
st("root:1:3", property(number, inst(actor, the_party), plural)).
st("root:1:5", property(number, inst(actor, the_party), plural)).
st("root:1:3", inst(item, supplies)).
st("root:1:5", inst(item, supplies)).
st("root:1:3", property(name, inst(item, supplies), "supplies")).
st("root:1:5", property(name, inst(item, supplies), "supplies")).
st("root:1:3", property(number, inst(item, supplies), plural)).
st("root:1:5", property(number, inst(item, supplies), plural)).
st("root:1:3", inst(party_member, "Arel")).
st("root:1:5", inst(party_member, "Arel")).
st("root:1:3", property(name, inst(party_member, "Arel"), "Arel")).
st("root:1:5", property(name, inst(party_member, "Arel"), "Arel")).
st("root:1:3", property(gender, inst(party_member, "Arel"), feminine)).
st("root:1:5", property(gender, inst(party_member, "Arel"), feminine)).
st("root:1:3", property(proper, inst(party_member, "Arel"), false)).
st("root:1:5", property(proper, inst(party_member, "Arel"), false)).
st("root:1:3", inst(party_member, "Jain")).
st("root:1:5", inst(party_member, "Jain")).
st("root:1:3", property(name, inst(party_member, "Jain"), "Jain")).
st("root:1:5", property(name, inst(party_member, "Jain"), "Jain")).
st("root:1:3", property(gender, inst(party_member, "Jain"), masculine)).
st("root:1:5", property(gender, inst(party_member, "Jain"), masculine)).
st("root:1:3", property(proper, inst(party_member, "Jain"), false)).
st("root:1:5", property(proper, inst(party_member, "Jain"), false)).
st("root:1:3", property(travel_mode, inst(actor, the_party), walking)).
st("root:1:5", property(travel_mode, inst(actor, the_party), walking)).
st("root:1:3", property(environment, inst(actor, the_party), hills)).
st("root:1:5", property(environment, inst(actor, the_party), hills)).
st("root:1:3", inst(item, treasure)).
st("root:1:5", inst(item, treasure)).
st("root:1:3", property(name, inst(item, treasure), "treasure")).
st("root:1:5", property(name, inst(item, treasure), "treasure")).
st("root:1:3", property(has_item, inst(actor, the_party), inst(item, treasure))).
st("root:1:5", property(has_item, inst(actor, the_party), inst(item, treasure))).
st("root:1:3", inst(actor, bandits)).
st("root:1:5", inst(actor, bandits)).
st("root:1:3", property(name, inst(actor, bandits), "bandits")).
st("root:1:5", property(name, inst(actor, bandits), "bandits")).
st("root:1:3", property(number, inst(actor, bandits), plural)).
st("root:1:5", property(number, inst(actor, bandits), plural)).
st("root:1:3", inst(actor, merchants)).
st("root:1:5", inst(actor, merchants)).
st("root:1:3", property(name, inst(actor, merchants), "merchants")).
st("root:1:5", property(name, inst(actor, merchants), "merchants")).
st("root:1:3", property(number, inst(actor, merchants), plural)).
st("root:1:5", property(number, inst(actor, merchants), plural)).
st("root:1:3", potential(problem, threatening(inst(actor, bandits), inst(actor, merchants)))).
st("root:1:5", potential(problem, threatening(inst(actor, bandits), inst(actor, merchants)))).
st("root:1:3", property(person, inst(item, treasure), third)).
st("root:1:5", property(person, inst(item, treasure), third)).
st("root:1:3", property(person, inst(actor, bandits), third)).
st("root:1:5", property(person, inst(actor, bandits), third)).
st("root:1:3", property(person, inst(actor, merchants), third)).
st("root:1:5", property(person, inst(actor, merchants), third)).
st("root:1:3", property(number, inst(item, treasure), singular)).
st("root:1:5", property(number, inst(item, treasure), singular)).
st("root:1:3", property(gender, inst(item, treasure), neuter)).
st("root:1:5", property(gender, inst(item, treasure), neuter)).
st("root:1:3", property(gender, inst(actor, bandits), neuter)).
st("root:1:5", property(gender, inst(actor, bandits), neuter)).
st("root:1:3", property(gender, inst(actor, merchants), neuter)).
st("root:1:5", property(gender, inst(actor, merchants), neuter)).
st("root:1:3", property(proper, inst(item, treasure), true)).
st("root:1:5", property(proper, inst(item, treasure), true)).
st("root:1:3", property(proper, inst(actor, bandits), true)).
st("root:1:5", property(proper, inst(actor, bandits), true)).
st("root:1:3", property(proper, inst(actor, merchants), true)).
st("root:1:5", property(proper, inst(actor, merchants), true)).
potential_text("root:1:3", "N#bandits/they V#be/prs/bandits V#threaten/prp/bandits N#merchants/them").
potential_text("root:1:5", "N#bandits/they V#be/prs/bandits V#threaten/prp/bandits N#merchants/them").
vignette("root:1:3", root).
vignette("root:1:5", root).
