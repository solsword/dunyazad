experiment_template(dilemma).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
setup_priority(on_sale, 4).
setup_priority(tavern, 6).
setup_priority(threatened_innocents, 1).
setup_priority(market, 3).
setup_priority(monster_attack, 2).
setup_priority(healer, 5).
at(root, text_mode(example)).
st(root, property(type, inst(actor, you), person)).
st(root, property(has_skill, inst(actor, you), literacy)).
st(root, inst(actor, you)).
st(root, state(party_member, inst(actor, you))).
st(root, property(name, inst(actor, you), "Dunyazad")).
st(root, property(person, inst(actor, you), second)).
st(root, property(gender, inst(actor, you), feminine)).
st(root, property(determined, inst(actor, you), false)).
at(root, option(1)).
vignette(root, root).
setup(root, monster_attack).
at(root, setup_arg(monster, inst(actor, monster_20))).
st(root, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root, relation(has_item, inst(actor, you), inst(item, legends_book_30))).
st(root, property(number, inst(item, dragon_scale_23), singular)).
st(root, property(number, inst(item, ring_25), singular)).
st(root, property(number, inst(item, legends_book_30), singular)).
st(root, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root, property(name, inst(item, ring_25), "ring")).
st(root, property(name, inst(item, legends_book_30), "book of legends")).
st(root, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root, property(type, inst(item, ring_25), ring)).
st(root, property(type, inst(item, legends_book_30), legends_book)).
st(root, inst(item, dragon_scale_23)).
st(root, inst(item, ring_25)).
st(root, inst(item, legends_book_30)).
spontaneous(st(root, inst(actor, monster_20))).
st(root, inst(actor, monster_20)).
at(root, consequence_of(option(1), o(get_injured, injured), state(injured, inst(actor, you)))).
at(root, arg(option(1), pacifier, inst(actor, you))).
at(root, action(option(1), pacify)).
at(root, consequence_of(option(3), o(get_injured, injured), state(injured, inst(actor, you)))).
at(root, arg(option(3), fearful, inst(actor, you))).
at(root, action(option(3), flee)).
at(root, action(option(4), attack)).
at(root, consequence_of(option(4), o(aggressor_state, killed), state(killed, inst(actor, you)))).
at(root, arg(option(4), aggressor, inst(actor, you))).
at(root, consequence_of(option(4), o(aggressor_state, injured), state(injured, inst(actor, you)))).
at(root, consequence(option(1), state(injured, inst(actor, you)))).
at(root, outcome(option(1), o(get_injured, injured))).
at(root, consequence(option(3), state(injured, inst(actor, you)))).
at(root, outcome(option(3), o(get_injured, injured))).
at(root, consequence(option(4), state(injured, inst(actor, you)))).
at(root, outcome(option(1), o(mood, enraged))).
at(root, outcome(option(4), o(aggressor_state, injured))).
spontaneous(st(root, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
st(root, relation(threatening, inst(actor, monster_20), inst(actor, you))).
unresolved_potential(root, option(1), potential(problem, state(injured, inst(actor, you)))).
unresolved_potential(root, option(3), potential(problem, state(injured, inst(actor, you)))).
unresolved_potential(root, option(4), potential(problem, state(injured, inst(actor, you)))).
at(root, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
spontaneous(st(root, property(type, inst(actor, monster_20), dragon))).
spontaneous(st(root, property(determined, inst(actor, monster_20), true))).
spontaneous(st(root, property(person, inst(actor, monster_20), third))).
st(root, property(type, inst(actor, monster_20), dragon)).
st(root, property(determined, inst(actor, monster_20), true)).
st(root, property(person, inst(actor, monster_20), third)).
at(root, consequence_of(option(4), o(get_loot, loot), relation(has_item, inst(actor, you), inst(item, legends_book_30)))).
spontaneous(st(root, property(gender, inst(actor, monster_20), neuter))).
spontaneous(st(root, property(number, inst(actor, monster_20), singular))).
spontaneous(st(root, property(name, inst(actor, monster_20), "dragon"))).
st(root, property(gender, inst(actor, monster_20), neuter)).
st(root, property(number, inst(actor, monster_20), singular)).
st(root, property(name, inst(actor, monster_20), "dragon")).
spontaneous(st(root, property(has_skill, inst(actor, monster_20), fighting))).
spontaneous(st(root, property(has_skill, inst(actor, monster_20), monstrous))).
spontaneous(st(root, property(has_skill, inst(actor, monster_20), wilderness_lore))).
spontaneous(st(root, property(has_skill, inst(actor, monster_20), unintelligent))).
st(root, property(has_skill, inst(actor, monster_20), fighting)).
st(root, property(has_skill, inst(actor, monster_20), monstrous)).
st(root, property(has_skill, inst(actor, monster_20), wilderness_lore)).
st(root, property(has_skill, inst(actor, monster_20), unintelligent)).
at(root, arg(option(1), aggressive, inst(actor, monster_20))).
at(root, consequence_of(option(4), o(target_state, killed), state(killed, inst(actor, monster_20)))).
at(root, arg(option(4), target, inst(actor, monster_20))).
at(root, consequence_of(option(4), o(target_state, injured), state(injured, inst(actor, monster_20)))).
at(root, consequence_of(option(4), o(target_state, killed), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, consequence_of(option(4), o(target_state, injured), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, consequence_of(option(4), o(success, victory), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, consequence_of(option(1), o(mood, relaxed), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, consequence_of(option(1), o(mood, enraged), relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(3), o(success, escape), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, arg(option(3), from, inst(actor, monster_20))).
at(root, outcome(option(4), o(success, defeat))).
at(root, consequence(option(1), relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(4), o(target_state, killed), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(4), o(target_state, injured), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(4), o(success, victory), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(1), o(mood, relaxed), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(3), o(success, escape), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
unresolved_potential(root, option(1), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
unresolved_potential(root, option(3), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
unresolved_potential(root, option(4), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
spontaneous(st(root, property(type, inst(item, dragon_claws_113), dragon_claws))).
spontaneous(st(root, relation(has_item, inst(actor, monster_20), inst(item, dragon_claws_113)))).
spontaneous(st(root, inst(item, dragon_claws_113))).
st(root, property(type, inst(item, dragon_claws_113), dragon_claws)).
st(root, relation(has_item, inst(actor, monster_20), inst(item, dragon_claws_113))).
st(root, inst(item, dragon_claws_113)).
spontaneous(st(root, property(gender, inst(item, dragon_claws_113), neuter))).
spontaneous(st(root, property(number, inst(item, dragon_claws_113), plural))).
spontaneous(st(root, property(name, inst(item, dragon_claws_113), "claws"))).
spontaneous(st(root, property(determined, inst(item, dragon_claws_113), true))).
spontaneous(st(root, property(person, inst(item, dragon_claws_113), third))).
st(root, property(gender, inst(item, dragon_claws_113), neuter)).
st(root, property(number, inst(item, dragon_claws_113), plural)).
st(root, property(name, inst(item, dragon_claws_113), "claws")).
st(root, property(determined, inst(item, dragon_claws_113), true)).
st(root, property(person, inst(item, dragon_claws_113), third)).
at(root, category(potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))), urgent)).
at(root, category(potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))), immediate)).
st(root, property(has_skill, inst(actor, you), elocution)).
st(root, property(has_skill, inst(actor, you), storytelling)).
at(root, outcome(option(4), o(target_state, unharmed))).
at(root, initiator(option(1), inst(actor, you))).
at(root, initiator(option(3), inst(actor, you))).
at(root, initiator(option(4), inst(actor, you))).
at(root, option(3)).
at(root, option(4)).
at(root, skill_link(fighting, contest, tool, attack, between(aggressor, target), either(o(aggressor_state, unharmed), o(aggressor_state, injured)))).
at(root, skill_link(fighting, contest, tool, attack, between(aggressor, target), either(o(target_state, injured), o(target_state, unharmed)))).
at(root, skill_link(fighting, contest, tool, attack, between(aggressor, target), either(o(success, victory), o(success, defeat)))).
at(root, skill_link(storytelling, promotes, no_tool, tell_story, teller, o(quality, entertaining))).
at(root, skill_link(elocution, promotes, no_tool, talk_down, asking, o(attitude, convinced))).
at(root, skill_link(storytelling, promotes, no_tool, talk_down, asking, o(attitude, convinced))).
at(root, skill_link(elocution, promotes, no_tool, talk_down, asking, o(enraged, not_enraged))).
at(root, skill_link(storytelling, promotes, no_tool, talk_down, asking, o(enraged, not_enraged))).
at(root, skill_link(elocution, promotes, no_tool, pay_off, asking, o(deal, deal))).
at(root, skill_link(elocution, promotes, no_tool, trade, buyer, o(deal, deal))).
at(root, skill_link(music, required, tool, play_song, musician, o(quality, harmonious))).
at(root, skill_link(healing, required, tool, treat_injury, doctor, o(success, healed))).
at(root, skill_link(healing, avoids, no_tool, treat_injury, doctor, o(success, killed))).
at(root, skill_link(elocution, required, no_tool, gossip, interested, o(utility, useful))).
at(root, skill_link(healing, required, tool, buy_healing, doctor, o(success, healed))).
at(root, skill_link(healing, avoids, no_tool, buy_healing, doctor, o(success, killed))).
at(root, skill_link(elocution, promotes, no_tool, buy_healing, buyer, o(deal, deal))).
at(root, skill_link(music, required, tool, pacify, pacifier, o(mood, relaxed))).
at(root, skill_link(music, required, no_tool, pacify, pacifier, o(get_injured, safe))).
at(root, unlikely_outcome(option(1), o(mood, relaxed))).
at(root, unlikely_outcome(option(1), o(get_injured, safe))).
at(root, has_tool_for(inst(actor, you), storytelling)).
at(root, has_tool_for(inst(actor, monster_20), fighting)).
setting(root, wilderness).
at(root, skill_link(wilderness_lore, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(wilderness_lore, contest, no_tool, pursue, between(subject, object), either(o(result, caught_up), o(result, lost)))).
at(root, unlikely_outcome(option(4), o(aggressor_state, unharmed))).
at(root, unlikely_outcome(option(4), o(target_state, injured))).
at(root, unlikely_outcome(option(4), o(success, victory))).
at(root, unlikely_outcome(option(3), o(success, escape))).
at(root, likely_outcome(option(4), o(aggressor_state, injured))).
at(root, likely_outcome(option(4), o(target_state, unharmed))).
at(root, likely_outcome(option(4), o(success, defeat))).
at(root, likely_outcome(option(3), o(success, failure))).
at(root, unlikely_outcome(option(4), o(target_state, killed))).
at(root, unlikely_outcome(option(4), o(get_loot, loot))).
at(root, outcome(option(3), o(success, failure))).
at(root, likely_outcome(option(1), o(get_injured, injured))).
at(root, likely_outcome(option(1), o(mood, enraged))).
at(root, likely_outcome(option(4), o(get_loot, nothing))).
at(root, outcome(option(4), o(get_loot, nothing))).
node_status_reached(root, initialized).
node_status_reached(root, built).
node_status(root, built).
at(root, relevant_to(option(1), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, relevant_to(option(3), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, relevant_to(option(4), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, importance(1, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, importance(2, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, importance(3, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, importance(4, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, some_importance(1)).
at(root, some_importance(2)).
at(root, some_importance(3)).
at(root, some_importance(4)).
at(root, most_important(potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, problematic_for(potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))), inst(actor, you))).
at(root, motivated(option(1))).
at(root, motivated(option(3))).
at(root, motivated(option(4))).
at(root, expectation(option(4), enables, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(1), enables, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), enables, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(4), enables, avoid_threats_to(inst(actor, you)))).
at(root, initiated_by(potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))), inst(actor, monster_20))).
at(root, can_trade(inst(actor, you), inst(item, dragon_scale_23))).
at(root, can_trade(inst(actor, you), inst(item, ring_25))).
at(root, can_trade(inst(actor, you), inst(item, legends_book_30))).
at(root, can_trade(inst(actor, monster_20), inst(item, dragon_claws_113))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), elocution))).
at(root, player_goal(have_tool_for(inst(actor, you), storytelling))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(1), threatens, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), threatens, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), threatens, preserve_health(inst(actor, you)))).
at(root, expectation(option(4), threatens, preserve_health(inst(actor, you)))).
at(root, expectation(option(4), threatens, preserve_health(inst(actor, monster_20)))).
at(root, expectation(option(1), fails, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), fails, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(4), fails, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), fails, preserve_health(inst(actor, you)))).
at(root, expectation(option(4), fails, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(3), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(4), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(4), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(4), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, stakes(preserve_health(inst(actor, you)), high)).
at(root, stakes(preserve_health(inst(actor, monster_20)), high)).
at(root, stakes(avoid_threats_to(inst(actor, you)), high)).
at(root, stakes(high)).
at(root, stakes(as_intended(inst(actor, you)), low)).
at(root, stakes(have_tool_for(inst(actor, you), storytelling), low)).
at(root, option_feel(option(1), longshot)).
at(root, option_feel(option(3), longshot)).
at(root, option_feel(option(4), longshot)).
at(root, choice_structure(negative_alternatives)).
at(root, relevant_skill(option(1), inst(actor, you), missing, music)).
at(root, relevant_skill(option(4), inst(actor, monster_20), has, fighting)).
at(root, relevant_skill(option(3), inst(actor, monster_20), has, wilderness_lore)).
at(root, relevant_skill(option(4), inst(actor, you), missing, fighting)).
at(root, relevant_skill(option(3), inst(actor, you), missing, wilderness_lore)).
at(root, relevant_tool(option(4), inst(actor, monster_20), has, inst(item, dragon_claws_113))).
at(root, is_powerful(inst(actor, monster_20))).
at(root, outcome_perception(option(1), hinders, avoid_threats_to(inst(actor, you)))).
at(root, outcome_perception(option(1), fails, preserve_health(inst(actor, you)))).
at(root, outcome_perception(option(3), fails, preserve_health(inst(actor, you)))).
at(root, outcome_perception(option(4), fails, preserve_health(inst(actor, you)))).
at(root, choice_structure(uncomfortable)).
at(root, choice_structure(bleak)).
at(root, outcome_perception(option(1), fails, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), fails, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(4), fails, as_intended(inst(actor, you)))).
successor(root, option(3), root_3).
story_node(root_3).
st(root_3, state(injured, inst(actor, you))).
successor(root, option(1), root_3).
successor(root, option(4), root_3).
st(root_3, relation(threatening, inst(actor, monster_20), inst(actor, you))).
st(root_3, property(type, inst(actor, you), person)).
st(root_3, property(has_skill, inst(actor, you), literacy)).
st(root_3, inst(actor, you)).
st(root_3, state(party_member, inst(actor, you))).
st(root_3, property(name, inst(actor, you), "Dunyazad")).
st(root_3, property(person, inst(actor, you), second)).
st(root_3, property(gender, inst(actor, you), feminine)).
st(root_3, property(determined, inst(actor, you), false)).
st(root_3, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root_3, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_3, relation(has_item, inst(actor, you), inst(item, legends_book_30))).
st(root_3, property(number, inst(item, dragon_scale_23), singular)).
st(root_3, property(number, inst(item, ring_25), singular)).
st(root_3, property(number, inst(item, legends_book_30), singular)).
st(root_3, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root_3, property(name, inst(item, ring_25), "ring")).
st(root_3, property(name, inst(item, legends_book_30), "book of legends")).
st(root_3, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root_3, property(type, inst(item, ring_25), ring)).
st(root_3, property(type, inst(item, legends_book_30), legends_book)).
st(root_3, inst(item, dragon_scale_23)).
st(root_3, inst(item, ring_25)).
st(root_3, inst(item, legends_book_30)).
st(root_3, inst(actor, monster_20)).
st(root_3, property(type, inst(actor, monster_20), dragon)).
st(root_3, property(determined, inst(actor, monster_20), true)).
st(root_3, property(person, inst(actor, monster_20), third)).
st(root_3, property(gender, inst(actor, monster_20), neuter)).
st(root_3, property(number, inst(actor, monster_20), singular)).
st(root_3, property(name, inst(actor, monster_20), "dragon")).
st(root_3, property(has_skill, inst(actor, monster_20), fighting)).
st(root_3, property(has_skill, inst(actor, monster_20), monstrous)).
st(root_3, property(has_skill, inst(actor, monster_20), wilderness_lore)).
st(root_3, property(has_skill, inst(actor, monster_20), unintelligent)).
st(root_3, property(type, inst(item, dragon_claws_113), dragon_claws)).
st(root_3, relation(has_item, inst(actor, monster_20), inst(item, dragon_claws_113))).
st(root_3, inst(item, dragon_claws_113)).
st(root_3, property(gender, inst(item, dragon_claws_113), neuter)).
st(root_3, property(number, inst(item, dragon_claws_113), plural)).
st(root_3, property(name, inst(item, dragon_claws_113), "claws")).
st(root_3, property(determined, inst(item, dragon_claws_113), true)).
st(root_3, property(person, inst(item, dragon_claws_113), third)).
st(root_3, property(has_skill, inst(actor, you), elocution)).
st(root_3, property(has_skill, inst(actor, you), storytelling)).
node_status_reached(root_3, uninitialized).
node_status_reached(root, branched).
node_status_reached(root, polished).
node_status(root_3, uninitialized).
node_status(root, polished).
path_length(root_3, 1).
at(root_3, is_powerful(inst(actor, monster_20))).
at(root_3, text_mode(example)).
max_unique(113).
