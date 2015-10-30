seed(25173).
surface_property(name, inst(actor, you), "Dunyazad").
surface_property(person, inst(actor, you), second).
surface_property(gender, inst(actor, you), feminine).
surface_property(number, inst(actor, you), singular).
surface_property(determined, inst(actor, you), false).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
at(root, perceived_stakes(none)).
setup_priority(on_sale, 1).
setup_priority(tavern, 4).
setup_priority(threatened_innocents, 2).
setup_priority(market, 3).
setup_priority(monster_attack, 5).
setup_priority(healer, 6).
action_priority(attack, 19).
action_priority(shift_blame, 10).
action_priority(steal, 14).
action_priority(flee, 8).
action_priority(tell_story, 9).
action_priority(talk_down, 5).
action_priority(pay_off, 15).
action_priority(accuse, 1).
action_priority(deny_blame, 20).
action_priority(trade, 6).
action_priority(reach_destination, 17).
action_priority(play_song, 18).
action_priority(treat_injury, 4).
action_priority(dispel, 7).
action_priority(polymorph, 2).
action_priority(gossip, 16).
action_priority(explain_innocence, 11).
action_priority(buy_healing, 13).
action_priority(pacify, 12).
action_priority(travel_onwards, 3).
at(root, text_mode(example)).
st(root, property(type, inst(actor, you), person)).
st(root, property(has_skill, inst(actor, you), literacy)).
st(root, inst(actor, you)).
st(root, state(party_member, inst(actor, you))).
at(root, option(1)).
vignette(root, root).
setup(root, threatened_innocents).
at(root, setup_arg(goods, inst(item, item_10))).
at(root, setup_arg(attacker, inst(actor, tough_11))).
at(root, setup_arg(victim, inst(actor, businessperson_12))).
at(root, setup_arg(goods, inst(item, treasure_13))).
st(root, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root, property(type, inst(item, perfume_24), perfume)).
st(root, property(type, inst(item, ring_25), ring)).
st(root, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root, inst(item, perfume_24)).
st(root, inst(item, ring_25)).
st(root, inst(item, ancient_grimoire_27)).
spontaneous(st(root, inst(item, item_10))).
spontaneous(st(root, inst(actor, tough_11))).
spontaneous(st(root, inst(actor, businessperson_12))).
spontaneous(st(root, inst(item, treasure_13))).
st(root, inst(item, item_10)).
st(root, inst(actor, tough_11)).
st(root, inst(actor, businessperson_12)).
st(root, inst(item, treasure_13)).
st(root, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root, property(type, inst(item, oboe_35), oboe)).
st(root, inst(item, oboe_35)).
at(root, action(option(1), polymorph)).
at(root, outcome(option(1), o(success, cursed))).
spontaneous(st(root, relation(has_item, inst(actor, businessperson_12), inst(item, item_10)))).
spontaneous(st(root, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
spontaneous(st(root, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13)))).
st(root, relation(has_item, inst(actor, businessperson_12), inst(item, item_10))).
st(root, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))).
st(root, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
at(root, consequence(option(2), _not, relation(has_item, inst(actor, you), inst(item, perfume_24)))).
at(root, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
spontaneous(st(root, property(type, inst(item, item_10), sack_of_gold))).
spontaneous(st(root, property(type, inst(actor, tough_11), bandits))).
spontaneous(st(root, property(type, inst(actor, businessperson_12), merchant))).
spontaneous(st(root, property(type, inst(item, treasure_13), perfume))).
st(root, property(type, inst(item, item_10), sack_of_gold)).
st(root, property(type, inst(actor, tough_11), bandits)).
st(root, property(type, inst(actor, businessperson_12), merchant)).
st(root, property(type, inst(item, treasure_13), perfume)).
at(root, arg(option(2), price, inst(item, perfume_24))).
at(root, action(option(2), pay_off)).
at(root, outcome(option(2), o(deal, deal))).
at(root, consequence(option(1), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(2), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(3), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(1), nullifies, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence_of(option(2), o(deal, deal), _not, relation(has_item, inst(actor, you), inst(item, perfume_24)))).
spontaneous(st(root, property(has_skill, inst(actor, tough_11), fighting))).
spontaneous(st(root, property(has_skill, inst(actor, businessperson_12), literacy))).
st(root, property(has_skill, inst(actor, tough_11), fighting)).
st(root, property(has_skill, inst(actor, businessperson_12), literacy)).
at(root, arg(option(1), target, inst(actor, tough_11))).
at(root, arg(option(2), victim, inst(actor, businessperson_12))).
at(root, arg(option(3), victim, inst(actor, businessperson_12))).
at(root, consequence_of(option(1), o(success, cursed), property(polymorphed, inst(actor, tough_11), bandits))).
at(root, consequence_of(option(2), o(deal, deal), relation(has_item, inst(actor, tough_11), inst(item, perfume_24)))).
at(root, consequence_of(option(2), o(deal, deal), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, arg(option(2), listening, inst(actor, tough_11))).
at(root, arg(option(3), listening, inst(actor, tough_11))).
at(root, action(option(3), talk_down)).
at(root, consequence_of(option(3), o(attitude, convinced), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, arg(option(2), asking, inst(actor, you))).
at(root, arg(option(3), asking, inst(actor, you))).
at(root, consequence_of(option(3), o(enraged, enraged), relation(threatening, inst(actor, tough_11), inst(actor, you)))).
at(root, consequence(option(1), property(polymorphed, inst(actor, tough_11), bandits))).
at(root, consequence(option(2), relation(has_item, inst(actor, tough_11), inst(item, perfume_24)))).
at(root, consequence(option(2), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence(option(3), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, outcome(option(3), o(attitude, convinced))).
at(root, consequence_of(option(2), o(deal, deal), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence_of(option(3), o(attitude, convinced), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence_of(option(1), o(success, cursed), nullifies, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence_of(option(1), o(success, cursed), property(has_skill, inst(actor, tough_11), unintelligent))).
unresolved_potential(root, option(1), potential(problem, property(polymorphed, inst(actor, tough_11), bandits))).
at(root, consequence_of(option(1), o(success, cursed), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(1), property(has_skill, inst(actor, tough_11), unintelligent))).
spontaneous(st(root, property(type, inst(item, spear_56), spear))).
spontaneous(st(root, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56)))).
spontaneous(st(root, inst(item, spear_56))).
st(root, property(type, inst(item, spear_56), spear)).
st(root, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56))).
st(root, inst(item, spear_56)).
at(root, category(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), urgent)).
at(root, category(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), immediate)).
at(root, arg(option(1), caster, inst(actor, you))).
surface_property(name, inst(item, oboe_35), "oboe").
surface_property(name, inst(item, perfume_24), "perfume").
surface_property(name, inst(item, ancient_grimoire_27), "ancient grimoire").
surface_property(name, inst(item, ring_25), "ring").
surface_property(name, inst(actor, businessperson_12), "merchant").
surface_property(name, inst(actor, tough_11), "bandits").
st(root, property(has_skill, inst(actor, you), music)).
st(root, property(has_skill, inst(actor, you), elocution)).
st(root, property(has_skill, inst(actor, you), sorcery)).
at(root, initiator(option(1), inst(actor, you))).
at(root, initiator(option(2), inst(actor, you))).
at(root, initiator(option(3), inst(actor, you))).
at(root, option(2)).
at(root, option(3)).
node_status_reached(root, initialized).
node_status_reached(root, built).
node_status(root, built).
at(root, skill_link(fighting, contest, tool, attack, between(aggressor, target), either(o(aggressor_state, unharmed), o(aggressor_state, injured)))).
at(root, skill_link(fighting, contest, tool, attack, between(aggressor, target), either(o(target_state, injured), o(target_state, unharmed)))).
at(root, skill_link(fighting, contest, tool, attack, between(aggressor, target), either(o(success, victory), o(success, defeat)))).
at(root, skill_link(elocution, contest, no_tool, shift_blame, between(shifter, victim), either(o(success, shifted), o(success, suspected)))).
at(root, skill_link(thievery, contest, no_tool, steal, between(thief, victim), either(o(get_item, get), o(get_item, dont_get)))).
at(root, skill_link(thievery, contest, no_tool, steal, between(thief, victim), either(o(get_caught, safe), o(get_caught, caught)))).
at(root, skill_link(storytelling, promotes, no_tool, tell_story, teller, o(quality, entertaining))).
at(root, skill_link(elocution, promotes, no_tool, talk_down, asking, o(attitude, convinced))).
at(root, skill_link(storytelling, promotes, no_tool, talk_down, asking, o(attitude, convinced))).
at(root, skill_link(elocution, promotes, no_tool, talk_down, asking, o(enraged, not_enraged))).
at(root, skill_link(storytelling, promotes, no_tool, talk_down, asking, o(enraged, not_enraged))).
at(root, skill_link(elocution, promotes, no_tool, pay_off, asking, o(deal, deal))).
at(root, skill_link(elocution, contest, no_tool, accuse, between(accuser, target), either(o(success, accused), o(success, ignored)))).
at(root, skill_link(elocution, contest, no_tool, deny_blame, between(denier, accuser), either(o(success, exonerated), o(success, ignored)))).
at(root, skill_link(elocution, promotes, no_tool, trade, buyer, o(deal, deal))).
at(root, skill_link(music, required, tool, play_song, musician, o(quality, harmonious))).
at(root, skill_link(healing, required, tool, treat_injury, doctor, o(success, healed))).
at(root, skill_link(healing, avoids, no_tool, treat_injury, doctor, o(success, killed))).
at(root, skill_link(sorcery, contest, tool, dispel, between(caster, target), either(o(success, cursed), o(success, no_effect)))).
at(root, skill_link(sorcery, required, tool, polymorph, caster, o(success, cursed))).
at(root, skill_link(sorcery, promotes, tool, polymorph, caster, o(success, cursed))).
at(root, skill_link(elocution, required, no_tool, gossip, interested, o(utility, useful))).
at(root, skill_link(storytelling, promotes, no_tool, explain_innocence, explainer, o(success, exonerated))).
at(root, skill_link(healing, required, tool, buy_healing, doctor, o(success, healed))).
at(root, skill_link(healing, avoids, no_tool, buy_healing, doctor, o(success, killed))).
at(root, skill_link(elocution, promotes, no_tool, buy_healing, buyer, o(deal, deal))).
at(root, skill_link(music, required, tool, pacify, pacifier, o(mood, relaxed))).
at(root, skill_link(music, promotes, tool, pacify, pacifier, o(mood, relaxed))).
at(root, skill_link(music, required, no_tool, pacify, pacifier, o(get_injured, safe))).
at(root, likely_outcome(option(3), o(attitude, convinced))).
at(root, likely_outcome(option(3), o(enraged, not_enraged))).
at(root, likely_outcome(option(2), o(deal, deal))).
at(root, has_tool_for(inst(actor, you), music)).
at(root, has_tool_for(inst(actor, you), sorcery)).
at(root, has_tool_for(inst(actor, businessperson_12), fighting)).
at(root, likely_outcome(option(1), o(success, cursed))).
setting(root, town).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(get_injured, safe), o(get_injured, injured)))).
at(root, unlikely_outcome(option(3), o(enraged, enraged))).
at(root, outcome_predictability(option(3), o(attitude, convinced), predictable)).
at(root, outcome_predictability(option(3), o(enraged, not_enraged), predictable)).
at(root, outcome(option(3), o(enraged, not_enraged))).
at(root, outcome_predictability(option(2), o(deal, deal), predictable)).
at(root, outcome_predictability(option(1), o(success, cursed), predictable)).
at(root, neutral_outcome(option(3), o(attitude, unconvinced))).
at(root, neutral_outcome(option(2), o(deal, no_deal))).
at(root, neutral_outcome(option(1), o(success, no_effect))).
at(root, possible_outcome_perception(option(3), o(enraged, enraged), hinders, avoid_threats_to(inst(actor, you)))).
at(root, possible_outcome_perception(option(2), o(deal, deal), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, possible_outcome_perception(option(3), o(attitude, convinced), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, possible_outcome_perception(option(1), o(success, cursed), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, possible_outcome_perception(option(1), o(success, cursed), fails, preserve_original_form(inst(actor, tough_11)))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(avoid_accusations(inst(actor, you)))).
at(root, player_goal(preserve_original_form(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), music))).
at(root, player_goal(have_tool_for(inst(actor, you), elocution))).
at(root, player_goal(have_tool_for(inst(actor, you), sorcery))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, player_goal(avoid_threats_to(inst(actor, businessperson_12)))).
at(root, player_goal(avoid_accusations(inst(actor, businessperson_12)))).
at(root, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root, goal_stakes(preserve_health(inst(actor, tough_11)), high)).
at(root, goal_stakes(preserve_health(inst(actor, businessperson_12)), high)).
at(root, goal_stakes(as_intended(inst(actor, you)), low)).
at(root, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, tough_11)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, businessperson_12)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, tough_11)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, businessperson_12)), high)).
at(root, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), elocution), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, tough_11), fighting), low)).
at(root, goal_stakes(have_tool_for(inst(actor, businessperson_12), literacy), low)).
at(root, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, tough_11)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, businessperson_12)), high)).
at(root, outcome_perception(option(1), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, outcome_perception(option(2), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, outcome_perception(option(3), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, outcome_perception(option(1), fails, preserve_original_form(inst(actor, tough_11)))).
at(root, outcome_perception(option(1), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(2), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), achieves, as_intended(inst(actor, you)))).
at(root, outcome_stakes(option(1), high)).
at(root, outcome_stakes(option(2), high)).
at(root, outcome_stakes(option(3), high)).
at(root, important_outcome(option(2), o(deal, deal))).
at(root, important_outcome(option(3), o(attitude, convinced))).
at(root, important_outcome(option(1), o(success, cursed))).
at(root, overall_predictability(option(1), predictable)).
at(root, overall_predictability(option(2), predictable)).
at(root, overall_predictability(option(3), predictable)).
at(root, relevant_to(option(1), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, relevant_to(option(2), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, relevant_to(option(3), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, importance(1, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, importance(2, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, importance(3, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, importance(4, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, some_importance(1)).
at(root, some_importance(2)).
at(root, some_importance(3)).
at(root, some_importance(4)).
at(root, most_important(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, problematic_for(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), inst(actor, businessperson_12))).
at(root, motivated(option(1))).
at(root, motivated(option(2))).
at(root, motivated(option(3))).
at(root, expectation(option(1), enables, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(2), enables, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(3), enables, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, initiated_by(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), inst(actor, tough_11))).
surface_property(number, inst(item, oboe_35), singular).
surface_property(number, inst(item, perfume_24), plural).
surface_property(number, inst(item, ancient_grimoire_27), singular).
surface_property(number, inst(item, ring_25), singular).
at(root, can_trade(inst(actor, you), inst(item, perfume_24))).
at(root, can_trade(inst(actor, you), inst(item, ring_25))).
at(root, can_trade(inst(actor, you), inst(item, ancient_grimoire_27))).
at(root, can_trade(inst(actor, you), inst(item, oboe_35))).
at(root, can_trade(inst(actor, businessperson_12), inst(item, item_10))).
at(root, can_trade(inst(actor, businessperson_12), inst(item, treasure_13))).
at(root, can_trade(inst(actor, businessperson_12), inst(item, spear_56))).
at(root, expectation(option(3), threatens, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), threatens, preserve_original_form(inst(actor, tough_11)))).
at(root, expectation(option(1), fails, preserve_original_form(inst(actor, tough_11)))).
at(root, expectation(option(1), achieves, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(2), achieves, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(3), achieves, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(1), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(2), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(3), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(1), irrelevant, avoid_accusations(inst(actor, businessperson_12)))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, avoid_accusations(inst(actor, businessperson_12)))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, avoid_accusations(inst(actor, businessperson_12)))).
at(root, perceived_option_stakes(option(1), high)).
at(root, perceived_option_stakes(option(2), high)).
at(root, perceived_option_stakes(option(3), high)).
at(root, perceived_stakes(high)).
at(root, option_feel(option(1), safe)).
at(root, option_feel(option(2), safe)).
at(root, option_feel(option(3), hopeful)).
at(root, option_structure(positive_alternatives)).
at(root, option_feel(option(1), sure_thing)).
at(root, option_feel(option(2), sure_thing)).
at(root, outcome_overall(option(1), good)).
at(root, outcome_overall(option(2), good)).
at(root, outcome_overall(option(3), good)).
at(root, outcome_feel(option(1), expected_success)).
at(root, outcome_feel(option(2), expected_success)).
at(root, outcome_feel(option(3), expected_success)).
at(root, relevant_skill(option(2), inst(actor, you), has, elocution)).
at(root, relevant_skill(option(3), inst(actor, you), has, elocution)).
at(root, relevant_skill(option(1), inst(actor, you), has, sorcery)).
at(root, relevant_tool(option(1), inst(actor, you), has, inst(item, ancient_grimoire_27))).
at(root, is_powerful(inst(actor, tough_11))).
at(root, is_powerless(inst(actor, businessperson_12))).
at(root, guilty(inst(actor, tough_11))).
at(root, actual_stakes(high)).
resolves_vignette(root, option(2)).
resolves_vignette(root, option(3)).
surface_property(name, inst(item, item_10), "sack of gold").
surface_property(name, inst(item, treasure_13), "perfume").
surface_property(name, inst(item, spear_56), "spear").
surface_property(person, inst(item, perfume_24), third).
surface_property(person, inst(item, ring_25), third).
surface_property(person, inst(item, ancient_grimoire_27), third).
surface_property(person, inst(item, item_10), third).
surface_property(person, inst(actor, tough_11), third).
surface_property(person, inst(actor, businessperson_12), third).
surface_property(person, inst(item, treasure_13), third).
surface_property(person, inst(item, oboe_35), third).
surface_property(person, inst(item, spear_56), third).
surface_property(number, inst(item, item_10), singular).
surface_property(number, inst(actor, tough_11), plural).
surface_property(number, inst(actor, businessperson_12), singular).
surface_property(number, inst(item, treasure_13), plural).
surface_property(number, inst(item, spear_56), singular).
surface_property(gender, inst(item, perfume_24), neuter).
surface_property(gender, inst(item, ring_25), neuter).
surface_property(gender, inst(item, ancient_grimoire_27), neuter).
surface_property(gender, inst(item, item_10), neuter).
surface_property(gender, inst(actor, tough_11), neuter).
surface_property(gender, inst(actor, businessperson_12), feminine).
surface_property(gender, inst(item, treasure_13), neuter).
surface_property(gender, inst(item, oboe_35), neuter).
surface_property(gender, inst(item, spear_56), neuter).
surface_property(determined, inst(item, perfume_24), true).
surface_property(determined, inst(item, ring_25), true).
surface_property(determined, inst(item, ancient_grimoire_27), true).
surface_property(determined, inst(item, item_10), true).
surface_property(determined, inst(actor, tough_11), true).
surface_property(determined, inst(actor, businessperson_12), true).
surface_property(determined, inst(item, treasure_13), true).
surface_property(determined, inst(item, oboe_35), true).
surface_property(determined, inst(item, spear_56), true).
successor(root, option(1), root_1).
successor(root, option(2), root_2).
successor(root, option(3), root_3).
story_node(root_1).
story_node(root_2).
story_node(root_3).
st(root_1, property(polymorphed, inst(actor, tough_11), bandits)).
st(root_2, relation(has_item, inst(actor, tough_11), inst(item, perfume_24))).
st(root_1, property(has_skill, inst(actor, tough_11), unintelligent)).
st(root_1, property(type, inst(actor, you), person)).
st(root_2, property(type, inst(actor, you), person)).
st(root_3, property(type, inst(actor, you), person)).
st(root_1, property(has_skill, inst(actor, you), literacy)).
st(root_2, property(has_skill, inst(actor, you), literacy)).
st(root_3, property(has_skill, inst(actor, you), literacy)).
st(root_1, inst(actor, you)).
st(root_2, inst(actor, you)).
st(root_3, inst(actor, you)).
st(root_1, state(party_member, inst(actor, you))).
st(root_2, state(party_member, inst(actor, you))).
st(root_3, state(party_member, inst(actor, you))).
st(root_1, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_3, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_1, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_2, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_3, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_1, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root_2, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root_3, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root_1, property(type, inst(item, perfume_24), perfume)).
st(root_2, property(type, inst(item, perfume_24), perfume)).
st(root_3, property(type, inst(item, perfume_24), perfume)).
st(root_1, property(type, inst(item, ring_25), ring)).
st(root_2, property(type, inst(item, ring_25), ring)).
st(root_3, property(type, inst(item, ring_25), ring)).
st(root_1, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root_2, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root_3, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root_1, inst(item, perfume_24)).
st(root_2, inst(item, perfume_24)).
st(root_3, inst(item, perfume_24)).
st(root_1, inst(item, ring_25)).
st(root_2, inst(item, ring_25)).
st(root_3, inst(item, ring_25)).
st(root_1, inst(item, ancient_grimoire_27)).
st(root_2, inst(item, ancient_grimoire_27)).
st(root_3, inst(item, ancient_grimoire_27)).
st(root_1, inst(item, item_10)).
st(root_2, inst(item, item_10)).
st(root_3, inst(item, item_10)).
st(root_1, inst(actor, tough_11)).
st(root_2, inst(actor, tough_11)).
st(root_3, inst(actor, tough_11)).
st(root_1, inst(actor, businessperson_12)).
st(root_2, inst(actor, businessperson_12)).
st(root_3, inst(actor, businessperson_12)).
st(root_1, inst(item, treasure_13)).
st(root_2, inst(item, treasure_13)).
st(root_3, inst(item, treasure_13)).
st(root_1, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_2, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_3, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_1, property(type, inst(item, oboe_35), oboe)).
st(root_2, property(type, inst(item, oboe_35), oboe)).
st(root_3, property(type, inst(item, oboe_35), oboe)).
st(root_1, inst(item, oboe_35)).
st(root_2, inst(item, oboe_35)).
st(root_3, inst(item, oboe_35)).
st(root_1, relation(has_item, inst(actor, businessperson_12), inst(item, item_10))).
st(root_2, relation(has_item, inst(actor, businessperson_12), inst(item, item_10))).
st(root_3, relation(has_item, inst(actor, businessperson_12), inst(item, item_10))).
st(root_1, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
st(root_2, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
st(root_3, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
st(root_1, property(type, inst(item, item_10), sack_of_gold)).
st(root_2, property(type, inst(item, item_10), sack_of_gold)).
st(root_3, property(type, inst(item, item_10), sack_of_gold)).
st(root_1, property(type, inst(actor, tough_11), bandits)).
st(root_2, property(type, inst(actor, tough_11), bandits)).
st(root_3, property(type, inst(actor, tough_11), bandits)).
st(root_1, property(type, inst(actor, businessperson_12), merchant)).
st(root_2, property(type, inst(actor, businessperson_12), merchant)).
st(root_3, property(type, inst(actor, businessperson_12), merchant)).
st(root_1, property(type, inst(item, treasure_13), perfume)).
st(root_2, property(type, inst(item, treasure_13), perfume)).
st(root_3, property(type, inst(item, treasure_13), perfume)).
st(root_1, property(has_skill, inst(actor, tough_11), fighting)).
st(root_2, property(has_skill, inst(actor, tough_11), fighting)).
st(root_3, property(has_skill, inst(actor, tough_11), fighting)).
st(root_1, property(has_skill, inst(actor, businessperson_12), literacy)).
st(root_2, property(has_skill, inst(actor, businessperson_12), literacy)).
st(root_3, property(has_skill, inst(actor, businessperson_12), literacy)).
st(root_1, property(type, inst(item, spear_56), spear)).
st(root_2, property(type, inst(item, spear_56), spear)).
st(root_3, property(type, inst(item, spear_56), spear)).
st(root_1, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56))).
st(root_2, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56))).
st(root_3, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56))).
st(root_1, inst(item, spear_56)).
st(root_2, inst(item, spear_56)).
st(root_3, inst(item, spear_56)).
st(root_1, property(has_skill, inst(actor, you), music)).
st(root_2, property(has_skill, inst(actor, you), music)).
st(root_3, property(has_skill, inst(actor, you), music)).
st(root_1, property(has_skill, inst(actor, you), elocution)).
st(root_2, property(has_skill, inst(actor, you), elocution)).
st(root_3, property(has_skill, inst(actor, you), elocution)).
st(root_1, property(has_skill, inst(actor, you), sorcery)).
st(root_2, property(has_skill, inst(actor, you), sorcery)).
st(root_3, property(has_skill, inst(actor, you), sorcery)).
node_status_reached(root_1, uninitialized).
node_status_reached(root_2, uninitialized).
node_status_reached(root_3, uninitialized).
node_status_reached(root, branched).
node_status_reached(root, polished).
node_status(root_1, uninitialized).
node_status(root_2, uninitialized).
node_status(root_3, uninitialized).
node_status(root, polished).
at(root_1, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_1, goal_stakes(preserve_health(inst(actor, tough_11)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, tough_11)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, tough_11)), high)).
at(root_1, goal_stakes(preserve_health(inst(actor, businessperson_12)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, businessperson_12)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, businessperson_12)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, tough_11)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, tough_11)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, tough_11)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, businessperson_12)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, businessperson_12)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, businessperson_12)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, tough_11)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, tough_11)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, tough_11)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, businessperson_12)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, businessperson_12)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, businessperson_12)), high)).
at(root_1, goal_stakes(have_tool_for(inst(actor, tough_11), unintelligent), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, tough_11), fighting), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, tough_11), fighting), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, tough_11), fighting), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, businessperson_12), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, businessperson_12), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, businessperson_12), literacy), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), elocution), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), elocution), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), elocution), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, tough_11)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, tough_11)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, tough_11)), high)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, businessperson_12)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, businessperson_12)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, businessperson_12)), high)).
path_length(root_1, 1).
path_length(root_2, 1).
path_length(root_3, 1).
at(root_1, perceived_stakes(none)).
at(root_2, perceived_stakes(none)).
at(root_3, perceived_stakes(none)).
at(root_1, is_powerful(inst(actor, tough_11))).
at(root_2, is_powerful(inst(actor, tough_11))).
at(root_3, is_powerful(inst(actor, tough_11))).
at(root_1, is_powerless(inst(actor, businessperson_12))).
at(root_2, is_powerless(inst(actor, businessperson_12))).
at(root_3, is_powerless(inst(actor, businessperson_12))).
at(root_1, text_mode(example)).
at(root_2, text_mode(example)).
at(root_3, text_mode(example)).
max_unique(56).
