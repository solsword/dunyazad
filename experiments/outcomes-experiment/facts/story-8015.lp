surface_property(name, inst(actor, you), "Dunyazad").
surface_property(person, inst(actor, you), second).
surface_property(gender, inst(actor, you), feminine).
surface_property(number, inst(actor, you), singular).
surface_property(determined, inst(actor, you), false).
seed(8015).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
at(root, perceived_stakes(none)).
setup_priority(on_sale, 5).
setup_priority(tavern, 3).
setup_priority(threatened_innocents, 4).
setup_priority(market, 1).
setup_priority(monster_attack, 6).
setup_priority(healer, 2).
action_priority(attack, 7).
action_priority(shift_blame, 6).
action_priority(steal, 11).
action_priority(flee, 5).
action_priority(tell_story, 12).
action_priority(talk_down, 16).
action_priority(pay_off, 1).
action_priority(accuse, 18).
action_priority(deny_blame, 2).
action_priority(trade, 15).
action_priority(reach_destination, 8).
action_priority(play_song, 4).
action_priority(treat_injury, 14).
action_priority(dispel, 9).
action_priority(polymorph, 17).
action_priority(gossip, 20).
action_priority(explain_innocence, 19).
action_priority(buy_healing, 13).
action_priority(pacify, 3).
action_priority(travel_onwards, 10).
at(root, text_mode(example)).
st(root, property(type, inst(actor, you), person)).
st(root, property(has_skill, inst(actor, you), literacy)).
st(root, inst(actor, you)).
st(root, state(party_member, inst(actor, you))).
at(root, option(1)).
vignette(root, root).
setup(root, threatened_innocents).
at(root, setup_arg(attacker, inst(actor, tough_11))).
at(root, setup_arg(victim, inst(actor, businessperson_12))).
at(root, setup_arg(goods, inst(item, treasure_13))).
st(root, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root, relation(has_item, inst(actor, you), inst(item, medicine_book_28))).
st(root, property(type, inst(item, perfume_24), perfume)).
st(root, property(type, inst(item, ring_25), ring)).
st(root, property(type, inst(item, medicine_book_28), medicine_book)).
st(root, inst(item, perfume_24)).
st(root, inst(item, ring_25)).
st(root, inst(item, medicine_book_28)).
spontaneous(st(root, inst(actor, tough_11))).
spontaneous(st(root, inst(actor, businessperson_12))).
spontaneous(st(root, inst(item, treasure_13))).
st(root, inst(actor, tough_11)).
st(root, inst(actor, businessperson_12)).
st(root, inst(item, treasure_13)).
st(root, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root, relation(has_item, inst(actor, you), inst(item, medicine_chest_36))).
st(root, property(type, inst(item, oboe_35), oboe)).
st(root, property(type, inst(item, medicine_chest_36), medicine_chest)).
st(root, inst(item, oboe_35)).
st(root, inst(item, medicine_chest_36)).
at(root, action(option(3), polymorph)).
spontaneous(st(root, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
spontaneous(st(root, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13)))).
st(root, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))).
st(root, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
at(root, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
spontaneous(st(root, property(type, inst(actor, tough_11), bandits))).
spontaneous(st(root, property(type, inst(actor, businessperson_12), merchant))).
spontaneous(st(root, property(type, inst(item, treasure_13), perfume))).
st(root, property(type, inst(actor, tough_11), bandits)).
st(root, property(type, inst(actor, businessperson_12), merchant)).
st(root, property(type, inst(item, treasure_13), perfume)).
at(root, consequence_of(option(1), o(onwards, onwards), _not, property(type, inst(actor, tough_11), bandits))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, property(type, inst(actor, businessperson_12), merchant))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, property(type, inst(item, treasure_13), perfume))).
at(root, action(option(1), travel_onwards)).
at(root, consequence_of(option(1), o(onwards, onwards), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13)))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, inst(actor, tough_11))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, inst(actor, businessperson_12))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, inst(item, treasure_13))).
at(root, outcome(option(1), o(onwards, onwards))).
at(root, consequence(option(1), _not, property(type, inst(actor, tough_11), bandits))).
at(root, consequence(option(1), _not, property(type, inst(actor, businessperson_12), merchant))).
at(root, consequence(option(1), _not, property(type, inst(item, treasure_13), perfume))).
at(root, consequence(option(1), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(1), _not, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13)))).
at(root, consequence(option(1), _not, inst(actor, tough_11))).
at(root, consequence(option(1), _not, inst(actor, businessperson_12))).
at(root, consequence(option(1), _not, inst(item, treasure_13))).
at(root, consequence(option(1), nullifies, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence_of(option(1), o(onwards, onwards), nullifies, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
spontaneous(st(root, property(has_skill, inst(actor, tough_11), fighting))).
spontaneous(st(root, property(has_skill, inst(actor, businessperson_12), literacy))).
st(root, property(has_skill, inst(actor, tough_11), fighting)).
st(root, property(has_skill, inst(actor, businessperson_12), literacy)).
at(root, consequence_of(option(1), o(onwards, onwards), _not, property(has_skill, inst(actor, tough_11), fighting))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, property(has_skill, inst(actor, businessperson_12), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, businessperson_12), tinkering))).
st(root, property(has_skill, inst(actor, businessperson_12), tinkering)).
at(root, arg(option(3), target, inst(actor, tough_11))).
at(root, arg(option(2), victim, inst(actor, businessperson_12))).
at(root, consequence_of(option(3), o(success, cursed), property(polymorphed, inst(actor, tough_11), bandits))).
at(root, arg(option(2), listening, inst(actor, tough_11))).
at(root, action(option(2), talk_down)).
at(root, consequence_of(option(2), o(attitude, convinced), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, arg(option(2), asking, inst(actor, you))).
at(root, consequence_of(option(2), o(enraged, enraged), relation(threatening, inst(actor, tough_11), inst(actor, you)))).
at(root, consequence(option(2), relation(threatening, inst(actor, tough_11), inst(actor, you)))).
at(root, outcome(option(2), o(enraged, enraged))).
at(root, consequence(option(1), _not, property(has_skill, inst(actor, tough_11), fighting))).
at(root, consequence(option(1), _not, property(has_skill, inst(actor, businessperson_12), literacy))).
at(root, consequence_of(option(2), o(attitude, convinced), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, property(has_skill, inst(actor, businessperson_12), tinkering))).
at(root, consequence_of(option(3), o(success, cursed), nullifies, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence_of(option(3), o(success, cursed), property(has_skill, inst(actor, tough_11), unintelligent))).
unresolved_potential(root, option(2), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
unresolved_potential(root, option(3), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
unresolved_potential(root, option(2), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, you)))).
at(root, consequence_of(option(3), o(success, cursed), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(1), _not, property(has_skill, inst(actor, businessperson_12), tinkering))).
spontaneous(st(root, property(type, inst(item, spear_56), spear))).
spontaneous(st(root, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56)))).
spontaneous(st(root, inst(item, spear_56))).
st(root, property(type, inst(item, spear_56), spear)).
st(root, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56))).
st(root, inst(item, spear_56)).
at(root, consequence_of(option(1), o(onwards, onwards), _not, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56)))).
at(root, consequence(option(1), _not, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56)))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, property(type, inst(item, spear_56), spear))).
at(root, consequence_of(option(1), o(onwards, onwards), _not, inst(item, spear_56))).
at(root, consequence(option(1), _not, property(type, inst(item, spear_56), spear))).
at(root, consequence(option(1), _not, inst(item, spear_56))).
at(root, category(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), urgent)).
at(root, category(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), immediate)).
at(root, arg(option(1), subject, inst(actor, you))).
at(root, arg(option(3), caster, inst(actor, you))).
surface_property(name, inst(item, oboe_35), "oboe").
surface_property(name, inst(item, perfume_24), "perfume").
surface_property(name, inst(item, medicine_chest_36), "medicine chest").
surface_property(name, inst(item, medicine_book_28), "book of medicine").
surface_property(name, inst(item, ring_25), "ring").
surface_property(name, inst(actor, businessperson_12), "merchant").
surface_property(name, inst(actor, tough_11), "bandits").
st(root, property(has_skill, inst(actor, you), wilderness_lore)).
st(root, property(has_skill, inst(actor, you), music)).
st(root, property(has_skill, inst(actor, you), healing)).
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
at(root, skill_link(elocution, contest, no_tool, shift_blame, between(shifter, new_victim), either(o(success, shifted), o(success, suspected)))).
at(root, skill_link(thievery, contest, no_tool, steal, between(thief, victim), either(o(get_item, get), o(get_item, dont_get)))).
at(root, skill_link(thievery, contest, no_tool, steal, between(thief, victim), either(o(get_caught, safe), o(get_caught, caught)))).
at(root, skill_link(storytelling, promotes, no_tool, tell_story, teller, o(quality, entertaining))).
at(root, skill_link(elocution, required, no_tool, talk_down, asking, o(attitude, convinced))).
at(root, skill_link(storytelling, promotes, no_tool, talk_down, asking, o(attitude, convinced))).
at(root, skill_link(elocution, required, no_tool, talk_down, asking, o(enraged, not_enraged))).
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
at(root, has_tool_for(inst(actor, you), music)).
at(root, has_tool_for(inst(actor, you), healing)).
at(root, has_tool_for(inst(actor, businessperson_12), fighting)).
setting(root, town).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(get_injured, safe), o(get_injured, injured)))).
at(root, likely_outcome(option(3), o(success, no_effect))).
at(root, unlikely_outcome(option(2), o(attitude, convinced))).
at(root, unlikely_outcome(option(2), o(enraged, not_enraged))).
at(root, unlikely_outcome(option(3), o(success, cursed))).
at(root, likely_outcome(option(2), o(attitude, unconvinced))).
at(root, outcome_predictability(option(2), o(attitude, unconvinced), predictable)).
at(root, outcome(option(2), o(attitude, unconvinced))).
at(root, likely_outcome(option(2), o(enraged, enraged))).
at(root, outcome_predictability(option(2), o(enraged, enraged), predictable)).
at(root, outcome_predictability(option(3), o(success, no_effect), predictable)).
at(root, outcome(option(3), o(success, no_effect))).
at(root, likely_outcome(option(1), o(onwards, onwards))).
at(root, outcome_predictability(option(1), o(onwards, onwards), predictable)).
at(root, possible_outcome_perception(option(2), o(enraged, enraged), hinders, avoid_threats_to(inst(actor, you)))).
at(root, possible_outcome_perception(option(2), o(attitude, convinced), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, possible_outcome_perception(option(3), o(success, cursed), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, possible_outcome_perception(option(3), o(success, cursed), fails, preserve_original_form(inst(actor, tough_11)))).
at(root, possible_outcome_perception(option(1), o(onwards, onwards), fails, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(avoid_accusations(inst(actor, you)))).
at(root, player_goal(preserve_original_form(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), wilderness_lore))).
at(root, player_goal(have_tool_for(inst(actor, you), music))).
at(root, player_goal(have_tool_for(inst(actor, you), healing))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, guilty(inst(actor, tough_11))).
at(root, player_goal(avoid_threats_to(inst(actor, businessperson_12)))).
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
at(root, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), healing), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, tough_11), fighting), low)).
at(root, goal_stakes(have_tool_for(inst(actor, businessperson_12), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, businessperson_12), tinkering), low)).
at(root, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, tough_11)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, businessperson_12)), high)).
at(root, outcome_perception(option(2), hinders, avoid_threats_to(inst(actor, you)))).
at(root, outcome_perception(option(2), hinders, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, outcome_perception(option(3), hinders, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, outcome_perception(option(1), fails, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, outcome_perception(option(2), fails, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), fails, as_intended(inst(actor, you)))).
at(root, outcome_stakes(option(1), high)).
at(root, outcome_stakes(option(2), high)).
at(root, outcome_stakes(option(3), high)).
at(root, important_outcome(option(2), o(enraged, enraged))).
at(root, important_outcome(option(1), o(onwards, onwards))).
at(root, important_outcome(option(3), o(success, no_effect))).
at(root, important_outcome(option(2), o(attitude, unconvinced))).
at(root, overall_predictability(option(1), predictable)).
at(root, overall_predictability(option(2), predictable)).
at(root, overall_predictability(option(3), predictable)).
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
at(root, expectation(option(2), enables, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(3), enables, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, initiated_by(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), inst(actor, tough_11))).
surface_property(number, inst(item, oboe_35), singular).
surface_property(number, inst(item, perfume_24), plural).
surface_property(number, inst(item, medicine_chest_36), singular).
surface_property(number, inst(item, medicine_book_28), singular).
surface_property(number, inst(item, ring_25), singular).
at(root, can_trade(inst(actor, you), inst(item, perfume_24))).
at(root, can_trade(inst(actor, you), inst(item, ring_25))).
at(root, can_trade(inst(actor, you), inst(item, medicine_book_28))).
at(root, can_trade(inst(actor, you), inst(item, oboe_35))).
at(root, can_trade(inst(actor, you), inst(item, medicine_chest_36))).
at(root, can_trade(inst(actor, businessperson_12), inst(item, treasure_13))).
at(root, can_trade(inst(actor, businessperson_12), inst(item, spear_56))).
at(root, expectation(option(2), threatens, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), threatens, preserve_original_form(inst(actor, tough_11)))).
at(root, expectation(option(2), fails, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), fails, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(2), fails, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(3), fails, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(2), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(3), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, perceived_option_stakes(option(1), high)).
at(root, perceived_option_stakes(option(2), high)).
at(root, perceived_option_stakes(option(3), high)).
at(root, perceived_stakes(high)).
at(root, option_feel(option(2), longshot)).
at(root, option_feel(option(3), longshot)).
at(root, option_feel(option(1), bad)).
at(root, option_structure(negative_alternatives)).
at(root, option_feel(option(1), doomed)).
at(root, outcome_overall(option(2), bad)).
at(root, outcome_overall(option(3), bad)).
at(root, outcome_overall(option(1), awful)).
at(root, outcome_feel(option(1), expected_failure)).
at(root, outcome_feel(option(2), expected_failure)).
at(root, outcome_feel(option(3), expected_failure)).
at(root, relevant_skill(option(2), inst(actor, you), missing, elocution)).
at(root, relevant_skill(option(3), inst(actor, you), missing, sorcery)).
at(root, is_powerful(inst(actor, tough_11))).
at(root, is_powerless(inst(actor, businessperson_12))).
at(root, actual_stakes(high)).
at(root, option_structure(uncomfortable)).
at(root, option_structure(bleak)).
surface_property(name, inst(item, treasure_13), "perfume").
surface_property(name, inst(item, spear_56), "spear").
surface_property(person, inst(item, perfume_24), third).
surface_property(person, inst(item, ring_25), third).
surface_property(person, inst(item, medicine_book_28), third).
surface_property(person, inst(actor, tough_11), third).
surface_property(person, inst(actor, businessperson_12), third).
surface_property(person, inst(item, treasure_13), third).
surface_property(person, inst(item, oboe_35), third).
surface_property(person, inst(item, medicine_chest_36), third).
surface_property(person, inst(item, spear_56), third).
surface_property(number, inst(actor, tough_11), plural).
surface_property(number, inst(actor, businessperson_12), singular).
surface_property(number, inst(item, treasure_13), plural).
surface_property(number, inst(item, spear_56), singular).
surface_property(gender, inst(item, perfume_24), neuter).
surface_property(gender, inst(item, ring_25), neuter).
surface_property(gender, inst(item, medicine_book_28), neuter).
surface_property(gender, inst(actor, tough_11), neuter).
surface_property(gender, inst(actor, businessperson_12), masculine).
surface_property(gender, inst(item, treasure_13), neuter).
surface_property(gender, inst(item, oboe_35), neuter).
surface_property(gender, inst(item, medicine_chest_36), neuter).
surface_property(gender, inst(item, spear_56), neuter).
surface_property(determined, inst(item, perfume_24), true).
surface_property(determined, inst(item, ring_25), true).
surface_property(determined, inst(item, medicine_book_28), true).
surface_property(determined, inst(actor, tough_11), true).
surface_property(determined, inst(actor, businessperson_12), true).
surface_property(determined, inst(item, treasure_13), true).
surface_property(determined, inst(item, oboe_35), true).
surface_property(determined, inst(item, medicine_chest_36), true).
surface_property(determined, inst(item, spear_56), true).
successor(root, option(1), root_1).
successor(root, option(2), root_2).
successor(root, option(3), root_3).
story_node(root_1).
story_node(root_2).
story_node(root_3).
st(root_2, relation(threatening, inst(actor, tough_11), inst(actor, you))).
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
st(root_2, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_3, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_1, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_2, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_3, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_1, relation(has_item, inst(actor, you), inst(item, medicine_book_28))).
st(root_2, relation(has_item, inst(actor, you), inst(item, medicine_book_28))).
st(root_3, relation(has_item, inst(actor, you), inst(item, medicine_book_28))).
st(root_1, property(type, inst(item, perfume_24), perfume)).
st(root_2, property(type, inst(item, perfume_24), perfume)).
st(root_3, property(type, inst(item, perfume_24), perfume)).
st(root_1, property(type, inst(item, ring_25), ring)).
st(root_2, property(type, inst(item, ring_25), ring)).
st(root_3, property(type, inst(item, ring_25), ring)).
st(root_1, property(type, inst(item, medicine_book_28), medicine_book)).
st(root_2, property(type, inst(item, medicine_book_28), medicine_book)).
st(root_3, property(type, inst(item, medicine_book_28), medicine_book)).
st(root_1, inst(item, perfume_24)).
st(root_2, inst(item, perfume_24)).
st(root_3, inst(item, perfume_24)).
st(root_1, inst(item, ring_25)).
st(root_2, inst(item, ring_25)).
st(root_3, inst(item, ring_25)).
st(root_1, inst(item, medicine_book_28)).
st(root_2, inst(item, medicine_book_28)).
st(root_3, inst(item, medicine_book_28)).
st(root_2, inst(actor, tough_11)).
st(root_3, inst(actor, tough_11)).
st(root_2, inst(actor, businessperson_12)).
st(root_3, inst(actor, businessperson_12)).
st(root_2, inst(item, treasure_13)).
st(root_3, inst(item, treasure_13)).
st(root_1, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_2, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_3, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_1, relation(has_item, inst(actor, you), inst(item, medicine_chest_36))).
st(root_2, relation(has_item, inst(actor, you), inst(item, medicine_chest_36))).
st(root_3, relation(has_item, inst(actor, you), inst(item, medicine_chest_36))).
st(root_1, property(type, inst(item, oboe_35), oboe)).
st(root_2, property(type, inst(item, oboe_35), oboe)).
st(root_3, property(type, inst(item, oboe_35), oboe)).
st(root_1, property(type, inst(item, medicine_chest_36), medicine_chest)).
st(root_2, property(type, inst(item, medicine_chest_36), medicine_chest)).
st(root_3, property(type, inst(item, medicine_chest_36), medicine_chest)).
st(root_1, inst(item, oboe_35)).
st(root_2, inst(item, oboe_35)).
st(root_3, inst(item, oboe_35)).
st(root_1, inst(item, medicine_chest_36)).
st(root_2, inst(item, medicine_chest_36)).
st(root_3, inst(item, medicine_chest_36)).
st(root_2, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))).
st(root_3, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))).
st(root_2, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
st(root_3, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
st(root_2, property(type, inst(actor, tough_11), bandits)).
st(root_3, property(type, inst(actor, tough_11), bandits)).
st(root_2, property(type, inst(actor, businessperson_12), merchant)).
st(root_3, property(type, inst(actor, businessperson_12), merchant)).
st(root_2, property(type, inst(item, treasure_13), perfume)).
st(root_3, property(type, inst(item, treasure_13), perfume)).
st(root_2, property(has_skill, inst(actor, tough_11), fighting)).
st(root_3, property(has_skill, inst(actor, tough_11), fighting)).
st(root_2, property(has_skill, inst(actor, businessperson_12), literacy)).
st(root_3, property(has_skill, inst(actor, businessperson_12), literacy)).
st(root_2, property(has_skill, inst(actor, businessperson_12), tinkering)).
st(root_3, property(has_skill, inst(actor, businessperson_12), tinkering)).
st(root_2, property(type, inst(item, spear_56), spear)).
st(root_3, property(type, inst(item, spear_56), spear)).
st(root_2, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56))).
st(root_3, relation(has_item, inst(actor, businessperson_12), inst(item, spear_56))).
st(root_2, inst(item, spear_56)).
st(root_3, inst(item, spear_56)).
st(root_1, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_2, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_3, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_1, property(has_skill, inst(actor, you), music)).
st(root_2, property(has_skill, inst(actor, you), music)).
st(root_3, property(has_skill, inst(actor, you), music)).
st(root_1, property(has_skill, inst(actor, you), healing)).
st(root_2, property(has_skill, inst(actor, you), healing)).
st(root_3, property(has_skill, inst(actor, you), healing)).
node_status_reached(root_1, uninitialized).
node_status_reached(root_2, uninitialized).
node_status_reached(root_3, uninitialized).
node_status_reached(root, branched).
node_status_reached(root, polished).
node_status(root_1, uninitialized).
node_status(root_2, uninitialized).
node_status(root_3, uninitialized).
node_status(root, polished).
at(root_2, guilty(inst(actor, tough_11))).
at(root_3, guilty(inst(actor, tough_11))).
at(root_1, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, tough_11)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, tough_11)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, businessperson_12)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, businessperson_12)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, tough_11)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, tough_11)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, businessperson_12)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, businessperson_12)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, tough_11)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, tough_11)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, businessperson_12)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, businessperson_12)), high)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, tough_11), fighting), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, tough_11), fighting), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, businessperson_12), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, businessperson_12), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, businessperson_12), tinkering), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, businessperson_12), tinkering), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), healing), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), healing), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), healing), low)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, tough_11)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, tough_11)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, businessperson_12)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, businessperson_12)), high)).
path_length(root_1, 1).
path_length(root_2, 1).
path_length(root_3, 1).
at(root_1, perceived_stakes(none)).
at(root_2, perceived_stakes(none)).
at(root_3, perceived_stakes(none)).
at(root_2, is_powerful(inst(actor, tough_11))).
at(root_3, is_powerful(inst(actor, tough_11))).
at(root_2, is_powerless(inst(actor, businessperson_12))).
at(root_3, is_powerless(inst(actor, businessperson_12))).
at(root_1, text_mode(example)).
at(root_2, text_mode(example)).
at(root_3, text_mode(example)).
max_unique(56).
