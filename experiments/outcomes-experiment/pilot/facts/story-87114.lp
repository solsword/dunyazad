seed(87114).
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
setup_priority(on_sale, 5).
setup_priority(tavern, 4).
setup_priority(threatened_innocents, 2).
setup_priority(market, 6).
setup_priority(monster_attack, 1).
setup_priority(healer, 3).
action_priority(attack, 18).
action_priority(shift_blame, 6).
action_priority(steal, 16).
action_priority(flee, 9).
action_priority(tell_story, 2).
action_priority(talk_down, 14).
action_priority(pay_off, 12).
action_priority(accuse, 20).
action_priority(deny_blame, 10).
action_priority(trade, 13).
action_priority(reach_destination, 7).
action_priority(play_song, 17).
action_priority(treat_injury, 11).
action_priority(dispel, 8).
action_priority(polymorph, 15).
action_priority(gossip, 19).
action_priority(explain_innocence, 4).
action_priority(buy_healing, 5).
action_priority(pacify, 1).
action_priority(travel_onwards, 3).
at(root, text_mode(example)).
st(root, property(type, inst(actor, you), person)).
st(root, property(has_skill, inst(actor, you), literacy)).
st(root, inst(actor, you)).
st(root, state(party_member, inst(actor, you))).
at(root, option(1)).
vignette(root, root).
setup(root, monster_attack).
at(root, setup_arg(monster, inst(actor, monster_20))).
st(root, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root, property(type, inst(item, ring_25), ring)).
st(root, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root, inst(item, sack_of_gold_23)).
st(root, inst(item, ring_25)).
st(root, inst(item, ancient_grimoire_27)).
spontaneous(st(root, inst(actor, monster_20))).
st(root, inst(actor, monster_20)).
st(root, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root, property(type, inst(item, oboe_35), oboe)).
st(root, inst(item, oboe_35)).
at(root, consequence_of(option(1), o(get_injured, injured), state(injured, inst(actor, you)))).
at(root, arg(option(1), pacifier, inst(actor, you))).
at(root, action(option(1), pacify)).
at(root, consequence_of(option(2), o(get_injured, injured), state(injured, inst(actor, you)))).
at(root, arg(option(2), fearful, inst(actor, you))).
at(root, action(option(2), flee)).
at(root, action(option(3), polymorph)).
at(root, outcome(option(3), o(success, cursed))).
spontaneous(st(root, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
st(root, relation(threatening, inst(actor, monster_20), inst(actor, you))).
at(root, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
spontaneous(st(root, property(type, inst(actor, monster_20), ogre))).
st(root, property(type, inst(actor, monster_20), ogre)).
at(root, consequence(option(1), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence(option(2), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence(option(3), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence(option(3), nullifies, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
spontaneous(st(root, property(has_skill, inst(actor, monster_20), fighting))).
spontaneous(st(root, property(has_skill, inst(actor, monster_20), monstrous))).
spontaneous(st(root, property(has_skill, inst(actor, monster_20), wilderness_lore))).
spontaneous(st(root, property(has_skill, inst(actor, monster_20), unintelligent))).
st(root, property(has_skill, inst(actor, monster_20), fighting)).
st(root, property(has_skill, inst(actor, monster_20), monstrous)).
st(root, property(has_skill, inst(actor, monster_20), wilderness_lore)).
st(root, property(has_skill, inst(actor, monster_20), unintelligent)).
at(root, arg(option(1), aggressive, inst(actor, monster_20))).
at(root, arg(option(3), target, inst(actor, monster_20))).
at(root, consequence_of(option(3), o(success, cursed), property(polymorphed, inst(actor, monster_20), ogre))).
at(root, consequence_of(option(1), o(mood, relaxed), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, consequence_of(option(1), o(mood, enraged), relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(2), o(success, escape), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, arg(option(2), from, inst(actor, monster_20))).
at(root, consequence(option(3), property(polymorphed, inst(actor, monster_20), ogre))).
at(root, consequence(option(1), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, consequence(option(2), resolves, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, outcome(option(1), o(mood, relaxed))).
at(root, outcome(option(2), o(success, escape))).
at(root, consequence_of(option(1), o(mood, relaxed), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(2), o(success, escape), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, consequence_of(option(3), o(success, cursed), nullifies, potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
unresolved_potential(root, option(3), potential(problem, property(polymorphed, inst(actor, monster_20), ogre))).
at(root, consequence_of(option(3), o(success, cursed), _not, relation(threatening, inst(actor, monster_20), inst(actor, you)))).
at(root, category(potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))), urgent)).
at(root, category(potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))), immediate)).
at(root, arg(option(3), caster, inst(actor, you))).
surface_property(name, inst(item, sack_of_gold_23), "sack of gold").
surface_property(name, inst(item, oboe_35), "oboe").
surface_property(name, inst(item, ancient_grimoire_27), "ancient grimoire").
surface_property(name, inst(item, ring_25), "ring").
st(root, property(has_skill, inst(actor, you), music)).
st(root, property(has_skill, inst(actor, you), acrobatics)).
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
at(root, has_tool_for(inst(actor, you), music)).
at(root, has_tool_for(inst(actor, you), sorcery)).
at(root, likely_outcome(option(3), o(success, cursed))).
at(root, likely_outcome(option(1), o(mood, relaxed))).
setting(root, city).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(get_injured, safe), o(get_injured, injured)))).
at(root, likely_outcome(option(2), o(success, escape))).
at(root, likely_outcome(option(1), o(get_injured, safe))).
at(root, likely_outcome(option(2), o(get_injured, safe))).
at(root, unlikely_outcome(option(2), o(success, failure))).
at(root, unlikely_outcome(option(1), o(get_injured, injured))).
at(root, unlikely_outcome(option(2), o(get_injured, injured))).
at(root, outcome_predictability(option(2), o(success, escape), predictable)).
at(root, outcome_predictability(option(3), o(success, cursed), predictable)).
at(root, outcome_predictability(option(1), o(mood, relaxed), predictable)).
at(root, outcome_predictability(option(1), o(get_injured, safe), predictable)).
at(root, outcome(option(1), o(get_injured, safe))).
at(root, outcome_predictability(option(2), o(get_injured, safe), predictable)).
at(root, outcome(option(2), o(get_injured, safe))).
at(root, neutral_outcome(option(3), o(success, no_effect))).
at(root, neutral_outcome(option(1), o(mood, enraged))).
at(root, possible_outcome_perception(option(1), o(mood, enraged), hinders, avoid_threats_to(inst(actor, you)))).
at(root, possible_outcome_perception(option(1), o(mood, relaxed), advances, avoid_threats_to(inst(actor, you)))).
at(root, possible_outcome_perception(option(2), o(success, escape), advances, avoid_threats_to(inst(actor, you)))).
at(root, possible_outcome_perception(option(3), o(success, cursed), advances, avoid_threats_to(inst(actor, you)))).
at(root, possible_outcome_perception(option(1), o(get_injured, injured), fails, preserve_health(inst(actor, you)))).
at(root, possible_outcome_perception(option(2), o(get_injured, injured), fails, preserve_health(inst(actor, you)))).
at(root, possible_outcome_perception(option(3), o(success, cursed), fails, preserve_original_form(inst(actor, monster_20)))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(avoid_accusations(inst(actor, you)))).
at(root, player_goal(preserve_original_form(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), music))).
at(root, player_goal(have_tool_for(inst(actor, you), acrobatics))).
at(root, player_goal(have_tool_for(inst(actor, you), sorcery))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root, goal_stakes(preserve_health(inst(actor, monster_20)), high)).
at(root, goal_stakes(as_intended(inst(actor, you)), low)).
at(root, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, monster_20)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, monster_20)), high)).
at(root, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), acrobatics), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, monster_20), fighting), low)).
at(root, goal_stakes(have_tool_for(inst(actor, monster_20), monstrous), low)).
at(root, goal_stakes(have_tool_for(inst(actor, monster_20), wilderness_lore), low)).
at(root, goal_stakes(have_tool_for(inst(actor, monster_20), unintelligent), low)).
at(root, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, monster_20)), high)).
at(root, outcome_perception(option(1), advances, avoid_threats_to(inst(actor, you)))).
at(root, outcome_perception(option(2), advances, avoid_threats_to(inst(actor, you)))).
at(root, outcome_perception(option(3), advances, avoid_threats_to(inst(actor, you)))).
at(root, outcome_perception(option(3), fails, preserve_original_form(inst(actor, monster_20)))).
at(root, outcome_perception(option(1), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(2), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), achieves, as_intended(inst(actor, you)))).
at(root, outcome_stakes(option(1), high)).
at(root, outcome_stakes(option(2), high)).
at(root, outcome_stakes(option(3), high)).
at(root, important_outcome(option(2), o(success, escape))).
at(root, important_outcome(option(3), o(success, cursed))).
at(root, important_outcome(option(1), o(mood, relaxed))).
at(root, overall_predictability(option(1), predictable)).
at(root, overall_predictability(option(2), predictable)).
at(root, overall_predictability(option(3), predictable)).
at(root, relevant_to(option(1), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, relevant_to(option(2), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
at(root, relevant_to(option(3), potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))))).
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
at(root, motivated(option(2))).
at(root, motivated(option(3))).
at(root, expectation(option(1), enables, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), enables, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), enables, avoid_threats_to(inst(actor, you)))).
at(root, initiated_by(potential(problem, relation(threatening, inst(actor, monster_20), inst(actor, you))), inst(actor, monster_20))).
surface_property(number, inst(item, sack_of_gold_23), singular).
surface_property(number, inst(item, oboe_35), singular).
surface_property(number, inst(item, ancient_grimoire_27), singular).
surface_property(number, inst(item, ring_25), singular).
at(root, can_trade(inst(actor, you), inst(item, sack_of_gold_23))).
at(root, can_trade(inst(actor, you), inst(item, ring_25))).
at(root, can_trade(inst(actor, you), inst(item, ancient_grimoire_27))).
at(root, can_trade(inst(actor, you), inst(item, oboe_35))).
at(root, expectation(option(1), threatens, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), threatens, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), threatens, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), threatens, preserve_original_form(inst(actor, monster_20)))).
at(root, expectation(option(3), fails, preserve_original_form(inst(actor, monster_20)))).
at(root, expectation(option(1), achieves, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), achieves, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), achieves, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(2), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(3), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), acrobatics))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), acrobatics))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), acrobatics))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, perceived_option_stakes(option(1), high)).
at(root, perceived_option_stakes(option(2), high)).
at(root, perceived_option_stakes(option(3), high)).
at(root, perceived_stakes(high)).
at(root, option_feel(option(3), safe)).
at(root, option_feel(option(1), hopeful)).
at(root, option_feel(option(2), hopeful)).
at(root, option_structure(positive_alternatives)).
at(root, option_feel(option(3), sure_thing)).
at(root, outcome_overall(option(1), good)).
at(root, outcome_overall(option(2), good)).
at(root, outcome_overall(option(3), good)).
at(root, outcome_feel(option(1), expected_success)).
at(root, outcome_feel(option(2), expected_success)).
at(root, outcome_feel(option(3), expected_success)).
at(root, relevant_skill(option(1), inst(actor, you), has, music)).
at(root, relevant_skill(option(3), inst(actor, you), has, sorcery)).
at(root, relevant_skill(option(2), inst(actor, you), has, acrobatics)).
at(root, relevant_tool(option(3), inst(actor, you), has, inst(item, ancient_grimoire_27))).
at(root, relevant_tool(option(1), inst(actor, you), has, inst(item, oboe_35))).
at(root, relevant_skill(option(2), inst(actor, monster_20), missing, acrobatics)).
at(root, is_powerful(inst(actor, monster_20))).
at(root, guilty(inst(actor, monster_20))).
at(root, actual_stakes(high)).
resolves_vignette(root, option(1)).
resolves_vignette(root, option(2)).
surface_property(name, inst(actor, monster_20), "ogre").
surface_property(person, inst(item, sack_of_gold_23), third).
surface_property(person, inst(item, ring_25), third).
surface_property(person, inst(item, ancient_grimoire_27), third).
surface_property(person, inst(actor, monster_20), third).
surface_property(person, inst(item, oboe_35), third).
surface_property(number, inst(actor, monster_20), singular).
surface_property(gender, inst(item, sack_of_gold_23), neuter).
surface_property(gender, inst(item, ring_25), neuter).
surface_property(gender, inst(item, ancient_grimoire_27), neuter).
surface_property(gender, inst(actor, monster_20), feminine).
surface_property(gender, inst(item, oboe_35), neuter).
surface_property(determined, inst(item, sack_of_gold_23), true).
surface_property(determined, inst(item, ring_25), true).
surface_property(determined, inst(item, ancient_grimoire_27), true).
surface_property(determined, inst(actor, monster_20), true).
surface_property(determined, inst(item, oboe_35), true).
successor(root, option(2), root_2).
successor(root, option(3), root_3).
story_node(root_2).
story_node(root_3).
st(root_3, property(polymorphed, inst(actor, monster_20), ogre)).
st(root_2, property(type, inst(actor, you), person)).
st(root_3, property(type, inst(actor, you), person)).
successor(root, option(1), root_2).
st(root_2, property(has_skill, inst(actor, you), literacy)).
st(root_3, property(has_skill, inst(actor, you), literacy)).
st(root_2, inst(actor, you)).
st(root_3, inst(actor, you)).
st(root_2, state(party_member, inst(actor, you))).
st(root_3, state(party_member, inst(actor, you))).
st(root_2, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root_3, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root_2, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_3, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_2, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root_3, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root_2, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root_3, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root_2, property(type, inst(item, ring_25), ring)).
st(root_3, property(type, inst(item, ring_25), ring)).
st(root_2, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root_3, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root_2, inst(item, sack_of_gold_23)).
st(root_3, inst(item, sack_of_gold_23)).
st(root_2, inst(item, ring_25)).
st(root_3, inst(item, ring_25)).
st(root_2, inst(item, ancient_grimoire_27)).
st(root_3, inst(item, ancient_grimoire_27)).
st(root_2, inst(actor, monster_20)).
st(root_3, inst(actor, monster_20)).
st(root_2, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_3, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_2, property(type, inst(item, oboe_35), oboe)).
st(root_3, property(type, inst(item, oboe_35), oboe)).
st(root_2, inst(item, oboe_35)).
st(root_3, inst(item, oboe_35)).
st(root_2, property(type, inst(actor, monster_20), ogre)).
st(root_3, property(type, inst(actor, monster_20), ogre)).
st(root_2, property(has_skill, inst(actor, monster_20), fighting)).
st(root_3, property(has_skill, inst(actor, monster_20), fighting)).
st(root_2, property(has_skill, inst(actor, monster_20), monstrous)).
st(root_3, property(has_skill, inst(actor, monster_20), monstrous)).
st(root_2, property(has_skill, inst(actor, monster_20), wilderness_lore)).
st(root_3, property(has_skill, inst(actor, monster_20), wilderness_lore)).
st(root_2, property(has_skill, inst(actor, monster_20), unintelligent)).
st(root_3, property(has_skill, inst(actor, monster_20), unintelligent)).
st(root_2, property(has_skill, inst(actor, you), music)).
st(root_3, property(has_skill, inst(actor, you), music)).
st(root_2, property(has_skill, inst(actor, you), acrobatics)).
st(root_3, property(has_skill, inst(actor, you), acrobatics)).
st(root_2, property(has_skill, inst(actor, you), sorcery)).
st(root_3, property(has_skill, inst(actor, you), sorcery)).
node_status_reached(root_2, uninitialized).
node_status_reached(root_3, uninitialized).
node_status_reached(root, branched).
node_status_reached(root, polished).
node_status(root_2, uninitialized).
node_status(root_3, uninitialized).
node_status(root, polished).
at(root_2, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, monster_20)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, monster_20)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, monster_20)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, monster_20)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, monster_20)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, monster_20)), high)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, monster_20), fighting), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, monster_20), fighting), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, monster_20), monstrous), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, monster_20), monstrous), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, monster_20), wilderness_lore), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, monster_20), wilderness_lore), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, monster_20), unintelligent), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, monster_20), unintelligent), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), acrobatics), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), acrobatics), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, monster_20)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, monster_20)), high)).
path_length(root_2, 1).
path_length(root_3, 1).
at(root_2, perceived_stakes(none)).
at(root_3, perceived_stakes(none)).
at(root_2, is_powerful(inst(actor, monster_20))).
at(root_3, is_powerful(inst(actor, monster_20))).
at(root_2, text_mode(example)).
at(root_3, text_mode(example)).
max_unique(35).
