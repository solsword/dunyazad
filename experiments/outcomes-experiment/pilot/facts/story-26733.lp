surface_property(name, inst(actor, you), "Dunyazad").
surface_property(person, inst(actor, you), second).
surface_property(gender, inst(actor, you), feminine).
surface_property(number, inst(actor, you), singular).
surface_property(determined, inst(actor, you), false).
seed(26733).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
at(root, perceived_stakes(none)).
setup_priority(on_sale, 6).
setup_priority(tavern, 4).
setup_priority(threatened_innocents, 3).
setup_priority(market, 1).
setup_priority(monster_attack, 5).
setup_priority(healer, 2).
action_priority(attack, 15).
action_priority(shift_blame, 18).
action_priority(steal, 2).
action_priority(flee, 8).
action_priority(tell_story, 19).
action_priority(talk_down, 9).
action_priority(pay_off, 4).
action_priority(accuse, 5).
action_priority(deny_blame, 10).
action_priority(trade, 16).
action_priority(reach_destination, 12).
action_priority(play_song, 3).
action_priority(treat_injury, 7).
action_priority(dispel, 1).
action_priority(polymorph, 17).
action_priority(gossip, 13).
action_priority(explain_innocence, 20).
action_priority(buy_healing, 6).
action_priority(pacify, 14).
action_priority(travel_onwards, 11).
at(root, text_mode(example)).
st(root, property(type, inst(actor, you), person)).
st(root, property(has_skill, inst(actor, you), literacy)).
st(root, inst(actor, you)).
st(root, state(party_member, inst(actor, you))).
at(root, option(1)).
vignette(root, root).
setup(root, market).
at(root, setup_arg(merchant_one, inst(actor, merchant_14))).
at(root, setup_arg(noble, inst(actor, aristocrat_16))).
at(root, setup_arg(lowlife, inst(actor, bad_guy_19))).
st(root, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root, property(type, inst(item, perfume_24), perfume)).
st(root, property(type, inst(item, ring_25), ring)).
st(root, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root, inst(item, perfume_24)).
st(root, inst(item, ring_25)).
st(root, inst(item, ancient_grimoire_27)).
spontaneous(st(root, inst(actor, merchant_14))).
spontaneous(st(root, inst(actor, aristocrat_16))).
spontaneous(st(root, inst(actor, bad_guy_19))).
st(root, inst(actor, merchant_14)).
st(root, inst(actor, aristocrat_16)).
st(root, inst(actor, bad_guy_19)).
st(root, relation(has_item, inst(actor, you), inst(item, spear_37))).
st(root, property(type, inst(item, spear_37), spear)).
st(root, inst(item, spear_37)).
at(root, action(option(3), attack)).
at(root, action(option(1), polymorph)).
at(root, action(option(2), polymorph)).
at(root, consequence_of(option(3), o(aggressor_state, killed), state(killed, inst(actor, you)))).
at(root, arg(option(3), aggressor, inst(actor, you))).
at(root, consequence_of(option(3), o(aggressor_state, injured), state(injured, inst(actor, you)))).
at(root, consequence(option(3), state(killed, inst(actor, you)))).
at(root, outcome(option(3), o(aggressor_state, killed))).
spontaneous(st(root, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
spontaneous(st(root, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
st(root, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))).
st(root, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))).
at(root, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
spontaneous(st(root, property(type, inst(actor, merchant_14), merchant))).
spontaneous(st(root, property(type, inst(actor, aristocrat_16), noble))).
spontaneous(st(root, property(type, inst(actor, bad_guy_19), bandits))).
st(root, property(type, inst(actor, merchant_14), merchant)).
st(root, property(type, inst(actor, aristocrat_16), noble)).
st(root, property(type, inst(actor, bad_guy_19), bandits)).
at(root, consequence_of(option(3), o(get_loot, loot), relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27)))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_14), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, aristocrat_16), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, bad_guy_19), fighting))).
st(root, property(has_skill, inst(actor, merchant_14), literacy)).
st(root, property(has_skill, inst(actor, aristocrat_16), literacy)).
st(root, property(has_skill, inst(actor, bad_guy_19), fighting)).
at(root, arg(option(1), target, inst(actor, bad_guy_19))).
at(root, arg(option(2), target, inst(actor, aristocrat_16))).
at(root, consequence_of(option(3), o(target_state, killed), state(killed, inst(actor, bad_guy_19)))).
at(root, arg(option(3), target, inst(actor, bad_guy_19))).
at(root, consequence_of(option(3), o(target_state, injured), state(injured, inst(actor, bad_guy_19)))).
at(root, consequence_of(option(3), o(target_state, killed), resolves, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, consequence_of(option(3), o(target_state, injured), resolves, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, consequence_of(option(3), o(success, victory), resolves, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, consequence_of(option(1), o(success, cursed), property(polymorphed, inst(actor, bad_guy_19), bandits))).
at(root, consequence_of(option(2), o(success, cursed), property(polymorphed, inst(actor, aristocrat_16), noble))).
at(root, outcome(option(3), o(success, defeat))).
at(root, consequence_of(option(3), o(success, victory), _not, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
at(root, consequence_of(option(3), o(target_state, killed), _not, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
at(root, consequence_of(option(3), o(target_state, injured), _not, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
at(root, consequence_of(option(1), o(success, cursed), nullifies, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, consequence_of(option(2), o(success, cursed), nullifies, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, consequence_of(option(1), o(success, cursed), property(has_skill, inst(actor, bad_guy_19), unintelligent))).
at(root, consequence_of(option(2), o(success, cursed), property(has_skill, inst(actor, aristocrat_16), unintelligent))).
unresolved_potential(root, option(1), potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
unresolved_potential(root, option(2), potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
unresolved_potential(root, option(3), potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
unresolved_potential(root, option(1), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
unresolved_potential(root, option(2), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
unresolved_potential(root, option(3), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence_of(option(1), o(success, cursed), _not, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14)))).
at(root, consequence_of(option(2), o(success, cursed), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
spontaneous(st(root, property(type, inst(item, spear_57), spear))).
spontaneous(st(root, property(type, inst(item, sack_of_gold_70), sack_of_gold))).
spontaneous(st(root, property(type, inst(item, plants_book_72), plants_book))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_14), inst(item, spear_57)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, plants_book_72)))).
spontaneous(st(root, inst(item, spear_57))).
spontaneous(st(root, inst(item, sack_of_gold_70))).
spontaneous(st(root, inst(item, plants_book_72))).
st(root, property(type, inst(item, spear_57), spear)).
st(root, property(type, inst(item, sack_of_gold_70), sack_of_gold)).
st(root, property(type, inst(item, plants_book_72), plants_book)).
st(root, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70))).
st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, plants_book_72))).
st(root, inst(item, spear_57)).
st(root, inst(item, sack_of_gold_70)).
st(root, inst(item, plants_book_72)).
at(root, consequence_of(option(2), o(success, cursed), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70)))).
at(root, consequence_of(option(2), o(success, cursed), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, plants_book_72)))).
spontaneous(st(root, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
st(root, relation(selling, inst(actor, merchant_14), inst(item, spear_57))).
at(root, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
at(root, category(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), urgent)).
at(root, category(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), immediate)).
at(root, category(potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))), urgent)).
at(root, category(potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))), immediate)).
at(root, arg(option(1), caster, inst(actor, you))).
at(root, arg(option(2), caster, inst(actor, you))).
surface_property(name, inst(item, perfume_24), "perfume").
surface_property(name, inst(item, spear_37), "spear").
surface_property(name, inst(item, ancient_grimoire_27), "ancient grimoire").
surface_property(name, inst(item, ring_25), "ring").
surface_property(name, inst(actor, merchant_14), "merchant").
surface_property(name, inst(actor, aristocrat_16), "noble").
surface_property(name, inst(actor, bad_guy_19), "bandits").
st(root, property(has_skill, inst(actor, you), wilderness_lore)).
st(root, property(has_skill, inst(actor, you), fighting)).
st(root, property(has_skill, inst(actor, you), sorcery)).
at(root, outcome(option(3), o(target_state, unharmed))).
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
at(root, has_tool_for(inst(actor, you), sorcery)).
at(root, has_tool_for(inst(actor, you), fighting)).
at(root, has_tool_for(inst(actor, merchant_14), fighting)).
at(root, has_tool_for(inst(actor, aristocrat_16), healing)).
at(root, has_tool_for(inst(actor, aristocrat_16), wilderness_lore)).
at(root, likely_outcome(option(1), o(success, cursed))).
at(root, likely_outcome(option(2), o(success, cursed))).
setting(root, wilderness).
at(root, skill_link(wilderness_lore, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(wilderness_lore, contest, no_tool, flee, between(fearful, from), either(o(get_injured, safe), o(get_injured, injured)))).
at(root, likely_outcome(option(3), o(aggressor_state, unharmed))).
at(root, likely_outcome(option(3), o(target_state, injured))).
at(root, likely_outcome(option(3), o(success, victory))).
at(root, unlikely_outcome(option(3), o(aggressor_state, injured))).
at(root, unlikely_outcome(option(3), o(target_state, unharmed))).
at(root, unlikely_outcome(option(3), o(success, defeat))).
at(root, unlikely_outcome(option(3), o(aggressor_state, killed))).
at(root, outcome(option(1), o(success, no_effect))).
at(root, outcome(option(2), o(success, no_effect))).
at(root, outcome(option(3), o(get_loot, nothing))).
at(root, outcome_predictability(option(3), o(get_loot, nothing), unpredictable)).
at(root, neutral_outcome(option(3), o(target_state, killed))).
at(root, neutral_outcome(option(3), o(get_loot, loot))).
at(root, neutral_outcome(option(3), o(get_loot, nothing))).
at(root, neutral_outcome(option(3), o(success, tie))).
at(root, neutral_outcome(option(1), o(success, no_effect))).
at(root, neutral_outcome(option(2), o(success, no_effect))).
at(root, outcome_predictability(option(1), o(success, no_effect), unexpected)).
at(root, outcome_predictability(option(2), o(success, no_effect), unexpected)).
at(root, outcome_predictability(option(3), o(target_state, unharmed), unfair)).
at(root, outcome_predictability(option(3), o(success, defeat), unfair)).
at(root, outcome_predictability(option(3), o(aggressor_state, killed), unfair)).
at(root, possible_outcome_perception(option(2), o(success, cursed), advances, avoid_accusations(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(3), o(success, victory), advances, avoid_threats_to(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(3), o(target_state, killed), advances, avoid_threats_to(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(3), o(target_state, injured), advances, avoid_threats_to(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(1), o(success, cursed), advances, avoid_threats_to(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(3), o(aggressor_state, injured), fails, preserve_health(inst(actor, you)))).
at(root, possible_outcome_perception(option(3), o(target_state, injured), fails, preserve_health(inst(actor, bad_guy_19)))).
at(root, possible_outcome_perception(option(3), o(aggressor_state, killed), fails, preserve_health(inst(actor, you)))).
at(root, possible_outcome_perception(option(3), o(target_state, killed), fails, preserve_health(inst(actor, bad_guy_19)))).
at(root, possible_outcome_perception(option(2), o(success, cursed), fails, preserve_original_form(inst(actor, aristocrat_16)))).
at(root, possible_outcome_perception(option(1), o(success, cursed), fails, preserve_original_form(inst(actor, bad_guy_19)))).
at(root, possible_outcome_perception(option(3), o(get_loot, loot), achieves, have_tool_for(inst(actor, you), sorcery))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(avoid_accusations(inst(actor, you)))).
at(root, player_goal(preserve_original_form(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), wilderness_lore))).
at(root, player_goal(have_tool_for(inst(actor, you), fighting))).
at(root, player_goal(have_tool_for(inst(actor, you), sorcery))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, player_goal(avoid_threats_to(inst(actor, merchant_14)))).
at(root, player_goal(avoid_accusations(inst(actor, merchant_14)))).
at(root, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root, goal_stakes(preserve_health(inst(actor, merchant_14)), high)).
at(root, goal_stakes(preserve_health(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(preserve_health(inst(actor, bad_guy_19)), high)).
at(root, goal_stakes(as_intended(inst(actor, you)), low)).
at(root, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, merchant_14)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, bad_guy_19)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, merchant_14)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, bad_guy_19)), high)).
at(root, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), fighting), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, bad_guy_19), fighting), low)).
at(root, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, bad_guy_19)), high)).
at(root, outcome_perception(option(1), hinders, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(2), hinders, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(3), hinders, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(1), hinders, avoid_threats_to(inst(actor, merchant_14)))).
at(root, outcome_perception(option(2), hinders, avoid_threats_to(inst(actor, merchant_14)))).
at(root, outcome_perception(option(3), hinders, avoid_threats_to(inst(actor, merchant_14)))).
at(root, outcome_perception(option(3), fails, preserve_health(inst(actor, you)))).
at(root, outcome_perception(option(1), fails, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(2), fails, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), fails, as_intended(inst(actor, you)))).
at(root, outcome_stakes(option(1), high)).
at(root, outcome_stakes(option(2), high)).
at(root, outcome_stakes(option(3), high)).
at(root, important_outcome(option(3), o(success, defeat))).
at(root, important_outcome(option(3), o(aggressor_state, killed))).
at(root, important_outcome(option(1), o(success, no_effect))).
at(root, important_outcome(option(2), o(success, no_effect))).
at(root, important_outcome(option(3), o(target_state, unharmed))).
at(root, overall_predictability(option(1), unexpected)).
at(root, overall_predictability(option(2), unexpected)).
at(root, overall_predictability(option(3), unfair)).
at(root, relevant_to(option(1), potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, relevant_to(option(2), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, relevant_to(option(3), potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, importance(1, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(1, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, importance(2, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(2, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, importance(3, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(3, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, importance(4, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(4, potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, importance(8, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57))))).
at(root, some_importance(1)).
at(root, some_importance(2)).
at(root, some_importance(3)).
at(root, some_importance(4)).
at(root, some_importance(8)).
at(root, most_important(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, most_important(potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))))).
at(root, problematic_for(potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))), inst(actor, merchant_14))).
at(root, problematic_for(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), inst(actor, merchant_14))).
at(root, motivated(option(1))).
at(root, motivated(option(2))).
at(root, motivated(option(3))).
at(root, expectation(option(3), enables, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(2), enables, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(1), enables, avoid_threats_to(inst(actor, merchant_14)))).
at(root, expectation(option(3), enables, avoid_threats_to(inst(actor, merchant_14)))).
at(root, initiated_by(potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57))), inst(actor, merchant_14))).
at(root, initiated_by(potential(problem, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))), inst(actor, bad_guy_19))).
at(root, initiated_by(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), inst(actor, aristocrat_16))).
surface_property(number, inst(item, perfume_24), plural).
surface_property(number, inst(item, spear_37), singular).
surface_property(number, inst(item, ancient_grimoire_27), singular).
surface_property(number, inst(item, ring_25), singular).
at(root, can_trade(inst(actor, you), inst(item, perfume_24))).
at(root, can_trade(inst(actor, you), inst(item, ring_25))).
at(root, can_trade(inst(actor, you), inst(item, ancient_grimoire_27))).
at(root, can_trade(inst(actor, you), inst(item, spear_37))).
at(root, can_trade(inst(actor, merchant_14), inst(item, spear_57))).
at(root, can_trade(inst(actor, aristocrat_16), inst(item, sack_of_gold_70))).
at(root, can_trade(inst(actor, aristocrat_16), inst(item, plants_book_72))).
at(root, setup_arg(goods_one, inst(item, spear_57))).
at(root, setup_arg(goods_two, inst(item, plants_book_72))).
at(root, expectation(option(3), threatens, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), threatens, preserve_health(inst(actor, bad_guy_19)))).
at(root, expectation(option(2), threatens, preserve_original_form(inst(actor, aristocrat_16)))).
at(root, expectation(option(1), threatens, preserve_original_form(inst(actor, bad_guy_19)))).
at(root, expectation(option(3), fails, preserve_health(inst(actor, bad_guy_19)))).
at(root, expectation(option(2), fails, preserve_original_form(inst(actor, aristocrat_16)))).
at(root, expectation(option(1), fails, preserve_original_form(inst(actor, bad_guy_19)))).
at(root, expectation(option(2), achieves, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(1), achieves, avoid_threats_to(inst(actor, merchant_14)))).
at(root, expectation(option(3), achieves, avoid_threats_to(inst(actor, merchant_14)))).
at(root, expectation(option(1), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(2), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(3), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), fighting))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(1), irrelevant, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), fighting))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, avoid_threats_to(inst(actor, merchant_14)))).
at(root, expectation(option(3), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), fighting))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, avoid_accusations(inst(actor, merchant_14)))).
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
at(root, outcome_overall(option(1), bad)).
at(root, outcome_overall(option(2), bad)).
at(root, outcome_overall(option(3), awful)).
at(root, outcome_feel(option(1), unfair)).
at(root, outcome_feel(option(2), unfair)).
at(root, outcome_feel(option(3), unfair)).
at(root, relevant_skill(option(1), inst(actor, you), has, sorcery)).
at(root, relevant_skill(option(2), inst(actor, you), has, sorcery)).
at(root, relevant_skill(option(3), inst(actor, you), has, fighting)).
at(root, relevant_skill(option(3), inst(actor, bad_guy_19), has, fighting)).
at(root, relevant_tool(option(1), inst(actor, you), has, inst(item, ancient_grimoire_27))).
at(root, relevant_tool(option(2), inst(actor, you), has, inst(item, ancient_grimoire_27))).
at(root, relevant_tool(option(3), inst(actor, you), has, inst(item, spear_37))).
at(root, relevant_tool(option(3), inst(actor, bad_guy_19), missing, fighting)).
at(root, surprising(option(1))).
at(root, surprising(option(2))).
at(root, surprising(option(3))).
at(root, is_powerful(inst(actor, aristocrat_16))).
at(root, is_powerful(inst(actor, bad_guy_19))).
at(root, is_powerless(inst(actor, merchant_14))).
at(root, guilty(inst(actor, bad_guy_19))).
at(root, guilty(inst(actor, aristocrat_16))).
at(root, actual_stakes(high)).
unresolved_potential(root, option(1), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
unresolved_potential(root, option(2), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
unresolved_potential(root, option(3), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
surface_property(name, inst(item, spear_57), "spear").
surface_property(name, inst(item, sack_of_gold_70), "sack of gold").
surface_property(name, inst(item, plants_book_72), "book of herbal lore").
surface_property(person, inst(item, perfume_24), third).
surface_property(person, inst(item, ring_25), third).
surface_property(person, inst(item, ancient_grimoire_27), third).
surface_property(person, inst(actor, merchant_14), third).
surface_property(person, inst(actor, aristocrat_16), third).
surface_property(person, inst(actor, bad_guy_19), third).
surface_property(person, inst(item, spear_37), third).
surface_property(person, inst(item, spear_57), third).
surface_property(person, inst(item, sack_of_gold_70), third).
surface_property(person, inst(item, plants_book_72), third).
surface_property(number, inst(actor, merchant_14), singular).
surface_property(number, inst(actor, aristocrat_16), singular).
surface_property(number, inst(actor, bad_guy_19), plural).
surface_property(number, inst(item, spear_57), singular).
surface_property(number, inst(item, sack_of_gold_70), singular).
surface_property(number, inst(item, plants_book_72), singular).
surface_property(gender, inst(item, perfume_24), neuter).
surface_property(gender, inst(item, ring_25), neuter).
surface_property(gender, inst(item, ancient_grimoire_27), neuter).
surface_property(gender, inst(actor, merchant_14), feminine).
surface_property(gender, inst(actor, aristocrat_16), masculine).
surface_property(gender, inst(actor, bad_guy_19), neuter).
surface_property(gender, inst(item, spear_37), neuter).
surface_property(gender, inst(item, spear_57), neuter).
surface_property(gender, inst(item, sack_of_gold_70), neuter).
surface_property(gender, inst(item, plants_book_72), neuter).
surface_property(determined, inst(item, perfume_24), true).
surface_property(determined, inst(item, ring_25), true).
surface_property(determined, inst(item, ancient_grimoire_27), true).
surface_property(determined, inst(actor, merchant_14), true).
surface_property(determined, inst(actor, aristocrat_16), true).
surface_property(determined, inst(actor, bad_guy_19), true).
surface_property(determined, inst(item, spear_37), true).
surface_property(determined, inst(item, spear_57), true).
surface_property(determined, inst(item, sack_of_gold_70), true).
surface_property(determined, inst(item, plants_book_72), true).
successor(root, option(2), root_2).
successor(root, option(3), root_3).
story_node(root_2).
story_node(root_3).
st(root_3, state(killed, inst(actor, you))).
st(root_2, property(type, inst(actor, you), person)).
st(root_3, property(type, inst(actor, you), person)).
successor(root, option(1), root_2).
st(root_2, property(has_skill, inst(actor, you), literacy)).
st(root_3, property(has_skill, inst(actor, you), literacy)).
st(root_2, inst(actor, you)).
st(root_3, inst(actor, you)).
st(root_2, state(party_member, inst(actor, you))).
st(root_3, state(party_member, inst(actor, you))).
st(root_2, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_3, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_2, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_3, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_2, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root_3, relation(has_item, inst(actor, you), inst(item, ancient_grimoire_27))).
st(root_2, property(type, inst(item, perfume_24), perfume)).
st(root_3, property(type, inst(item, perfume_24), perfume)).
st(root_2, property(type, inst(item, ring_25), ring)).
st(root_3, property(type, inst(item, ring_25), ring)).
st(root_2, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root_3, property(type, inst(item, ancient_grimoire_27), ancient_grimoire)).
st(root_2, inst(item, perfume_24)).
st(root_3, inst(item, perfume_24)).
st(root_2, inst(item, ring_25)).
st(root_3, inst(item, ring_25)).
st(root_2, inst(item, ancient_grimoire_27)).
st(root_3, inst(item, ancient_grimoire_27)).
st(root_2, inst(actor, merchant_14)).
st(root_3, inst(actor, merchant_14)).
st(root_2, inst(actor, aristocrat_16)).
st(root_3, inst(actor, aristocrat_16)).
st(root_2, inst(actor, bad_guy_19)).
st(root_3, inst(actor, bad_guy_19)).
st(root_2, relation(has_item, inst(actor, you), inst(item, spear_37))).
st(root_3, relation(has_item, inst(actor, you), inst(item, spear_37))).
st(root_2, property(type, inst(item, spear_37), spear)).
st(root_3, property(type, inst(item, spear_37), spear)).
st(root_2, inst(item, spear_37)).
st(root_3, inst(item, spear_37)).
st(root_2, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))).
st(root_3, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))).
st(root_2, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))).
st(root_3, relation(threatening, inst(actor, bad_guy_19), inst(actor, merchant_14))).
st(root_2, property(type, inst(actor, merchant_14), merchant)).
st(root_3, property(type, inst(actor, merchant_14), merchant)).
st(root_2, property(type, inst(actor, aristocrat_16), noble)).
st(root_3, property(type, inst(actor, aristocrat_16), noble)).
st(root_2, property(type, inst(actor, bad_guy_19), bandits)).
st(root_3, property(type, inst(actor, bad_guy_19), bandits)).
st(root_2, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_3, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_2, property(has_skill, inst(actor, aristocrat_16), literacy)).
st(root_3, property(has_skill, inst(actor, aristocrat_16), literacy)).
st(root_2, property(has_skill, inst(actor, bad_guy_19), fighting)).
st(root_3, property(has_skill, inst(actor, bad_guy_19), fighting)).
st(root_2, property(type, inst(item, spear_57), spear)).
st(root_3, property(type, inst(item, spear_57), spear)).
st(root_2, property(type, inst(item, sack_of_gold_70), sack_of_gold)).
st(root_3, property(type, inst(item, sack_of_gold_70), sack_of_gold)).
st(root_2, property(type, inst(item, plants_book_72), plants_book)).
st(root_3, property(type, inst(item, plants_book_72), plants_book)).
st(root_2, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root_3, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root_2, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70))).
st(root_3, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70))).
st(root_2, relation(has_item, inst(actor, aristocrat_16), inst(item, plants_book_72))).
st(root_3, relation(has_item, inst(actor, aristocrat_16), inst(item, plants_book_72))).
st(root_2, inst(item, spear_57)).
st(root_3, inst(item, spear_57)).
st(root_2, inst(item, sack_of_gold_70)).
st(root_3, inst(item, sack_of_gold_70)).
st(root_2, inst(item, plants_book_72)).
st(root_3, inst(item, plants_book_72)).
st(root_2, relation(selling, inst(actor, merchant_14), inst(item, spear_57))).
st(root_3, relation(selling, inst(actor, merchant_14), inst(item, spear_57))).
st(root_2, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_3, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_2, property(has_skill, inst(actor, you), fighting)).
st(root_3, property(has_skill, inst(actor, you), fighting)).
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
at(root_2, goal_stakes(preserve_health(inst(actor, merchant_14)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, merchant_14)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, aristocrat_16)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, aristocrat_16)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, bad_guy_19)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, bad_guy_19)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, merchant_14)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, merchant_14)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, aristocrat_16)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, aristocrat_16)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, bad_guy_19)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, bad_guy_19)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, merchant_14)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, merchant_14)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, aristocrat_16)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, aristocrat_16)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, bad_guy_19)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, bad_guy_19)), high)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, bad_guy_19), fighting), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, bad_guy_19), fighting), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), fighting), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), fighting), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, bad_guy_19)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, bad_guy_19)), high)).
path_length(root_2, 1).
path_length(root_3, 1).
at(root_2, perceived_stakes(none)).
at(root_3, perceived_stakes(none)).
at(root_2, is_powerful(inst(actor, aristocrat_16))).
at(root_3, is_powerful(inst(actor, aristocrat_16))).
at(root_2, is_powerful(inst(actor, bad_guy_19))).
at(root_3, is_powerful(inst(actor, bad_guy_19))).
at(root_2, is_powerless(inst(actor, merchant_14))).
at(root_3, is_powerless(inst(actor, merchant_14))).
at(root_2, guilty(inst(actor, bad_guy_19))).
at(root_3, guilty(inst(actor, bad_guy_19))).
at(root_2, guilty(inst(actor, aristocrat_16))).
at(root_3, guilty(inst(actor, aristocrat_16))).
at(root_2, text_mode(example)).
at(root_3, text_mode(example)).
max_unique(72).
