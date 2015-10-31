surface_property(name, inst(actor, you), "Dunyazad").
surface_property(person, inst(actor, you), second).
surface_property(gender, inst(actor, you), feminine).
surface_property(number, inst(actor, you), singular).
surface_property(determined, inst(actor, you), false).
seed(87991).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
at(root, perceived_stakes(none)).
setup_priority(on_sale, 2).
setup_priority(tavern, 5).
setup_priority(threatened_innocents, 6).
setup_priority(market, 4).
setup_priority(monster_attack, 1).
setup_priority(healer, 3).
action_priority(attack, 9).
action_priority(shift_blame, 1).
action_priority(steal, 20).
action_priority(flee, 10).
action_priority(tell_story, 16).
action_priority(talk_down, 13).
action_priority(pay_off, 4).
action_priority(accuse, 7).
action_priority(deny_blame, 12).
action_priority(trade, 5).
action_priority(reach_destination, 6).
action_priority(play_song, 15).
action_priority(treat_injury, 11).
action_priority(dispel, 19).
action_priority(polymorph, 2).
action_priority(gossip, 17).
action_priority(explain_innocence, 18).
action_priority(buy_healing, 8).
action_priority(pacify, 3).
action_priority(travel_onwards, 14).
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
at(root, setup_arg(peasant, inst(actor, laborer_17))).
st(root, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root, property(type, inst(item, plants_book_26), plants_book)).
st(root, inst(item, sack_of_gold_23)).
st(root, inst(item, plants_book_26)).
spontaneous(st(root, inst(actor, merchant_14))).
spontaneous(st(root, inst(actor, aristocrat_16))).
spontaneous(st(root, inst(actor, laborer_17))).
st(root, inst(actor, merchant_14)).
st(root, inst(actor, aristocrat_16)).
st(root, inst(actor, laborer_17)).
st(root, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root, property(type, inst(item, oboe_35), oboe)).
st(root, inst(item, oboe_35)).
at(root, action(option(1), polymorph)).
spontaneous(st(root, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
spontaneous(st(root, state(knows_gossip, inst(actor, merchant_14)))).
st(root, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))).
st(root, state(knows_gossip, inst(actor, merchant_14))).
at(root, potential(opportunity, state(knows_gossip, inst(actor, merchant_14)))).
at(root, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
spontaneous(st(root, property(type, inst(actor, merchant_14), merchant))).
spontaneous(st(root, property(type, inst(actor, aristocrat_16), noble))).
spontaneous(st(root, property(type, inst(actor, laborer_17), peasant))).
st(root, property(type, inst(actor, merchant_14), merchant)).
st(root, property(type, inst(actor, aristocrat_16), noble)).
st(root, property(type, inst(actor, laborer_17), peasant)).
at(root, consequence_of(option(2), o(onwards, onwards), _not, state(knows_gossip, inst(actor, merchant_14)))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(type, inst(actor, merchant_14), merchant))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(type, inst(actor, aristocrat_16), noble))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(type, inst(actor, laborer_17), peasant))).
at(root, action(option(2), travel_onwards)).
at(root, consequence_of(option(2), o(onwards, onwards), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, inst(actor, merchant_14))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, inst(actor, aristocrat_16))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, inst(actor, laborer_17))).
at(root, outcome(option(2), o(onwards, onwards))).
at(root, consequence(option(2), _not, state(knows_gossip, inst(actor, merchant_14)))).
at(root, consequence(option(2), _not, property(type, inst(actor, merchant_14), merchant))).
at(root, consequence(option(2), _not, property(type, inst(actor, aristocrat_16), noble))).
at(root, consequence(option(2), _not, property(type, inst(actor, laborer_17), peasant))).
at(root, consequence(option(2), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence(option(2), _not, inst(actor, merchant_14))).
at(root, consequence(option(2), _not, inst(actor, aristocrat_16))).
at(root, consequence(option(2), _not, inst(actor, laborer_17))).
at(root, consequence(option(2), nullifies, potential(opportunity, state(knows_gossip, inst(actor, merchant_14))))).
at(root, consequence(option(2), nullifies, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, consequence_of(option(2), o(onwards, onwards), nullifies, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, consequence_of(option(2), o(onwards, onwards), nullifies, potential(opportunity, state(knows_gossip, inst(actor, merchant_14))))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_14), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, aristocrat_16), literacy))).
st(root, property(has_skill, inst(actor, merchant_14), literacy)).
st(root, property(has_skill, inst(actor, aristocrat_16), literacy)).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(has_skill, inst(actor, merchant_14), literacy))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(has_skill, inst(actor, aristocrat_16), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_14), tinkering))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_14), music))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_14), storytelling))).
st(root, property(has_skill, inst(actor, merchant_14), tinkering)).
st(root, property(has_skill, inst(actor, merchant_14), music)).
st(root, property(has_skill, inst(actor, merchant_14), storytelling)).
at(root, arg(option(1), target, inst(actor, aristocrat_16))).
at(root, arg(option(3), accuser, inst(actor, aristocrat_16))).
at(root, arg(option(3), victim, inst(actor, merchant_14))).
at(root, consequence_of(option(1), o(success, cursed), property(polymorphed, inst(actor, aristocrat_16), noble))).
at(root, consequence_of(option(3), o(success, exonerated), resolves, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, action(option(3), deny_blame)).
at(root, consequence(option(2), _not, property(has_skill, inst(actor, merchant_14), literacy))).
at(root, consequence(option(2), _not, property(has_skill, inst(actor, aristocrat_16), literacy))).
at(root, consequence_of(option(3), o(success, exonerated), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(has_skill, inst(actor, merchant_14), tinkering))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(has_skill, inst(actor, merchant_14), music))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(has_skill, inst(actor, merchant_14), storytelling))).
at(root, consequence_of(option(1), o(success, cursed), nullifies, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, consequence_of(option(1), o(success, cursed), property(has_skill, inst(actor, aristocrat_16), unintelligent))).
spontaneous(st(root, property(has_skill, inst(actor, aristocrat_16), elocution))).
st(root, property(has_skill, inst(actor, aristocrat_16), elocution)).
unresolved_potential(root, option(1), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
unresolved_potential(root, option(3), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence_of(option(1), o(success, cursed), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence(option(2), _not, property(has_skill, inst(actor, merchant_14), tinkering))).
at(root, consequence(option(2), _not, property(has_skill, inst(actor, merchant_14), music))).
at(root, consequence(option(2), _not, property(has_skill, inst(actor, merchant_14), storytelling))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(has_skill, inst(actor, aristocrat_16), elocution))).
spontaneous(st(root, property(type, inst(item, spear_57), spear))).
spontaneous(st(root, property(type, inst(item, sack_of_gold_70), sack_of_gold))).
spontaneous(st(root, property(type, inst(item, medicine_book_74), medicine_book))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_14), inst(item, spear_57)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, medicine_book_74)))).
spontaneous(st(root, inst(item, spear_57))).
spontaneous(st(root, inst(item, sack_of_gold_70))).
spontaneous(st(root, inst(item, medicine_book_74))).
st(root, property(type, inst(item, spear_57), spear)).
st(root, property(type, inst(item, sack_of_gold_70), sack_of_gold)).
st(root, property(type, inst(item, medicine_book_74), medicine_book)).
st(root, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70))).
st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, medicine_book_74))).
st(root, inst(item, spear_57)).
st(root, inst(item, sack_of_gold_70)).
st(root, inst(item, medicine_book_74)).
at(root, consequence_of(option(1), o(success, cursed), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70)))).
at(root, consequence_of(option(1), o(success, cursed), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, medicine_book_74)))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, relation(has_item, inst(actor, merchant_14), inst(item, spear_57)))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70)))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, medicine_book_74)))).
at(root, consequence(option(2), _not, property(has_skill, inst(actor, aristocrat_16), elocution))).
at(root, consequence(option(2), _not, relation(has_item, inst(actor, merchant_14), inst(item, spear_57)))).
at(root, consequence(option(2), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70)))).
at(root, consequence(option(2), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, medicine_book_74)))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(type, inst(item, spear_57), spear))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(type, inst(item, sack_of_gold_70), sack_of_gold))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, property(type, inst(item, medicine_book_74), medicine_book))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, inst(item, spear_57))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, inst(item, sack_of_gold_70))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, inst(item, medicine_book_74))).
at(root, consequence(option(2), _not, property(type, inst(item, spear_57), spear))).
at(root, consequence(option(2), _not, property(type, inst(item, sack_of_gold_70), sack_of_gold))).
at(root, consequence(option(2), _not, property(type, inst(item, medicine_book_74), medicine_book))).
at(root, consequence(option(2), _not, inst(item, spear_57))).
at(root, consequence(option(2), _not, inst(item, sack_of_gold_70))).
at(root, consequence(option(2), _not, inst(item, medicine_book_74))).
spontaneous(st(root, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
st(root, relation(selling, inst(actor, merchant_14), inst(item, spear_57))).
at(root, consequence_of(option(2), o(onwards, onwards), _not, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
at(root, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
at(root, consequence(option(2), _not, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
at(root, consequence_of(option(2), o(onwards, onwards), nullifies, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57))))).
at(root, consequence(option(2), nullifies, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57))))).
at(root, category(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), urgent)).
at(root, category(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), immediate)).
at(root, arg(option(2), subject, inst(actor, you))).
at(root, arg(option(1), caster, inst(actor, you))).
at(root, arg(option(3), denier, inst(actor, you))).
surface_property(name, inst(item, sack_of_gold_23), "sack of gold").
surface_property(name, inst(item, oboe_35), "oboe").
surface_property(name, inst(item, plants_book_26), "book of herbal lore").
surface_property(name, inst(actor, merchant_14), "merchant").
surface_property(name, inst(actor, laborer_17), "peasant").
surface_property(name, inst(actor, aristocrat_16), "noble").
st(root, property(has_skill, inst(actor, you), wilderness_lore)).
st(root, property(has_skill, inst(actor, you), music)).
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
at(root, has_tool_for(inst(actor, you), wilderness_lore)).
at(root, has_tool_for(inst(actor, merchant_14), fighting)).
at(root, has_tool_for(inst(actor, aristocrat_16), healing)).
setting(root, road).
at(root, skill_link(wilderness_lore, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(wilderness_lore, contest, no_tool, flee, between(fearful, from), either(o(get_injured, safe), o(get_injured, injured)))).
at(root, likely_outcome(option(3), o(success, ignored))).
at(root, likely_outcome(option(1), o(success, no_effect))).
at(root, unlikely_outcome(option(1), o(success, cursed))).
at(root, unlikely_outcome(option(3), o(success, exonerated))).
at(root, outcome_predictability(option(1), o(success, no_effect), predictable)).
at(root, outcome(option(1), o(success, no_effect))).
at(root, outcome_predictability(option(3), o(success, ignored), predictable)).
at(root, outcome(option(3), o(success, ignored))).
at(root, likely_outcome(option(2), o(onwards, onwards))).
at(root, outcome_predictability(option(2), o(onwards, onwards), predictable)).
at(root, possible_outcome_perception(option(3), o(success, exonerated), advances, avoid_accusations(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(1), o(success, cursed), advances, avoid_accusations(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(1), o(success, cursed), fails, preserve_original_form(inst(actor, aristocrat_16)))).
at(root, possible_outcome_perception(option(2), o(onwards, onwards), fails, avoid_accusations(inst(actor, merchant_14)))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(avoid_accusations(inst(actor, you)))).
at(root, player_goal(preserve_original_form(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), wilderness_lore))).
at(root, player_goal(have_tool_for(inst(actor, you), music))).
at(root, player_goal(have_tool_for(inst(actor, you), sorcery))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, guilty(inst(actor, aristocrat_16))).
at(root, player_goal(avoid_accusations(inst(actor, merchant_14)))).
at(root, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root, goal_stakes(preserve_health(inst(actor, merchant_14)), high)).
at(root, goal_stakes(preserve_health(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(preserve_health(inst(actor, laborer_17)), high)).
at(root, goal_stakes(as_intended(inst(actor, you)), low)).
at(root, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, merchant_14)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(preserve_original_form(inst(actor, laborer_17)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, merchant_14)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(avoid_accusations(inst(actor, laborer_17)), high)).
at(root, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, merchant_14), tinkering), low)).
at(root, goal_stakes(have_tool_for(inst(actor, merchant_14), music), low)).
at(root, goal_stakes(have_tool_for(inst(actor, merchant_14), storytelling), low)).
at(root, goal_stakes(have_tool_for(inst(actor, aristocrat_16), elocution), low)).
at(root, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, laborer_17)), high)).
at(root, outcome_perception(option(1), hinders, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(3), hinders, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(2), fails, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(1), fails, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), fails, as_intended(inst(actor, you)))).
at(root, outcome_stakes(option(1), high)).
at(root, outcome_stakes(option(2), high)).
at(root, outcome_stakes(option(3), high)).
at(root, important_outcome(option(2), o(onwards, onwards))).
at(root, important_outcome(option(1), o(success, no_effect))).
at(root, important_outcome(option(3), o(success, ignored))).
at(root, overall_predictability(option(1), predictable)).
at(root, overall_predictability(option(2), predictable)).
at(root, overall_predictability(option(3), predictable)).
at(root, relevant_to(option(1), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, relevant_to(option(3), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(1, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(2, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(3, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(4, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(8, potential(opportunity, state(knows_gossip, inst(actor, merchant_14))))).
at(root, importance(8, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57))))).
at(root, some_importance(1)).
at(root, some_importance(2)).
at(root, some_importance(3)).
at(root, some_importance(4)).
at(root, some_importance(8)).
at(root, most_important(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, problematic_for(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), inst(actor, merchant_14))).
at(root, motivated(option(1))).
at(root, motivated(option(2))).
at(root, motivated(option(3))).
at(root, expectation(option(1), enables, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(3), enables, avoid_accusations(inst(actor, merchant_14)))).
at(root, initiated_by(potential(opportunity, state(knows_gossip, inst(actor, merchant_14))), inst(actor, merchant_14))).
at(root, initiated_by(potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57))), inst(actor, merchant_14))).
at(root, initiated_by(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), inst(actor, aristocrat_16))).
surface_property(number, inst(item, sack_of_gold_23), singular).
surface_property(number, inst(item, oboe_35), singular).
surface_property(number, inst(item, plants_book_26), singular).
at(root, can_trade(inst(actor, you), inst(item, sack_of_gold_23))).
at(root, can_trade(inst(actor, you), inst(item, plants_book_26))).
at(root, can_trade(inst(actor, you), inst(item, oboe_35))).
at(root, can_trade(inst(actor, merchant_14), inst(item, spear_57))).
at(root, can_trade(inst(actor, aristocrat_16), inst(item, sack_of_gold_70))).
at(root, can_trade(inst(actor, aristocrat_16), inst(item, medicine_book_74))).
at(root, setup_arg(goods_one, inst(item, spear_57))).
at(root, setup_arg(goods_two, inst(item, plants_book_26))).
at(root, setup_arg(goods_two, inst(item, sack_of_gold_70))).
at(root, expectation(option(1), threatens, preserve_original_form(inst(actor, aristocrat_16)))).
at(root, expectation(option(1), fails, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(2), fails, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(3), fails, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(1), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(3), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, as_intended(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), sorcery))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, perceived_option_stakes(option(1), high)).
at(root, perceived_option_stakes(option(2), high)).
at(root, perceived_option_stakes(option(3), high)).
at(root, perceived_stakes(high)).
at(root, option_feel(option(1), longshot)).
at(root, option_feel(option(3), longshot)).
at(root, option_feel(option(2), bad)).
at(root, option_structure(negative_alternatives)).
at(root, option_feel(option(2), doomed)).
at(root, outcome_overall(option(1), bad)).
at(root, outcome_overall(option(3), bad)).
at(root, outcome_overall(option(2), awful)).
at(root, outcome_feel(option(1), expected_failure)).
at(root, outcome_feel(option(2), expected_failure)).
at(root, outcome_feel(option(3), expected_failure)).
at(root, relevant_skill(option(1), inst(actor, you), has, sorcery)).
at(root, relevant_skill(option(3), inst(actor, aristocrat_16), has, elocution)).
at(root, relevant_skill(option(3), inst(actor, you), missing, elocution)).
at(root, relevant_tool(option(1), inst(actor, you), missing, sorcery)).
at(root, is_powerful(inst(actor, aristocrat_16))).
at(root, is_powerless(inst(actor, merchant_14))).
at(root, is_powerless(inst(actor, laborer_17))).
at(root, actual_stakes(high)).
at(root, option_structure(uncomfortable)).
at(root, option_structure(bleak)).
unresolved_potential(root, option(1), potential(opportunity, state(knows_gossip, inst(actor, merchant_14)))).
unresolved_potential(root, option(1), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
unresolved_potential(root, option(3), potential(opportunity, state(knows_gossip, inst(actor, merchant_14)))).
unresolved_potential(root, option(3), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, spear_57)))).
surface_property(name, inst(item, spear_57), "spear").
surface_property(name, inst(item, sack_of_gold_70), "sack of gold").
surface_property(name, inst(item, medicine_book_74), "book of medicine").
surface_property(person, inst(item, sack_of_gold_23), third).
surface_property(person, inst(item, plants_book_26), third).
surface_property(person, inst(actor, merchant_14), third).
surface_property(person, inst(actor, aristocrat_16), third).
surface_property(person, inst(actor, laborer_17), third).
surface_property(person, inst(item, oboe_35), third).
surface_property(person, inst(item, spear_57), third).
surface_property(person, inst(item, sack_of_gold_70), third).
surface_property(person, inst(item, medicine_book_74), third).
surface_property(number, inst(actor, merchant_14), singular).
surface_property(number, inst(actor, aristocrat_16), singular).
surface_property(number, inst(actor, laborer_17), singular).
surface_property(number, inst(item, spear_57), singular).
surface_property(number, inst(item, sack_of_gold_70), singular).
surface_property(number, inst(item, medicine_book_74), singular).
surface_property(gender, inst(item, sack_of_gold_23), neuter).
surface_property(gender, inst(item, plants_book_26), neuter).
surface_property(gender, inst(actor, merchant_14), feminine).
surface_property(gender, inst(actor, aristocrat_16), masculine).
surface_property(gender, inst(actor, laborer_17), masculine).
surface_property(gender, inst(item, oboe_35), neuter).
surface_property(gender, inst(item, spear_57), neuter).
surface_property(gender, inst(item, sack_of_gold_70), neuter).
surface_property(gender, inst(item, medicine_book_74), neuter).
surface_property(determined, inst(item, sack_of_gold_23), true).
surface_property(determined, inst(item, plants_book_26), true).
surface_property(determined, inst(actor, merchant_14), true).
surface_property(determined, inst(actor, aristocrat_16), true).
surface_property(determined, inst(actor, laborer_17), true).
surface_property(determined, inst(item, oboe_35), true).
surface_property(determined, inst(item, spear_57), true).
surface_property(determined, inst(item, sack_of_gold_70), true).
surface_property(determined, inst(item, medicine_book_74), true).
successor(root, option(1), root_1).
successor(root, option(2), root_2).
story_node(root_1).
story_node(root_2).
st(root_1, property(type, inst(actor, you), person)).
st(root_2, property(type, inst(actor, you), person)).
successor(root, option(3), root_1).
st(root_1, property(has_skill, inst(actor, you), literacy)).
st(root_2, property(has_skill, inst(actor, you), literacy)).
st(root_1, inst(actor, you)).
st(root_2, inst(actor, you)).
st(root_1, state(party_member, inst(actor, you))).
st(root_2, state(party_member, inst(actor, you))).
st(root_1, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root_2, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root_1, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_2, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_1, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root_2, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root_1, property(type, inst(item, plants_book_26), plants_book)).
st(root_2, property(type, inst(item, plants_book_26), plants_book)).
st(root_1, inst(item, sack_of_gold_23)).
st(root_2, inst(item, sack_of_gold_23)).
st(root_1, inst(item, plants_book_26)).
st(root_2, inst(item, plants_book_26)).
st(root_1, inst(actor, merchant_14)).
st(root_1, inst(actor, aristocrat_16)).
st(root_1, inst(actor, laborer_17)).
st(root_1, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_2, relation(has_item, inst(actor, you), inst(item, oboe_35))).
st(root_1, property(type, inst(item, oboe_35), oboe)).
st(root_2, property(type, inst(item, oboe_35), oboe)).
st(root_1, inst(item, oboe_35)).
st(root_2, inst(item, oboe_35)).
st(root_1, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))).
st(root_1, state(knows_gossip, inst(actor, merchant_14))).
st(root_1, property(type, inst(actor, merchant_14), merchant)).
st(root_1, property(type, inst(actor, aristocrat_16), noble)).
st(root_1, property(type, inst(actor, laborer_17), peasant)).
st(root_1, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_1, property(has_skill, inst(actor, aristocrat_16), literacy)).
st(root_1, property(has_skill, inst(actor, merchant_14), tinkering)).
st(root_1, property(has_skill, inst(actor, merchant_14), music)).
st(root_1, property(has_skill, inst(actor, merchant_14), storytelling)).
st(root_1, property(has_skill, inst(actor, aristocrat_16), elocution)).
st(root_1, property(type, inst(item, spear_57), spear)).
st(root_1, property(type, inst(item, sack_of_gold_70), sack_of_gold)).
st(root_1, property(type, inst(item, medicine_book_74), medicine_book)).
st(root_1, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root_1, relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_70))).
st(root_1, relation(has_item, inst(actor, aristocrat_16), inst(item, medicine_book_74))).
st(root_1, inst(item, spear_57)).
st(root_1, inst(item, sack_of_gold_70)).
st(root_1, inst(item, medicine_book_74)).
st(root_1, relation(selling, inst(actor, merchant_14), inst(item, spear_57))).
st(root_1, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_2, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_1, property(has_skill, inst(actor, you), music)).
st(root_2, property(has_skill, inst(actor, you), music)).
st(root_1, property(has_skill, inst(actor, you), sorcery)).
st(root_2, property(has_skill, inst(actor, you), sorcery)).
node_status_reached(root_1, uninitialized).
node_status_reached(root_2, uninitialized).
node_status_reached(root, branched).
node_status_reached(root, polished).
node_status(root_1, uninitialized).
node_status(root_2, uninitialized).
node_status(root, polished).
at(root_1, guilty(inst(actor, aristocrat_16))).
at(root_1, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_1, goal_stakes(preserve_health(inst(actor, merchant_14)), high)).
at(root_1, goal_stakes(preserve_health(inst(actor, aristocrat_16)), high)).
at(root_1, goal_stakes(preserve_health(inst(actor, laborer_17)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, merchant_14)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, aristocrat_16)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, laborer_17)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, merchant_14)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, aristocrat_16)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, laborer_17)), high)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, merchant_14), tinkering), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, merchant_14), music), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, merchant_14), storytelling), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, aristocrat_16), elocution), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), music), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), sorcery), low)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, laborer_17)), high)).
path_length(root_1, 1).
path_length(root_2, 1).
at(root_1, perceived_stakes(none)).
at(root_2, perceived_stakes(none)).
at(root_1, is_powerful(inst(actor, aristocrat_16))).
at(root_1, is_powerless(inst(actor, merchant_14))).
at(root_1, is_powerless(inst(actor, laborer_17))).
at(root_1, text_mode(example)).
at(root_2, text_mode(example)).
max_unique(74).
