surface_property(name, inst(actor, you), "Dunyazad").
surface_property(person, inst(actor, you), second).
surface_property(gender, inst(actor, you), feminine).
surface_property(number, inst(actor, you), singular).
surface_property(determined, inst(actor, you), false).
seed(95923).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
at(root, perceived_stakes(none)).
setup_priority(on_sale, 6).
setup_priority(tavern, 4).
setup_priority(threatened_innocents, 2).
setup_priority(market, 5).
setup_priority(monster_attack, 1).
setup_priority(healer, 3).
action_priority(attack, 18).
action_priority(shift_blame, 11).
action_priority(steal, 10).
action_priority(flee, 16).
action_priority(tell_story, 17).
action_priority(talk_down, 6).
action_priority(pay_off, 7).
action_priority(accuse, 12).
action_priority(deny_blame, 20).
action_priority(trade, 4).
action_priority(reach_destination, 14).
action_priority(play_song, 19).
action_priority(treat_injury, 15).
action_priority(dispel, 1).
action_priority(polymorph, 5).
action_priority(gossip, 8).
action_priority(explain_innocence, 13).
action_priority(buy_healing, 9).
action_priority(pacify, 3).
action_priority(travel_onwards, 2).
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
st(root, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root, property(type, inst(item, plants_book_26), plants_book)).
st(root, inst(item, sack_of_gold_23)).
st(root, inst(item, plants_book_26)).
spontaneous(st(root, inst(actor, merchant_14))).
spontaneous(st(root, inst(actor, aristocrat_16))).
spontaneous(st(root, inst(actor, bad_guy_19))).
st(root, inst(actor, merchant_14)).
st(root, inst(actor, aristocrat_16)).
st(root, inst(actor, bad_guy_19)).
at(root, action(option(1), polymorph)).
at(root, outcome(option(1), o(success, cursed))).
spontaneous(st(root, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
spontaneous(st(root, state(knows_gossip, inst(actor, bad_guy_19)))).
st(root, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))).
st(root, state(knows_gossip, inst(actor, bad_guy_19))).
at(root, potential(opportunity, state(knows_gossip, inst(actor, bad_guy_19)))).
at(root, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
spontaneous(st(root, property(type, inst(actor, merchant_14), merchant))).
spontaneous(st(root, property(type, inst(actor, aristocrat_16), noble))).
spontaneous(st(root, property(type, inst(actor, bad_guy_19), thief))).
st(root, property(type, inst(actor, merchant_14), merchant)).
st(root, property(type, inst(actor, aristocrat_16), noble)).
st(root, property(type, inst(actor, bad_guy_19), thief)).
at(root, arg(option(2), price, inst(item, sack_of_gold_23))).
at(root, action(option(2), pay_off)).
at(root, consequence(option(1), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence(option(3), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence(option(1), nullifies, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, consequence_of(option(2), o(deal, deal), _not, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23)))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_14), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, aristocrat_16), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, bad_guy_19), thievery))).
st(root, property(has_skill, inst(actor, merchant_14), literacy)).
st(root, property(has_skill, inst(actor, aristocrat_16), literacy)).
st(root, property(has_skill, inst(actor, bad_guy_19), thievery)).
at(root, arg(option(1), target, inst(actor, aristocrat_16))).
at(root, arg(option(2), victim, inst(actor, merchant_14))).
at(root, arg(option(3), victim, inst(actor, merchant_14))).
at(root, consequence_of(option(1), o(success, cursed), property(polymorphed, inst(actor, aristocrat_16), noble))).
at(root, consequence_of(option(2), o(deal, deal), relation(has_item, inst(actor, aristocrat_16), inst(item, sack_of_gold_23)))).
at(root, arg(option(2), listening, inst(actor, aristocrat_16))).
at(root, consequence_of(option(2), o(deal, deal), resolves, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, arg(option(3), listening, inst(actor, aristocrat_16))).
at(root, action(option(3), talk_down)).
at(root, consequence_of(option(3), o(attitude, convinced), resolves, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, arg(option(2), asking, inst(actor, you))).
at(root, arg(option(3), asking, inst(actor, you))).
at(root, consequence_of(option(3), o(enraged, enraged), relation(accusing, inst(actor, aristocrat_16), inst(actor, you)))).
at(root, consequence(option(1), property(polymorphed, inst(actor, aristocrat_16), noble))).
at(root, consequence(option(3), resolves, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, outcome(option(3), o(attitude, convinced))).
at(root, consequence_of(option(2), o(deal, deal), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence_of(option(3), o(attitude, convinced), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence_of(option(1), o(success, cursed), nullifies, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, consequence_of(option(1), o(success, cursed), property(has_skill, inst(actor, aristocrat_16), unintelligent))).
unresolved_potential(root, option(2), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
unresolved_potential(root, option(1), potential(problem, property(polymorphed, inst(actor, aristocrat_16), noble))).
at(root, consequence_of(option(1), o(success, cursed), _not, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14)))).
at(root, consequence(option(1), property(has_skill, inst(actor, aristocrat_16), unintelligent))).
spontaneous(st(root, property(type, inst(item, spear_57), spear))).
spontaneous(st(root, property(type, inst(item, perfume_71), perfume))).
spontaneous(st(root, property(type, inst(item, ancient_grimoire_73), ancient_grimoire))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_14), inst(item, spear_57)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, perfume_71)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, ancient_grimoire_73)))).
spontaneous(st(root, inst(item, spear_57))).
spontaneous(st(root, inst(item, perfume_71))).
spontaneous(st(root, inst(item, ancient_grimoire_73))).
st(root, property(type, inst(item, spear_57), spear)).
st(root, property(type, inst(item, perfume_71), perfume)).
st(root, property(type, inst(item, ancient_grimoire_73), ancient_grimoire)).
st(root, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, perfume_71))).
st(root, relation(has_item, inst(actor, aristocrat_16), inst(item, ancient_grimoire_73))).
st(root, inst(item, spear_57)).
st(root, inst(item, perfume_71)).
st(root, inst(item, ancient_grimoire_73)).
at(root, consequence_of(option(1), o(success, cursed), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, perfume_71)))).
at(root, consequence_of(option(1), o(success, cursed), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, ancient_grimoire_73)))).
at(root, consequence(option(1), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, perfume_71)))).
at(root, consequence(option(1), _not, relation(has_item, inst(actor, aristocrat_16), inst(item, ancient_grimoire_73)))).
at(root, category(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), urgent)).
at(root, category(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), immediate)).
at(root, arg(option(1), caster, inst(actor, you))).
surface_property(name, inst(item, sack_of_gold_23), "sack of gold").
surface_property(name, inst(item, plants_book_26), "book of herbal lore").
surface_property(name, inst(actor, merchant_14), "merchant").
surface_property(name, inst(actor, aristocrat_16), "noble").
st(root, property(has_skill, inst(actor, you), tinkering)).
st(root, property(has_skill, inst(actor, you), elocution)).
st(root, property(has_skill, inst(actor, you), healing)).
at(root, outcome(option(2), o(deal, no_deal))).
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
at(root, likely_outcome(option(2), o(deal, deal))).
at(root, has_tool_for(inst(actor, you), healing)).
at(root, has_tool_for(inst(actor, you), wilderness_lore)).
at(root, has_tool_for(inst(actor, merchant_14), fighting)).
at(root, has_tool_for(inst(actor, aristocrat_16), sorcery)).
setting(root, town).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(get_injured, safe), o(get_injured, injured)))).
at(root, likely_outcome(option(1), o(success, no_effect))).
at(root, unlikely_outcome(option(1), o(success, cursed))).
at(root, outcome(option(3), o(enraged, not_enraged))).
at(root, unlikely_outcome(option(2), o(deal, no_deal))).
at(root, outcome_predictability(option(3), o(attitude, convinced), unpredictable)).
at(root, outcome_predictability(option(3), o(enraged, not_enraged), unpredictable)).
at(root, neutral_outcome(option(3), o(attitude, convinced))).
at(root, neutral_outcome(option(3), o(attitude, unconvinced))).
at(root, neutral_outcome(option(3), o(enraged, enraged))).
at(root, neutral_outcome(option(3), o(enraged, not_enraged))).
at(root, outcome_predictability(option(1), o(success, cursed), unfair)).
at(root, outcome_predictability(option(2), o(deal, no_deal), unfair)).
at(root, possible_outcome_perception(option(3), o(enraged, enraged), hinders, avoid_accusations(inst(actor, you)))).
at(root, possible_outcome_perception(option(2), o(deal, deal), advances, avoid_accusations(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(3), o(attitude, convinced), advances, avoid_accusations(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(1), o(success, cursed), advances, avoid_accusations(inst(actor, merchant_14)))).
at(root, possible_outcome_perception(option(1), o(success, cursed), fails, preserve_original_form(inst(actor, aristocrat_16)))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(avoid_accusations(inst(actor, you)))).
at(root, player_goal(preserve_original_form(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), tinkering))).
at(root, player_goal(have_tool_for(inst(actor, you), elocution))).
at(root, player_goal(have_tool_for(inst(actor, you), healing))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, guilty(inst(actor, aristocrat_16))).
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
at(root, goal_stakes(have_tool_for(inst(actor, you), tinkering), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), elocution), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), healing), low)).
at(root, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root, goal_stakes(have_tool_for(inst(actor, bad_guy_19), thievery), low)).
at(root, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root, goal_stakes(avoid_threats_to(inst(actor, bad_guy_19)), high)).
at(root, outcome_perception(option(2), hinders, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(1), advances, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(3), advances, avoid_accusations(inst(actor, merchant_14)))).
at(root, outcome_perception(option(1), fails, preserve_original_form(inst(actor, aristocrat_16)))).
at(root, outcome_perception(option(1), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(2), fails, as_intended(inst(actor, you)))).
at(root, outcome_stakes(option(1), high)).
at(root, outcome_stakes(option(2), high)).
at(root, outcome_stakes(option(3), high)).
at(root, important_outcome(option(3), o(attitude, convinced))).
at(root, important_outcome(option(1), o(success, cursed))).
at(root, important_outcome(option(3), o(enraged, not_enraged))).
at(root, important_outcome(option(2), o(deal, no_deal))).
at(root, overall_predictability(option(3), unpredictable)).
at(root, overall_predictability(option(1), unfair)).
at(root, overall_predictability(option(2), unfair)).
at(root, relevant_to(option(1), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, relevant_to(option(2), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, relevant_to(option(3), potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(1, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(2, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(3, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(4, potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))))).
at(root, importance(8, potential(opportunity, state(knows_gossip, inst(actor, bad_guy_19))))).
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
at(root, expectation(option(2), enables, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(3), enables, avoid_accusations(inst(actor, merchant_14)))).
at(root, initiated_by(potential(opportunity, state(knows_gossip, inst(actor, bad_guy_19))), inst(actor, bad_guy_19))).
at(root, initiated_by(potential(problem, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))), inst(actor, aristocrat_16))).
surface_property(number, inst(item, sack_of_gold_23), singular).
surface_property(number, inst(item, plants_book_26), singular).
at(root, can_trade(inst(actor, you), inst(item, sack_of_gold_23))).
at(root, can_trade(inst(actor, you), inst(item, plants_book_26))).
at(root, can_trade(inst(actor, merchant_14), inst(item, spear_57))).
at(root, can_trade(inst(actor, aristocrat_16), inst(item, perfume_71))).
at(root, can_trade(inst(actor, aristocrat_16), inst(item, ancient_grimoire_73))).
at(root, setup_arg(goods_two, inst(item, plants_book_26))).
at(root, setup_arg(goods_two, inst(item, perfume_71))).
at(root, setup_arg(goods_two, inst(item, ancient_grimoire_73))).
at(root, expectation(option(3), threatens, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(1), threatens, preserve_original_form(inst(actor, aristocrat_16)))).
at(root, expectation(option(2), achieves, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(2), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(1), fails, avoid_accusations(inst(actor, merchant_14)))).
at(root, expectation(option(1), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), tinkering))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_accusations(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), tinkering))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, preserve_original_form(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, as_intended(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), tinkering))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, perceived_option_stakes(option(1), high)).
at(root, perceived_option_stakes(option(2), high)).
at(root, perceived_option_stakes(option(3), high)).
at(root, perceived_stakes(high)).
at(root, option_feel(option(2), safe)).
at(root, option_structure(obvious)).
at(root, option_feel(option(2), sure_thing)).
at(root, outcome_overall(option(2), bad)).
at(root, outcome_feel(option(2), unfair)).
at(root, option_feel(option(1), longshot)).
at(root, outcome_overall(option(1), good)).
at(root, outcome_overall(option(3), good)).
at(root, outcome_feel(option(1), miracle)).
at(root, option_feel(option(3), risky)).
at(root, outcome_feel(option(3), nice_gamble)).
at(root, relevant_skill(option(2), inst(actor, you), has, elocution)).
at(root, relevant_skill(option(3), inst(actor, you), has, elocution)).
at(root, relevant_skill(option(1), inst(actor, you), missing, sorcery)).
at(root, surprising(option(1))).
at(root, surprising(option(2))).
at(root, is_powerful(inst(actor, aristocrat_16))).
at(root, is_powerful(inst(actor, bad_guy_19))).
at(root, is_powerless(inst(actor, merchant_14))).
at(root, actual_stakes(high)).
unresolved_potential(root, option(1), potential(opportunity, state(knows_gossip, inst(actor, bad_guy_19)))).
unresolved_potential(root, option(2), potential(opportunity, state(knows_gossip, inst(actor, bad_guy_19)))).
unresolved_potential(root, option(3), potential(opportunity, state(knows_gossip, inst(actor, bad_guy_19)))).
surface_property(name, inst(actor, bad_guy_19), "thief").
surface_property(name, inst(item, spear_57), "spear").
surface_property(name, inst(item, perfume_71), "perfume").
surface_property(name, inst(item, ancient_grimoire_73), "ancient grimoire").
surface_property(person, inst(item, sack_of_gold_23), third).
surface_property(person, inst(item, plants_book_26), third).
surface_property(person, inst(actor, merchant_14), third).
surface_property(person, inst(actor, aristocrat_16), third).
surface_property(person, inst(actor, bad_guy_19), third).
surface_property(person, inst(item, spear_57), third).
surface_property(person, inst(item, perfume_71), third).
surface_property(person, inst(item, ancient_grimoire_73), third).
surface_property(number, inst(actor, merchant_14), singular).
surface_property(number, inst(actor, aristocrat_16), singular).
surface_property(number, inst(actor, bad_guy_19), singular).
surface_property(number, inst(item, spear_57), singular).
surface_property(number, inst(item, perfume_71), plural).
surface_property(number, inst(item, ancient_grimoire_73), singular).
surface_property(gender, inst(item, sack_of_gold_23), neuter).
surface_property(gender, inst(item, plants_book_26), neuter).
surface_property(gender, inst(actor, merchant_14), masculine).
surface_property(gender, inst(actor, aristocrat_16), masculine).
surface_property(gender, inst(actor, bad_guy_19), feminine).
surface_property(gender, inst(item, spear_57), neuter).
surface_property(gender, inst(item, perfume_71), neuter).
surface_property(gender, inst(item, ancient_grimoire_73), neuter).
surface_property(determined, inst(item, sack_of_gold_23), true).
surface_property(determined, inst(item, plants_book_26), true).
surface_property(determined, inst(actor, merchant_14), true).
surface_property(determined, inst(actor, aristocrat_16), true).
surface_property(determined, inst(actor, bad_guy_19), true).
surface_property(determined, inst(item, spear_57), true).
surface_property(determined, inst(item, perfume_71), true).
surface_property(determined, inst(item, ancient_grimoire_73), true).
successor(root, option(1), root_1).
successor(root, option(2), root_2).
successor(root, option(3), root_3).
story_node(root_1).
story_node(root_2).
story_node(root_3).
st(root_1, property(polymorphed, inst(actor, aristocrat_16), noble)).
st(root_1, property(has_skill, inst(actor, aristocrat_16), unintelligent)).
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
st(root_1, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root_2, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root_3, relation(has_item, inst(actor, you), inst(item, sack_of_gold_23))).
st(root_1, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_2, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_3, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_1, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root_2, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root_3, property(type, inst(item, sack_of_gold_23), sack_of_gold)).
st(root_1, property(type, inst(item, plants_book_26), plants_book)).
st(root_2, property(type, inst(item, plants_book_26), plants_book)).
st(root_3, property(type, inst(item, plants_book_26), plants_book)).
st(root_1, inst(item, sack_of_gold_23)).
st(root_2, inst(item, sack_of_gold_23)).
st(root_3, inst(item, sack_of_gold_23)).
st(root_1, inst(item, plants_book_26)).
st(root_2, inst(item, plants_book_26)).
st(root_3, inst(item, plants_book_26)).
st(root_1, inst(actor, merchant_14)).
st(root_2, inst(actor, merchant_14)).
st(root_3, inst(actor, merchant_14)).
st(root_1, inst(actor, aristocrat_16)).
st(root_2, inst(actor, aristocrat_16)).
st(root_3, inst(actor, aristocrat_16)).
st(root_1, inst(actor, bad_guy_19)).
st(root_2, inst(actor, bad_guy_19)).
st(root_3, inst(actor, bad_guy_19)).
st(root_2, relation(accusing, inst(actor, aristocrat_16), inst(actor, merchant_14))).
st(root_1, state(knows_gossip, inst(actor, bad_guy_19))).
st(root_2, state(knows_gossip, inst(actor, bad_guy_19))).
st(root_3, state(knows_gossip, inst(actor, bad_guy_19))).
st(root_1, property(type, inst(actor, merchant_14), merchant)).
st(root_2, property(type, inst(actor, merchant_14), merchant)).
st(root_3, property(type, inst(actor, merchant_14), merchant)).
st(root_1, property(type, inst(actor, aristocrat_16), noble)).
st(root_2, property(type, inst(actor, aristocrat_16), noble)).
st(root_3, property(type, inst(actor, aristocrat_16), noble)).
st(root_1, property(type, inst(actor, bad_guy_19), thief)).
st(root_2, property(type, inst(actor, bad_guy_19), thief)).
st(root_3, property(type, inst(actor, bad_guy_19), thief)).
st(root_1, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_2, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_3, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_1, property(has_skill, inst(actor, aristocrat_16), literacy)).
st(root_2, property(has_skill, inst(actor, aristocrat_16), literacy)).
st(root_3, property(has_skill, inst(actor, aristocrat_16), literacy)).
st(root_1, property(has_skill, inst(actor, bad_guy_19), thievery)).
st(root_2, property(has_skill, inst(actor, bad_guy_19), thievery)).
st(root_3, property(has_skill, inst(actor, bad_guy_19), thievery)).
st(root_1, property(type, inst(item, spear_57), spear)).
st(root_2, property(type, inst(item, spear_57), spear)).
st(root_3, property(type, inst(item, spear_57), spear)).
st(root_1, property(type, inst(item, perfume_71), perfume)).
st(root_2, property(type, inst(item, perfume_71), perfume)).
st(root_3, property(type, inst(item, perfume_71), perfume)).
st(root_1, property(type, inst(item, ancient_grimoire_73), ancient_grimoire)).
st(root_2, property(type, inst(item, ancient_grimoire_73), ancient_grimoire)).
st(root_3, property(type, inst(item, ancient_grimoire_73), ancient_grimoire)).
st(root_1, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root_2, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root_3, relation(has_item, inst(actor, merchant_14), inst(item, spear_57))).
st(root_2, relation(has_item, inst(actor, aristocrat_16), inst(item, perfume_71))).
st(root_3, relation(has_item, inst(actor, aristocrat_16), inst(item, perfume_71))).
st(root_2, relation(has_item, inst(actor, aristocrat_16), inst(item, ancient_grimoire_73))).
st(root_3, relation(has_item, inst(actor, aristocrat_16), inst(item, ancient_grimoire_73))).
st(root_1, inst(item, spear_57)).
st(root_2, inst(item, spear_57)).
st(root_3, inst(item, spear_57)).
st(root_1, inst(item, perfume_71)).
st(root_2, inst(item, perfume_71)).
st(root_3, inst(item, perfume_71)).
st(root_1, inst(item, ancient_grimoire_73)).
st(root_2, inst(item, ancient_grimoire_73)).
st(root_3, inst(item, ancient_grimoire_73)).
st(root_1, property(has_skill, inst(actor, you), tinkering)).
st(root_2, property(has_skill, inst(actor, you), tinkering)).
st(root_3, property(has_skill, inst(actor, you), tinkering)).
st(root_1, property(has_skill, inst(actor, you), elocution)).
st(root_2, property(has_skill, inst(actor, you), elocution)).
st(root_3, property(has_skill, inst(actor, you), elocution)).
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
at(root_2, guilty(inst(actor, aristocrat_16))).
at(root_1, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, you)), high)).
at(root_1, goal_stakes(preserve_health(inst(actor, merchant_14)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, merchant_14)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, merchant_14)), high)).
at(root_1, goal_stakes(preserve_health(inst(actor, aristocrat_16)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, aristocrat_16)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, aristocrat_16)), high)).
at(root_1, goal_stakes(preserve_health(inst(actor, bad_guy_19)), high)).
at(root_2, goal_stakes(preserve_health(inst(actor, bad_guy_19)), high)).
at(root_3, goal_stakes(preserve_health(inst(actor, bad_guy_19)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, you)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, merchant_14)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, merchant_14)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, merchant_14)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, aristocrat_16)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, aristocrat_16)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, aristocrat_16)), high)).
at(root_1, goal_stakes(preserve_original_form(inst(actor, bad_guy_19)), high)).
at(root_2, goal_stakes(preserve_original_form(inst(actor, bad_guy_19)), high)).
at(root_3, goal_stakes(preserve_original_form(inst(actor, bad_guy_19)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, you)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, merchant_14)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, merchant_14)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, merchant_14)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, aristocrat_16)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, aristocrat_16)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, aristocrat_16)), high)).
at(root_1, goal_stakes(avoid_accusations(inst(actor, bad_guy_19)), high)).
at(root_2, goal_stakes(avoid_accusations(inst(actor, bad_guy_19)), high)).
at(root_3, goal_stakes(avoid_accusations(inst(actor, bad_guy_19)), high)).
at(root_1, goal_stakes(have_tool_for(inst(actor, aristocrat_16), unintelligent), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), literacy), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, merchant_14), literacy), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, aristocrat_16), literacy), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, bad_guy_19), thievery), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, bad_guy_19), thievery), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, bad_guy_19), thievery), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), tinkering), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), tinkering), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), tinkering), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), elocution), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), elocution), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), elocution), low)).
at(root_1, goal_stakes(have_tool_for(inst(actor, you), healing), low)).
at(root_2, goal_stakes(have_tool_for(inst(actor, you), healing), low)).
at(root_3, goal_stakes(have_tool_for(inst(actor, you), healing), low)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, you)), high)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, merchant_14)), high)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, aristocrat_16)), high)).
at(root_1, goal_stakes(avoid_threats_to(inst(actor, bad_guy_19)), high)).
at(root_2, goal_stakes(avoid_threats_to(inst(actor, bad_guy_19)), high)).
at(root_3, goal_stakes(avoid_threats_to(inst(actor, bad_guy_19)), high)).
path_length(root_1, 1).
path_length(root_2, 1).
path_length(root_3, 1).
at(root_1, is_powerful(inst(actor, aristocrat_16))).
at(root_2, is_powerful(inst(actor, aristocrat_16))).
at(root_3, is_powerful(inst(actor, aristocrat_16))).
at(root_1, is_powerful(inst(actor, bad_guy_19))).
at(root_2, is_powerful(inst(actor, bad_guy_19))).
at(root_3, is_powerful(inst(actor, bad_guy_19))).
at(root_1, is_powerless(inst(actor, merchant_14))).
at(root_2, is_powerless(inst(actor, merchant_14))).
at(root_3, is_powerless(inst(actor, merchant_14))).
at(root_1, perceived_stakes(none)).
at(root_2, perceived_stakes(none)).
at(root_3, perceived_stakes(none)).
at(root_1, text_mode(example)).
at(root_2, text_mode(example)).
at(root_3, text_mode(example)).
max_unique(73).
