experiment_template(relaxed).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
setup_priority(on_sale, 4).
setup_priority(tavern, 1).
setup_priority(threatened_innocents, 5).
setup_priority(market, 3).
setup_priority(monster_attack, 6).
setup_priority(healer, 2).
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
setup(root, tavern).
at(root, setup_arg(merchant, inst(actor, merchant_6))).
at(root, setup_arg(noble, inst(actor, aristocrat_7))).
at(root, setup_arg(peasant, inst(actor, laborer_8))).
at(root, setup_arg(innkeeper, inst(actor, innkeeper_9))).
st(root, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root, relation(has_item, inst(actor, you), inst(item, speech_book_27))).
st(root, property(number, inst(item, perfume_24), plural)).
st(root, property(number, inst(item, speech_book_27), singular)).
st(root, property(name, inst(item, perfume_24), "perfume")).
st(root, property(name, inst(item, speech_book_27), "book of speeches")).
st(root, property(type, inst(item, perfume_24), perfume)).
st(root, property(type, inst(item, speech_book_27), speech_book)).
st(root, inst(item, perfume_24)).
st(root, inst(item, speech_book_27)).
spontaneous(st(root, inst(actor, merchant_6))).
spontaneous(st(root, inst(actor, aristocrat_7))).
spontaneous(st(root, inst(actor, laborer_8))).
spontaneous(st(root, inst(actor, innkeeper_9))).
st(root, inst(actor, merchant_6)).
st(root, inst(actor, aristocrat_7)).
st(root, inst(actor, laborer_8)).
st(root, inst(actor, innkeeper_9)).
spontaneous(st(root, state(bored, inst(actor, aristocrat_7)))).
spontaneous(st(root, state(bored, inst(actor, laborer_8)))).
st(root, state(bored, inst(actor, aristocrat_7))).
st(root, state(bored, inst(actor, laborer_8))).
at(root, potential(opportunity, state(bored, inst(actor, aristocrat_7)))).
at(root, potential(opportunity, state(bored, inst(actor, laborer_8)))).
spontaneous(st(root, property(type, inst(actor, merchant_6), merchant))).
spontaneous(st(root, property(type, inst(actor, aristocrat_7), noble))).
spontaneous(st(root, property(type, inst(actor, laborer_8), peasant))).
spontaneous(st(root, property(type, inst(actor, innkeeper_9), innkeeper))).
spontaneous(st(root, property(determined, inst(actor, merchant_6), true))).
spontaneous(st(root, property(determined, inst(actor, aristocrat_7), true))).
spontaneous(st(root, property(determined, inst(actor, laborer_8), true))).
spontaneous(st(root, property(determined, inst(actor, innkeeper_9), true))).
spontaneous(st(root, property(person, inst(actor, merchant_6), third))).
spontaneous(st(root, property(person, inst(actor, aristocrat_7), third))).
spontaneous(st(root, property(person, inst(actor, laborer_8), third))).
spontaneous(st(root, property(person, inst(actor, innkeeper_9), third))).
st(root, property(type, inst(actor, merchant_6), merchant)).
st(root, property(type, inst(actor, aristocrat_7), noble)).
st(root, property(type, inst(actor, laborer_8), peasant)).
st(root, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root, property(determined, inst(actor, merchant_6), true)).
st(root, property(determined, inst(actor, aristocrat_7), true)).
st(root, property(determined, inst(actor, laborer_8), true)).
st(root, property(determined, inst(actor, innkeeper_9), true)).
st(root, property(person, inst(actor, merchant_6), third)).
st(root, property(person, inst(actor, aristocrat_7), third)).
st(root, property(person, inst(actor, laborer_8), third)).
st(root, property(person, inst(actor, innkeeper_9), third)).
at(root, arg(option(4), buyer, inst(actor, you))).
at(root, action(option(4), trade)).
at(root, arg(option(4), price, inst(item, perfume_24))).
at(root, consequence(option(3), _not, state(bored, inst(actor, aristocrat_7)))).
at(root, consequence(option(1), _not, state(bored, inst(actor, laborer_8)))).
at(root, outcome(option(4), o(deal, deal))).
at(root, consequence_of(option(4), o(deal, deal), _not, relation(has_item, inst(actor, you), inst(item, perfume_24)))).
spontaneous(st(root, property(gender, inst(actor, merchant_6), masculine))).
spontaneous(st(root, property(gender, inst(actor, aristocrat_7), masculine))).
spontaneous(st(root, property(gender, inst(actor, laborer_8), masculine))).
spontaneous(st(root, property(gender, inst(actor, innkeeper_9), masculine))).
spontaneous(st(root, property(number, inst(actor, merchant_6), singular))).
spontaneous(st(root, property(number, inst(actor, aristocrat_7), singular))).
spontaneous(st(root, property(number, inst(actor, laborer_8), singular))).
spontaneous(st(root, property(number, inst(actor, innkeeper_9), singular))).
spontaneous(st(root, property(name, inst(actor, merchant_6), "merchant"))).
spontaneous(st(root, property(name, inst(actor, aristocrat_7), "noble"))).
spontaneous(st(root, property(name, inst(actor, laborer_8), "peasant"))).
spontaneous(st(root, property(name, inst(actor, innkeeper_9), "innkeeper"))).
st(root, property(gender, inst(actor, merchant_6), masculine)).
st(root, property(gender, inst(actor, aristocrat_7), masculine)).
st(root, property(gender, inst(actor, laborer_8), masculine)).
st(root, property(gender, inst(actor, innkeeper_9), masculine)).
st(root, property(number, inst(actor, merchant_6), singular)).
st(root, property(number, inst(actor, aristocrat_7), singular)).
st(root, property(number, inst(actor, laborer_8), singular)).
st(root, property(number, inst(actor, innkeeper_9), singular)).
st(root, property(name, inst(actor, merchant_6), "merchant")).
st(root, property(name, inst(actor, aristocrat_7), "noble")).
st(root, property(name, inst(actor, laborer_8), "peasant")).
st(root, property(name, inst(actor, innkeeper_9), "innkeeper")).
at(root, consequence(option(4), _not, relation(has_item, inst(actor, you), inst(item, perfume_24)))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_6), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, aristocrat_7), literacy))).
st(root, property(has_skill, inst(actor, merchant_6), literacy)).
st(root, property(has_skill, inst(actor, aristocrat_7), literacy)).
at(root, arg(option(4), seller, inst(actor, merchant_6))).
at(root, consequence_of(option(4), o(deal, deal), relation(has_item, inst(actor, merchant_6), inst(item, perfume_24)))).
at(root, arg(option(1), audience, inst(actor, laborer_8))).
at(root, arg(option(3), audience, inst(actor, aristocrat_7))).
at(root, action(option(1), tell_story)).
at(root, consequence_of(option(1), o(quality, boring), nullifies, potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, action(option(3), tell_story)).
at(root, consequence_of(option(3), o(quality, boring), nullifies, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, consequence_of(option(1), o(quality, entertaining), resolves, potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, consequence_of(option(3), o(quality, entertaining), resolves, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, consequence(option(1), resolves, potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, consequence(option(3), resolves, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, outcome(option(1), o(quality, entertaining))).
at(root, outcome(option(3), o(quality, entertaining))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_6), storytelling))).
spontaneous(st(root, property(has_skill, inst(actor, innkeeper_9), storytelling))).
spontaneous(st(root, property(has_skill, inst(actor, laborer_8), storytelling))).
st(root, property(has_skill, inst(actor, merchant_6), storytelling)).
st(root, property(has_skill, inst(actor, innkeeper_9), storytelling)).
st(root, property(has_skill, inst(actor, laborer_8), storytelling)).
at(root, consequence(option(4), relation(has_item, inst(actor, merchant_6), inst(item, perfume_24)))).
at(root, consequence_of(option(1), o(quality, boring), _not, state(bored, inst(actor, laborer_8)))).
at(root, consequence_of(option(3), o(quality, boring), _not, state(bored, inst(actor, aristocrat_7)))).
at(root, consequence_of(option(1), o(quality, entertaining), _not, state(bored, inst(actor, laborer_8)))).
at(root, consequence_of(option(3), o(quality, entertaining), _not, state(bored, inst(actor, aristocrat_7)))).
spontaneous(st(root, property(type, inst(item, dragon_scale_64), dragon_scale))).
spontaneous(st(root, property(type, inst(item, legends_book_73), legends_book))).
spontaneous(st(root, property(type, inst(item, perfume_116), perfume))).
spontaneous(st(root, property(type, inst(item, medicine_book_120), medicine_book))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_6), inst(item, legends_book_73)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, medicine_book_120)))).
spontaneous(st(root, inst(item, dragon_scale_64))).
spontaneous(st(root, inst(item, legends_book_73))).
spontaneous(st(root, inst(item, perfume_116))).
spontaneous(st(root, inst(item, medicine_book_120))).
st(root, property(type, inst(item, dragon_scale_64), dragon_scale)).
st(root, property(type, inst(item, legends_book_73), legends_book)).
st(root, property(type, inst(item, perfume_116), perfume)).
st(root, property(type, inst(item, medicine_book_120), medicine_book)).
st(root, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root, relation(has_item, inst(actor, merchant_6), inst(item, legends_book_73))).
st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116))).
st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, medicine_book_120))).
st(root, inst(item, dragon_scale_64)).
st(root, inst(item, legends_book_73)).
st(root, inst(item, perfume_116)).
st(root, inst(item, medicine_book_120)).
at(root, consequence(option(4), _not, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
spontaneous(st(root, property(gender, inst(item, dragon_scale_64), neuter))).
spontaneous(st(root, property(gender, inst(item, legends_book_73), neuter))).
spontaneous(st(root, property(gender, inst(item, perfume_116), neuter))).
spontaneous(st(root, property(gender, inst(item, medicine_book_120), neuter))).
spontaneous(st(root, property(number, inst(item, dragon_scale_64), singular))).
spontaneous(st(root, property(number, inst(item, legends_book_73), singular))).
spontaneous(st(root, property(number, inst(item, perfume_116), plural))).
spontaneous(st(root, property(number, inst(item, medicine_book_120), singular))).
spontaneous(st(root, property(name, inst(item, dragon_scale_64), "dragon scale"))).
spontaneous(st(root, property(name, inst(item, legends_book_73), "book of legends"))).
spontaneous(st(root, property(name, inst(item, perfume_116), "perfume"))).
spontaneous(st(root, property(name, inst(item, medicine_book_120), "book of medicine"))).
spontaneous(st(root, property(determined, inst(item, dragon_scale_64), true))).
spontaneous(st(root, property(determined, inst(item, legends_book_73), true))).
spontaneous(st(root, property(determined, inst(item, perfume_116), true))).
spontaneous(st(root, property(determined, inst(item, medicine_book_120), true))).
spontaneous(st(root, property(person, inst(item, dragon_scale_64), third))).
spontaneous(st(root, property(person, inst(item, legends_book_73), third))).
spontaneous(st(root, property(person, inst(item, perfume_116), third))).
spontaneous(st(root, property(person, inst(item, medicine_book_120), third))).
st(root, property(gender, inst(item, dragon_scale_64), neuter)).
st(root, property(gender, inst(item, legends_book_73), neuter)).
st(root, property(gender, inst(item, perfume_116), neuter)).
st(root, property(gender, inst(item, medicine_book_120), neuter)).
st(root, property(number, inst(item, dragon_scale_64), singular)).
st(root, property(number, inst(item, legends_book_73), singular)).
st(root, property(number, inst(item, perfume_116), plural)).
st(root, property(number, inst(item, medicine_book_120), singular)).
st(root, property(name, inst(item, dragon_scale_64), "dragon scale")).
st(root, property(name, inst(item, legends_book_73), "book of legends")).
st(root, property(name, inst(item, perfume_116), "perfume")).
st(root, property(name, inst(item, medicine_book_120), "book of medicine")).
st(root, property(determined, inst(item, dragon_scale_64), true)).
st(root, property(determined, inst(item, legends_book_73), true)).
st(root, property(determined, inst(item, perfume_116), true)).
st(root, property(determined, inst(item, medicine_book_120), true)).
st(root, property(person, inst(item, dragon_scale_64), third)).
st(root, property(person, inst(item, legends_book_73), third)).
st(root, property(person, inst(item, perfume_116), third)).
st(root, property(person, inst(item, medicine_book_120), third)).
at(root, consequence_of(option(4), o(deal, deal), relation(has_item, inst(actor, you), inst(item, dragon_scale_64)))).
at(root, arg(option(4), goods, inst(item, dragon_scale_64))).
at(root, setup_arg(goods, inst(item, dragon_scale_64))).
at(root, setup_arg(goods, inst(item, legends_book_73))).
at(root, consequence(option(4), relation(has_item, inst(actor, you), inst(item, dragon_scale_64)))).
at(root, consequence_of(option(4), o(deal, deal), _not, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
spontaneous(st(root, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
st(root, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))).
at(root, potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
at(root, consequence_of(option(4), o(deal, deal), resolves, potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))))).
at(root, consequence(option(4), _not, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
at(root, consequence(option(4), resolves, potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))))).
at(root, consequence_of(option(4), o(deal, deal), _not, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
at(root, arg(option(1), teller, inst(actor, you))).
at(root, arg(option(3), teller, inst(actor, you))).
st(root, property(has_skill, inst(actor, you), elocution)).
st(root, property(has_skill, inst(actor, you), storytelling)).
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
at(root, has_tool_for(inst(actor, merchant_6), storytelling)).
at(root, has_tool_for(inst(actor, aristocrat_7), healing)).
setting(root, wilderness).
at(root, skill_link(wilderness_lore, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(wilderness_lore, contest, no_tool, pursue, between(subject, object), either(o(result, caught_up), o(result, lost)))).
at(root, likely_outcome(option(1), o(quality, entertaining))).
at(root, likely_outcome(option(3), o(quality, entertaining))).
at(root, likely_outcome(option(4), o(deal, deal))).
node_status_reached(root, initialized).
node_status_reached(root, built).
node_status(root, built).
at(root, relevant_to(option(3), potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, relevant_to(option(1), potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, relevant_to(option(4), potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))))).
at(root, importance(8, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, importance(8, potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, importance(8, potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))))).
at(root, some_importance(8)).
at(root, most_important(potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, most_important(potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, most_important(potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))))).
at(root, motivated(option(1))).
at(root, motivated(option(3))).
at(root, motivated(option(4))).
at(root, initiated_by(potential(opportunity, state(bored, inst(actor, aristocrat_7))), inst(actor, aristocrat_7))).
at(root, initiated_by(potential(opportunity, state(bored, inst(actor, laborer_8))), inst(actor, laborer_8))).
at(root, initiated_by(potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))), inst(actor, merchant_6))).
at(root, can_trade(inst(actor, you), inst(item, perfume_24))).
at(root, can_trade(inst(actor, you), inst(item, speech_book_27))).
at(root, can_trade(inst(actor, merchant_6), inst(item, dragon_scale_64))).
at(root, can_trade(inst(actor, merchant_6), inst(item, legends_book_73))).
at(root, can_trade(inst(actor, aristocrat_7), inst(item, perfume_116))).
at(root, can_trade(inst(actor, aristocrat_7), inst(item, medicine_book_120))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), elocution))).
at(root, player_goal(have_tool_for(inst(actor, you), storytelling))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(1), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(3), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(4), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(4), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(4), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(4), irrelevant, have_tool_for(inst(actor, you), elocution))).
at(root, expectation(option(4), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(4), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, stakes(as_intended(inst(actor, you)), low)).
at(root, stakes(low)).
at(root, option_feel(option(1), safe)).
at(root, option_feel(option(3), safe)).
at(root, option_feel(option(4), safe)).
at(root, choice_structure(relaxed)).
at(root, relevant_skill(option(4), inst(actor, you), has, elocution)).
at(root, relevant_skill(option(1), inst(actor, you), has, storytelling)).
at(root, relevant_skill(option(3), inst(actor, you), has, storytelling)).
at(root, is_powerful(inst(actor, aristocrat_7))).
at(root, is_powerless(inst(actor, merchant_6))).
at(root, is_powerless(inst(actor, laborer_8))).
at(root, is_powerless(inst(actor, innkeeper_9))).
at(root, option_feel(option(1), sure_thing)).
at(root, option_feel(option(3), sure_thing)).
at(root, option_feel(option(4), sure_thing)).
at(root, choice_structure(positive_alternatives)).
at(root, outcome_perception(option(1), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(4), achieves, as_intended(inst(actor, you)))).
unresolved_potential(root, option(1), potential(opportunity, state(bored, inst(actor, aristocrat_7)))).
unresolved_potential(root, option(1), potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
unresolved_potential(root, option(3), potential(opportunity, state(bored, inst(actor, laborer_8)))).
unresolved_potential(root, option(3), potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
unresolved_potential(root, option(4), potential(opportunity, state(bored, inst(actor, aristocrat_7)))).
unresolved_potential(root, option(4), potential(opportunity, state(bored, inst(actor, laborer_8)))).
successor(root, option(1), root_1).
successor(root, option(3), root_3).
successor(root, option(4), root_4).
story_node(root_1).
story_node(root_3).
story_node(root_4).
st(root_4, relation(has_item, inst(actor, merchant_6), inst(item, perfume_24))).
st(root_4, relation(has_item, inst(actor, you), inst(item, dragon_scale_64))).
st(root_1, property(type, inst(actor, you), person)).
st(root_3, property(type, inst(actor, you), person)).
st(root_4, property(type, inst(actor, you), person)).
st(root_1, property(has_skill, inst(actor, you), literacy)).
st(root_3, property(has_skill, inst(actor, you), literacy)).
st(root_4, property(has_skill, inst(actor, you), literacy)).
st(root_1, inst(actor, you)).
st(root_3, inst(actor, you)).
st(root_4, inst(actor, you)).
st(root_1, state(party_member, inst(actor, you))).
st(root_3, state(party_member, inst(actor, you))).
st(root_4, state(party_member, inst(actor, you))).
st(root_1, property(name, inst(actor, you), "Dunyazad")).
st(root_3, property(name, inst(actor, you), "Dunyazad")).
st(root_4, property(name, inst(actor, you), "Dunyazad")).
st(root_1, property(person, inst(actor, you), second)).
st(root_3, property(person, inst(actor, you), second)).
st(root_4, property(person, inst(actor, you), second)).
st(root_1, property(gender, inst(actor, you), feminine)).
st(root_3, property(gender, inst(actor, you), feminine)).
st(root_4, property(gender, inst(actor, you), feminine)).
st(root_1, property(determined, inst(actor, you), false)).
st(root_3, property(determined, inst(actor, you), false)).
st(root_4, property(determined, inst(actor, you), false)).
st(root_1, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_3, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_1, relation(has_item, inst(actor, you), inst(item, speech_book_27))).
st(root_3, relation(has_item, inst(actor, you), inst(item, speech_book_27))).
st(root_4, relation(has_item, inst(actor, you), inst(item, speech_book_27))).
st(root_1, property(number, inst(item, perfume_24), plural)).
st(root_3, property(number, inst(item, perfume_24), plural)).
st(root_4, property(number, inst(item, perfume_24), plural)).
st(root_1, property(number, inst(item, speech_book_27), singular)).
st(root_3, property(number, inst(item, speech_book_27), singular)).
st(root_4, property(number, inst(item, speech_book_27), singular)).
st(root_1, property(name, inst(item, perfume_24), "perfume")).
st(root_3, property(name, inst(item, perfume_24), "perfume")).
st(root_4, property(name, inst(item, perfume_24), "perfume")).
st(root_1, property(name, inst(item, speech_book_27), "book of speeches")).
st(root_3, property(name, inst(item, speech_book_27), "book of speeches")).
st(root_4, property(name, inst(item, speech_book_27), "book of speeches")).
st(root_1, property(type, inst(item, perfume_24), perfume)).
st(root_3, property(type, inst(item, perfume_24), perfume)).
st(root_4, property(type, inst(item, perfume_24), perfume)).
st(root_1, property(type, inst(item, speech_book_27), speech_book)).
st(root_3, property(type, inst(item, speech_book_27), speech_book)).
st(root_4, property(type, inst(item, speech_book_27), speech_book)).
st(root_1, inst(item, perfume_24)).
st(root_3, inst(item, perfume_24)).
st(root_4, inst(item, perfume_24)).
st(root_1, inst(item, speech_book_27)).
st(root_3, inst(item, speech_book_27)).
st(root_4, inst(item, speech_book_27)).
st(root_1, inst(actor, merchant_6)).
st(root_3, inst(actor, merchant_6)).
st(root_4, inst(actor, merchant_6)).
st(root_1, inst(actor, aristocrat_7)).
st(root_3, inst(actor, aristocrat_7)).
st(root_4, inst(actor, aristocrat_7)).
st(root_1, inst(actor, laborer_8)).
st(root_3, inst(actor, laborer_8)).
st(root_4, inst(actor, laborer_8)).
st(root_1, inst(actor, innkeeper_9)).
st(root_3, inst(actor, innkeeper_9)).
st(root_4, inst(actor, innkeeper_9)).
st(root_1, state(bored, inst(actor, aristocrat_7))).
st(root_4, state(bored, inst(actor, aristocrat_7))).
st(root_3, state(bored, inst(actor, laborer_8))).
st(root_4, state(bored, inst(actor, laborer_8))).
st(root_1, property(type, inst(actor, merchant_6), merchant)).
st(root_3, property(type, inst(actor, merchant_6), merchant)).
st(root_4, property(type, inst(actor, merchant_6), merchant)).
st(root_1, property(type, inst(actor, aristocrat_7), noble)).
st(root_3, property(type, inst(actor, aristocrat_7), noble)).
st(root_4, property(type, inst(actor, aristocrat_7), noble)).
st(root_1, property(type, inst(actor, laborer_8), peasant)).
st(root_3, property(type, inst(actor, laborer_8), peasant)).
st(root_4, property(type, inst(actor, laborer_8), peasant)).
st(root_1, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_3, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_4, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_1, property(determined, inst(actor, merchant_6), true)).
st(root_3, property(determined, inst(actor, merchant_6), true)).
st(root_4, property(determined, inst(actor, merchant_6), true)).
st(root_1, property(determined, inst(actor, aristocrat_7), true)).
st(root_3, property(determined, inst(actor, aristocrat_7), true)).
st(root_4, property(determined, inst(actor, aristocrat_7), true)).
st(root_1, property(determined, inst(actor, laborer_8), true)).
st(root_3, property(determined, inst(actor, laborer_8), true)).
st(root_4, property(determined, inst(actor, laborer_8), true)).
st(root_1, property(determined, inst(actor, innkeeper_9), true)).
st(root_3, property(determined, inst(actor, innkeeper_9), true)).
st(root_4, property(determined, inst(actor, innkeeper_9), true)).
st(root_1, property(person, inst(actor, merchant_6), third)).
st(root_3, property(person, inst(actor, merchant_6), third)).
st(root_4, property(person, inst(actor, merchant_6), third)).
st(root_1, property(person, inst(actor, aristocrat_7), third)).
st(root_3, property(person, inst(actor, aristocrat_7), third)).
st(root_4, property(person, inst(actor, aristocrat_7), third)).
st(root_1, property(person, inst(actor, laborer_8), third)).
st(root_3, property(person, inst(actor, laborer_8), third)).
st(root_4, property(person, inst(actor, laborer_8), third)).
st(root_1, property(person, inst(actor, innkeeper_9), third)).
st(root_3, property(person, inst(actor, innkeeper_9), third)).
st(root_4, property(person, inst(actor, innkeeper_9), third)).
st(root_1, property(gender, inst(actor, merchant_6), masculine)).
st(root_3, property(gender, inst(actor, merchant_6), masculine)).
st(root_4, property(gender, inst(actor, merchant_6), masculine)).
st(root_1, property(gender, inst(actor, aristocrat_7), masculine)).
st(root_3, property(gender, inst(actor, aristocrat_7), masculine)).
st(root_4, property(gender, inst(actor, aristocrat_7), masculine)).
st(root_1, property(gender, inst(actor, laborer_8), masculine)).
st(root_3, property(gender, inst(actor, laborer_8), masculine)).
st(root_4, property(gender, inst(actor, laborer_8), masculine)).
st(root_1, property(gender, inst(actor, innkeeper_9), masculine)).
st(root_3, property(gender, inst(actor, innkeeper_9), masculine)).
st(root_4, property(gender, inst(actor, innkeeper_9), masculine)).
st(root_1, property(number, inst(actor, merchant_6), singular)).
st(root_3, property(number, inst(actor, merchant_6), singular)).
st(root_4, property(number, inst(actor, merchant_6), singular)).
st(root_1, property(number, inst(actor, aristocrat_7), singular)).
st(root_3, property(number, inst(actor, aristocrat_7), singular)).
st(root_4, property(number, inst(actor, aristocrat_7), singular)).
st(root_1, property(number, inst(actor, laborer_8), singular)).
st(root_3, property(number, inst(actor, laborer_8), singular)).
st(root_4, property(number, inst(actor, laborer_8), singular)).
st(root_1, property(number, inst(actor, innkeeper_9), singular)).
st(root_3, property(number, inst(actor, innkeeper_9), singular)).
st(root_4, property(number, inst(actor, innkeeper_9), singular)).
st(root_1, property(name, inst(actor, merchant_6), "merchant")).
st(root_3, property(name, inst(actor, merchant_6), "merchant")).
st(root_4, property(name, inst(actor, merchant_6), "merchant")).
st(root_1, property(name, inst(actor, aristocrat_7), "noble")).
st(root_3, property(name, inst(actor, aristocrat_7), "noble")).
st(root_4, property(name, inst(actor, aristocrat_7), "noble")).
st(root_1, property(name, inst(actor, laborer_8), "peasant")).
st(root_3, property(name, inst(actor, laborer_8), "peasant")).
st(root_4, property(name, inst(actor, laborer_8), "peasant")).
st(root_1, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_3, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_4, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_1, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_3, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_4, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_1, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_3, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_4, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_1, property(has_skill, inst(actor, merchant_6), storytelling)).
st(root_3, property(has_skill, inst(actor, merchant_6), storytelling)).
st(root_4, property(has_skill, inst(actor, merchant_6), storytelling)).
st(root_1, property(has_skill, inst(actor, innkeeper_9), storytelling)).
st(root_3, property(has_skill, inst(actor, innkeeper_9), storytelling)).
st(root_4, property(has_skill, inst(actor, innkeeper_9), storytelling)).
st(root_1, property(has_skill, inst(actor, laborer_8), storytelling)).
st(root_3, property(has_skill, inst(actor, laborer_8), storytelling)).
st(root_4, property(has_skill, inst(actor, laborer_8), storytelling)).
st(root_1, property(type, inst(item, dragon_scale_64), dragon_scale)).
st(root_3, property(type, inst(item, dragon_scale_64), dragon_scale)).
st(root_4, property(type, inst(item, dragon_scale_64), dragon_scale)).
st(root_1, property(type, inst(item, legends_book_73), legends_book)).
st(root_3, property(type, inst(item, legends_book_73), legends_book)).
st(root_4, property(type, inst(item, legends_book_73), legends_book)).
st(root_1, property(type, inst(item, perfume_116), perfume)).
st(root_3, property(type, inst(item, perfume_116), perfume)).
st(root_4, property(type, inst(item, perfume_116), perfume)).
st(root_1, property(type, inst(item, medicine_book_120), medicine_book)).
st(root_3, property(type, inst(item, medicine_book_120), medicine_book)).
st(root_4, property(type, inst(item, medicine_book_120), medicine_book)).
st(root_1, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root_3, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root_1, relation(has_item, inst(actor, merchant_6), inst(item, legends_book_73))).
st(root_3, relation(has_item, inst(actor, merchant_6), inst(item, legends_book_73))).
st(root_4, relation(has_item, inst(actor, merchant_6), inst(item, legends_book_73))).
st(root_1, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116))).
st(root_3, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116))).
st(root_4, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116))).
st(root_1, relation(has_item, inst(actor, aristocrat_7), inst(item, medicine_book_120))).
st(root_3, relation(has_item, inst(actor, aristocrat_7), inst(item, medicine_book_120))).
st(root_4, relation(has_item, inst(actor, aristocrat_7), inst(item, medicine_book_120))).
st(root_1, inst(item, dragon_scale_64)).
st(root_3, inst(item, dragon_scale_64)).
st(root_4, inst(item, dragon_scale_64)).
st(root_1, inst(item, legends_book_73)).
st(root_3, inst(item, legends_book_73)).
st(root_4, inst(item, legends_book_73)).
st(root_1, inst(item, perfume_116)).
st(root_3, inst(item, perfume_116)).
st(root_4, inst(item, perfume_116)).
st(root_1, inst(item, medicine_book_120)).
st(root_3, inst(item, medicine_book_120)).
st(root_4, inst(item, medicine_book_120)).
st(root_1, property(gender, inst(item, dragon_scale_64), neuter)).
st(root_3, property(gender, inst(item, dragon_scale_64), neuter)).
st(root_4, property(gender, inst(item, dragon_scale_64), neuter)).
st(root_1, property(gender, inst(item, legends_book_73), neuter)).
st(root_3, property(gender, inst(item, legends_book_73), neuter)).
st(root_4, property(gender, inst(item, legends_book_73), neuter)).
st(root_1, property(gender, inst(item, perfume_116), neuter)).
st(root_3, property(gender, inst(item, perfume_116), neuter)).
st(root_4, property(gender, inst(item, perfume_116), neuter)).
st(root_1, property(gender, inst(item, medicine_book_120), neuter)).
st(root_3, property(gender, inst(item, medicine_book_120), neuter)).
st(root_4, property(gender, inst(item, medicine_book_120), neuter)).
st(root_1, property(number, inst(item, dragon_scale_64), singular)).
st(root_3, property(number, inst(item, dragon_scale_64), singular)).
st(root_4, property(number, inst(item, dragon_scale_64), singular)).
st(root_1, property(number, inst(item, legends_book_73), singular)).
st(root_3, property(number, inst(item, legends_book_73), singular)).
st(root_4, property(number, inst(item, legends_book_73), singular)).
st(root_1, property(number, inst(item, perfume_116), plural)).
st(root_3, property(number, inst(item, perfume_116), plural)).
st(root_4, property(number, inst(item, perfume_116), plural)).
st(root_1, property(number, inst(item, medicine_book_120), singular)).
st(root_3, property(number, inst(item, medicine_book_120), singular)).
st(root_4, property(number, inst(item, medicine_book_120), singular)).
st(root_1, property(name, inst(item, dragon_scale_64), "dragon scale")).
st(root_3, property(name, inst(item, dragon_scale_64), "dragon scale")).
st(root_4, property(name, inst(item, dragon_scale_64), "dragon scale")).
st(root_1, property(name, inst(item, legends_book_73), "book of legends")).
st(root_3, property(name, inst(item, legends_book_73), "book of legends")).
st(root_4, property(name, inst(item, legends_book_73), "book of legends")).
st(root_1, property(name, inst(item, perfume_116), "perfume")).
st(root_3, property(name, inst(item, perfume_116), "perfume")).
st(root_4, property(name, inst(item, perfume_116), "perfume")).
st(root_1, property(name, inst(item, medicine_book_120), "book of medicine")).
st(root_3, property(name, inst(item, medicine_book_120), "book of medicine")).
st(root_4, property(name, inst(item, medicine_book_120), "book of medicine")).
st(root_1, property(determined, inst(item, dragon_scale_64), true)).
st(root_3, property(determined, inst(item, dragon_scale_64), true)).
st(root_4, property(determined, inst(item, dragon_scale_64), true)).
st(root_1, property(determined, inst(item, legends_book_73), true)).
st(root_3, property(determined, inst(item, legends_book_73), true)).
st(root_4, property(determined, inst(item, legends_book_73), true)).
st(root_1, property(determined, inst(item, perfume_116), true)).
st(root_3, property(determined, inst(item, perfume_116), true)).
st(root_4, property(determined, inst(item, perfume_116), true)).
st(root_1, property(determined, inst(item, medicine_book_120), true)).
st(root_3, property(determined, inst(item, medicine_book_120), true)).
st(root_4, property(determined, inst(item, medicine_book_120), true)).
st(root_1, property(person, inst(item, dragon_scale_64), third)).
st(root_3, property(person, inst(item, dragon_scale_64), third)).
st(root_4, property(person, inst(item, dragon_scale_64), third)).
st(root_1, property(person, inst(item, legends_book_73), third)).
st(root_3, property(person, inst(item, legends_book_73), third)).
st(root_4, property(person, inst(item, legends_book_73), third)).
st(root_1, property(person, inst(item, perfume_116), third)).
st(root_3, property(person, inst(item, perfume_116), third)).
st(root_4, property(person, inst(item, perfume_116), third)).
st(root_1, property(person, inst(item, medicine_book_120), third)).
st(root_3, property(person, inst(item, medicine_book_120), third)).
st(root_4, property(person, inst(item, medicine_book_120), third)).
st(root_1, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root_3, relation(selling, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root_1, property(has_skill, inst(actor, you), elocution)).
st(root_3, property(has_skill, inst(actor, you), elocution)).
st(root_4, property(has_skill, inst(actor, you), elocution)).
st(root_1, property(has_skill, inst(actor, you), storytelling)).
st(root_3, property(has_skill, inst(actor, you), storytelling)).
st(root_4, property(has_skill, inst(actor, you), storytelling)).
node_status_reached(root_1, uninitialized).
node_status_reached(root_3, uninitialized).
node_status_reached(root_4, uninitialized).
node_status_reached(root, branched).
node_status_reached(root, polished).
node_status(root_1, uninitialized).
node_status(root_3, uninitialized).
node_status(root_4, uninitialized).
node_status(root, polished).
path_length(root_1, 1).
path_length(root_3, 1).
path_length(root_4, 1).
at(root_1, is_powerful(inst(actor, aristocrat_7))).
at(root_3, is_powerful(inst(actor, aristocrat_7))).
at(root_4, is_powerful(inst(actor, aristocrat_7))).
at(root_1, is_powerless(inst(actor, merchant_6))).
at(root_3, is_powerless(inst(actor, merchant_6))).
at(root_4, is_powerless(inst(actor, merchant_6))).
at(root_1, is_powerless(inst(actor, laborer_8))).
at(root_3, is_powerless(inst(actor, laborer_8))).
at(root_4, is_powerless(inst(actor, laborer_8))).
at(root_1, is_powerless(inst(actor, innkeeper_9))).
at(root_3, is_powerless(inst(actor, innkeeper_9))).
at(root_4, is_powerless(inst(actor, innkeeper_9))).
at(root_1, text_mode(example)).
at(root_3, text_mode(example)).
at(root_4, text_mode(example)).
max_unique(120).
