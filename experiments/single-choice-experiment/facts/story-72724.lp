experiment_template(dilemma).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
setup_priority(on_sale, 4).
setup_priority(tavern, 3).
setup_priority(threatened_innocents, 1).
setup_priority(market, 5).
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
at(root, setup_arg(innkeeper, inst(actor, innkeeper_9))).
st(root, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root, relation(has_item, inst(actor, you), inst(item, legends_book_30))).
st(root, property(number, inst(item, perfume_24), plural)).
st(root, property(number, inst(item, legends_book_30), singular)).
st(root, property(name, inst(item, perfume_24), "perfume")).
st(root, property(name, inst(item, legends_book_30), "book of legends")).
st(root, property(type, inst(item, perfume_24), perfume)).
st(root, property(type, inst(item, legends_book_30), legends_book)).
st(root, inst(item, perfume_24)).
st(root, inst(item, legends_book_30)).
spontaneous(st(root, inst(actor, merchant_6))).
spontaneous(st(root, inst(actor, aristocrat_7))).
spontaneous(st(root, inst(actor, innkeeper_9))).
st(root, inst(actor, merchant_6)).
st(root, inst(actor, aristocrat_7)).
st(root, inst(actor, innkeeper_9)).
spontaneous(st(root, state(bored, inst(actor, merchant_6)))).
spontaneous(st(root, state(bored, inst(actor, aristocrat_7)))).
spontaneous(st(root, state(knows_gossip, inst(actor, innkeeper_9)))).
st(root, state(bored, inst(actor, merchant_6))).
st(root, state(bored, inst(actor, aristocrat_7))).
st(root, state(knows_gossip, inst(actor, innkeeper_9))).
at(root, potential(opportunity, state(bored, inst(actor, merchant_6)))).
at(root, potential(opportunity, state(bored, inst(actor, aristocrat_7)))).
at(root, potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9)))).
spontaneous(st(root, property(type, inst(actor, merchant_6), merchant))).
spontaneous(st(root, property(type, inst(actor, aristocrat_7), noble))).
spontaneous(st(root, property(type, inst(actor, innkeeper_9), innkeeper))).
spontaneous(st(root, property(determined, inst(actor, merchant_6), true))).
spontaneous(st(root, property(determined, inst(actor, aristocrat_7), true))).
spontaneous(st(root, property(determined, inst(actor, innkeeper_9), true))).
spontaneous(st(root, property(person, inst(actor, merchant_6), third))).
spontaneous(st(root, property(person, inst(actor, aristocrat_7), third))).
spontaneous(st(root, property(person, inst(actor, innkeeper_9), third))).
st(root, property(type, inst(actor, merchant_6), merchant)).
st(root, property(type, inst(actor, aristocrat_7), noble)).
st(root, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root, property(determined, inst(actor, merchant_6), true)).
st(root, property(determined, inst(actor, aristocrat_7), true)).
st(root, property(determined, inst(actor, innkeeper_9), true)).
st(root, property(person, inst(actor, merchant_6), third)).
st(root, property(person, inst(actor, aristocrat_7), third)).
st(root, property(person, inst(actor, innkeeper_9), third)).
at(root, consequence(option(2), _not, state(bored, inst(actor, merchant_6)))).
at(root, consequence(option(3), _not, state(bored, inst(actor, aristocrat_7)))).
at(root, consequence(option(1), _not, state(knows_gossip, inst(actor, innkeeper_9)))).
at(root, consequence(option(2), nullifies, potential(opportunity, state(bored, inst(actor, merchant_6))))).
at(root, consequence(option(3), nullifies, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, consequence(option(1), nullifies, potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9))))).
spontaneous(st(root, property(gender, inst(actor, merchant_6), masculine))).
spontaneous(st(root, property(gender, inst(actor, aristocrat_7), feminine))).
spontaneous(st(root, property(gender, inst(actor, innkeeper_9), masculine))).
spontaneous(st(root, property(number, inst(actor, merchant_6), singular))).
spontaneous(st(root, property(number, inst(actor, aristocrat_7), singular))).
spontaneous(st(root, property(number, inst(actor, innkeeper_9), singular))).
spontaneous(st(root, property(name, inst(actor, merchant_6), "merchant"))).
spontaneous(st(root, property(name, inst(actor, aristocrat_7), "noble"))).
spontaneous(st(root, property(name, inst(actor, innkeeper_9), "innkeeper"))).
st(root, property(gender, inst(actor, merchant_6), masculine)).
st(root, property(gender, inst(actor, aristocrat_7), feminine)).
st(root, property(gender, inst(actor, innkeeper_9), masculine)).
st(root, property(number, inst(actor, merchant_6), singular)).
st(root, property(number, inst(actor, aristocrat_7), singular)).
st(root, property(number, inst(actor, innkeeper_9), singular)).
st(root, property(name, inst(actor, merchant_6), "merchant")).
st(root, property(name, inst(actor, aristocrat_7), "noble")).
st(root, property(name, inst(actor, innkeeper_9), "innkeeper")).
spontaneous(st(root, property(has_skill, inst(actor, merchant_6), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, aristocrat_7), literacy))).
st(root, property(has_skill, inst(actor, merchant_6), literacy)).
st(root, property(has_skill, inst(actor, aristocrat_7), literacy)).
at(root, action(option(1), gossip)).
at(root, consequence_of(option(1), o(utility, useless), nullifies, potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9))))).
at(root, arg(option(1), knowledgeable, inst(actor, innkeeper_9))).
at(root, consequence_of(option(1), o(utility, useful), resolves, potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9))))).
at(root, action(option(2), play_song)).
at(root, consequence_of(option(2), o(quality, plain), nullifies, potential(opportunity, state(bored, inst(actor, merchant_6))))).
at(root, arg(option(2), audience, inst(actor, merchant_6))).
at(root, action(option(3), play_song)).
at(root, consequence_of(option(3), o(quality, plain), nullifies, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, arg(option(3), audience, inst(actor, aristocrat_7))).
at(root, consequence_of(option(2), o(quality, harmonious), resolves, potential(opportunity, state(bored, inst(actor, merchant_6))))).
at(root, consequence_of(option(3), o(quality, harmonious), resolves, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, outcome(option(1), o(utility, useless))).
at(root, outcome(option(2), o(quality, plain))).
at(root, outcome(option(3), o(quality, plain))).
spontaneous(st(root, property(has_skill, inst(actor, merchant_6), storytelling))).
spontaneous(st(root, property(has_skill, inst(actor, innkeeper_9), storytelling))).
st(root, property(has_skill, inst(actor, merchant_6), storytelling)).
st(root, property(has_skill, inst(actor, innkeeper_9), storytelling)).
at(root, consequence_of(option(1), o(utility, useless), _not, state(knows_gossip, inst(actor, innkeeper_9)))).
at(root, consequence_of(option(1), o(utility, useful), _not, state(knows_gossip, inst(actor, innkeeper_9)))).
at(root, consequence_of(option(2), o(quality, plain), _not, state(bored, inst(actor, merchant_6)))).
at(root, consequence_of(option(3), o(quality, plain), _not, state(bored, inst(actor, aristocrat_7)))).
at(root, consequence_of(option(2), o(quality, harmonious), _not, state(bored, inst(actor, merchant_6)))).
at(root, consequence_of(option(3), o(quality, harmonious), _not, state(bored, inst(actor, aristocrat_7)))).
spontaneous(st(root, property(type, inst(item, perfume_66), perfume))).
spontaneous(st(root, property(type, inst(item, medicine_chest_68), medicine_chest))).
spontaneous(st(root, property(type, inst(item, medicine_book_72), medicine_book))).
spontaneous(st(root, property(type, inst(item, perfume_116), perfume))).
spontaneous(st(root, property(type, inst(item, music_book_119), music_book))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_6), inst(item, perfume_66)))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68)))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_6), inst(item, medicine_book_72)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119)))).
spontaneous(st(root, inst(item, perfume_66))).
spontaneous(st(root, inst(item, medicine_chest_68))).
spontaneous(st(root, inst(item, medicine_book_72))).
spontaneous(st(root, inst(item, perfume_116))).
spontaneous(st(root, inst(item, music_book_119))).
st(root, property(type, inst(item, perfume_66), perfume)).
st(root, property(type, inst(item, medicine_chest_68), medicine_chest)).
st(root, property(type, inst(item, medicine_book_72), medicine_book)).
st(root, property(type, inst(item, perfume_116), perfume)).
st(root, property(type, inst(item, music_book_119), music_book)).
st(root, relation(has_item, inst(actor, merchant_6), inst(item, perfume_66))).
st(root, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68))).
st(root, relation(has_item, inst(actor, merchant_6), inst(item, medicine_book_72))).
st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116))).
st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119))).
st(root, inst(item, perfume_66)).
st(root, inst(item, medicine_chest_68)).
st(root, inst(item, medicine_book_72)).
st(root, inst(item, perfume_116)).
st(root, inst(item, music_book_119)).
spontaneous(st(root, property(gender, inst(item, perfume_66), neuter))).
spontaneous(st(root, property(gender, inst(item, medicine_chest_68), neuter))).
spontaneous(st(root, property(gender, inst(item, medicine_book_72), neuter))).
spontaneous(st(root, property(gender, inst(item, perfume_116), neuter))).
spontaneous(st(root, property(gender, inst(item, music_book_119), neuter))).
spontaneous(st(root, property(number, inst(item, perfume_66), plural))).
spontaneous(st(root, property(number, inst(item, medicine_chest_68), singular))).
spontaneous(st(root, property(number, inst(item, medicine_book_72), singular))).
spontaneous(st(root, property(number, inst(item, perfume_116), plural))).
spontaneous(st(root, property(number, inst(item, music_book_119), singular))).
spontaneous(st(root, property(name, inst(item, perfume_66), "perfume"))).
spontaneous(st(root, property(name, inst(item, medicine_chest_68), "medicine chest"))).
spontaneous(st(root, property(name, inst(item, medicine_book_72), "book of medicine"))).
spontaneous(st(root, property(name, inst(item, perfume_116), "perfume"))).
spontaneous(st(root, property(name, inst(item, music_book_119), "music book"))).
spontaneous(st(root, property(determined, inst(item, perfume_66), true))).
spontaneous(st(root, property(determined, inst(item, medicine_chest_68), true))).
spontaneous(st(root, property(determined, inst(item, medicine_book_72), true))).
spontaneous(st(root, property(determined, inst(item, perfume_116), true))).
spontaneous(st(root, property(determined, inst(item, music_book_119), true))).
spontaneous(st(root, property(person, inst(item, perfume_66), third))).
spontaneous(st(root, property(person, inst(item, medicine_chest_68), third))).
spontaneous(st(root, property(person, inst(item, medicine_book_72), third))).
spontaneous(st(root, property(person, inst(item, perfume_116), third))).
spontaneous(st(root, property(person, inst(item, music_book_119), third))).
st(root, property(gender, inst(item, perfume_66), neuter)).
st(root, property(gender, inst(item, medicine_chest_68), neuter)).
st(root, property(gender, inst(item, medicine_book_72), neuter)).
st(root, property(gender, inst(item, perfume_116), neuter)).
st(root, property(gender, inst(item, music_book_119), neuter)).
st(root, property(number, inst(item, perfume_66), plural)).
st(root, property(number, inst(item, medicine_chest_68), singular)).
st(root, property(number, inst(item, medicine_book_72), singular)).
st(root, property(number, inst(item, perfume_116), plural)).
st(root, property(number, inst(item, music_book_119), singular)).
st(root, property(name, inst(item, perfume_66), "perfume")).
st(root, property(name, inst(item, medicine_chest_68), "medicine chest")).
st(root, property(name, inst(item, medicine_book_72), "book of medicine")).
st(root, property(name, inst(item, perfume_116), "perfume")).
st(root, property(name, inst(item, music_book_119), "music book")).
st(root, property(determined, inst(item, perfume_66), true)).
st(root, property(determined, inst(item, medicine_chest_68), true)).
st(root, property(determined, inst(item, medicine_book_72), true)).
st(root, property(determined, inst(item, perfume_116), true)).
st(root, property(determined, inst(item, music_book_119), true)).
st(root, property(person, inst(item, perfume_66), third)).
st(root, property(person, inst(item, medicine_chest_68), third)).
st(root, property(person, inst(item, medicine_book_72), third)).
st(root, property(person, inst(item, perfume_116), third)).
st(root, property(person, inst(item, music_book_119), third)).
at(root, setup_arg(goods, inst(item, perfume_66))).
at(root, arg(option(1), interested, inst(actor, you))).
at(root, arg(option(2), musician, inst(actor, you))).
at(root, arg(option(3), musician, inst(actor, you))).
st(root, property(has_skill, inst(actor, you), music)).
st(root, property(has_skill, inst(actor, you), healing)).
at(root, initiator(option(1), inst(actor, you))).
at(root, initiator(option(2), inst(actor, you))).
at(root, initiator(option(3), inst(actor, you))).
at(root, option(2)).
at(root, option(3)).
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
at(root, unlikely_outcome(option(2), o(quality, harmonious))).
at(root, unlikely_outcome(option(3), o(quality, harmonious))).
at(root, unlikely_outcome(option(1), o(utility, useful))).
at(root, has_tool_for(inst(actor, you), storytelling)).
at(root, has_tool_for(inst(actor, merchant_6), healing)).
setting(root, city).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(acrobatics, contest, no_tool, pursue, between(subject, object), either(o(result, caught_up), o(result, lost)))).
at(root, likely_outcome(option(1), o(utility, useless))).
at(root, likely_outcome(option(2), o(quality, plain))).
at(root, likely_outcome(option(3), o(quality, plain))).
node_status_reached(root, initialized).
node_status_reached(root, built).
node_status(root, built).
at(root, relevant_to(option(2), potential(opportunity, state(bored, inst(actor, merchant_6))))).
at(root, relevant_to(option(3), potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, relevant_to(option(1), potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9))))).
at(root, importance(8, potential(opportunity, state(bored, inst(actor, merchant_6))))).
at(root, importance(8, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, importance(8, potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9))))).
at(root, some_importance(8)).
at(root, most_important(potential(opportunity, state(bored, inst(actor, merchant_6))))).
at(root, most_important(potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, most_important(potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9))))).
at(root, motivated(option(1))).
at(root, motivated(option(2))).
at(root, motivated(option(3))).
at(root, initiated_by(potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9))), inst(actor, innkeeper_9))).
at(root, initiated_by(potential(opportunity, state(bored, inst(actor, merchant_6))), inst(actor, merchant_6))).
at(root, initiated_by(potential(opportunity, state(bored, inst(actor, aristocrat_7))), inst(actor, aristocrat_7))).
at(root, can_trade(inst(actor, you), inst(item, perfume_24))).
at(root, can_trade(inst(actor, you), inst(item, legends_book_30))).
at(root, can_trade(inst(actor, merchant_6), inst(item, perfume_66))).
at(root, can_trade(inst(actor, merchant_6), inst(item, medicine_chest_68))).
at(root, can_trade(inst(actor, merchant_6), inst(item, medicine_book_72))).
at(root, can_trade(inst(actor, aristocrat_7), inst(item, perfume_116))).
at(root, can_trade(inst(actor, aristocrat_7), inst(item, music_book_119))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), music))).
at(root, player_goal(have_tool_for(inst(actor, you), healing))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(1), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(2), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(3), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), healing))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, stakes(as_intended(inst(actor, you)), low)).
at(root, stakes(low)).
at(root, option_feel(option(1), bad)).
at(root, option_feel(option(2), bad)).
at(root, option_feel(option(3), bad)).
at(root, option_feel(option(1), doomed)).
at(root, option_feel(option(2), doomed)).
at(root, option_feel(option(3), doomed)).
at(root, choice_structure(negative_alternatives)).
at(root, relevant_skill(option(2), inst(actor, you), has, music)).
at(root, relevant_skill(option(3), inst(actor, you), has, music)).
at(root, relevant_skill(option(1), inst(actor, you), missing, elocution)).
at(root, relevant_tool(option(2), inst(actor, you), missing, music)).
at(root, relevant_tool(option(3), inst(actor, you), missing, music)).
at(root, is_powerful(inst(actor, aristocrat_7))).
at(root, is_powerless(inst(actor, merchant_6))).
at(root, is_powerless(inst(actor, innkeeper_9))).
at(root, choice_structure(bleak)).
at(root, choice_structure(depressing)).
at(root, outcome_perception(option(1), fails, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(2), fails, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), fails, as_intended(inst(actor, you)))).
unresolved_potential(root, option(1), potential(opportunity, state(bored, inst(actor, merchant_6)))).
unresolved_potential(root, option(1), potential(opportunity, state(bored, inst(actor, aristocrat_7)))).
unresolved_potential(root, option(2), potential(opportunity, state(bored, inst(actor, aristocrat_7)))).
unresolved_potential(root, option(2), potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9)))).
unresolved_potential(root, option(3), potential(opportunity, state(bored, inst(actor, merchant_6)))).
unresolved_potential(root, option(3), potential(opportunity, state(knows_gossip, inst(actor, innkeeper_9)))).
successor(root, option(1), root_1).
successor(root, option(2), root_2).
successor(root, option(3), root_3).
story_node(root_1).
story_node(root_2).
story_node(root_3).
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
st(root_1, property(name, inst(actor, you), "Dunyazad")).
st(root_2, property(name, inst(actor, you), "Dunyazad")).
st(root_3, property(name, inst(actor, you), "Dunyazad")).
st(root_1, property(person, inst(actor, you), second)).
st(root_2, property(person, inst(actor, you), second)).
st(root_3, property(person, inst(actor, you), second)).
st(root_1, property(gender, inst(actor, you), feminine)).
st(root_2, property(gender, inst(actor, you), feminine)).
st(root_3, property(gender, inst(actor, you), feminine)).
st(root_1, property(determined, inst(actor, you), false)).
st(root_2, property(determined, inst(actor, you), false)).
st(root_3, property(determined, inst(actor, you), false)).
st(root_1, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_2, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_3, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root_1, relation(has_item, inst(actor, you), inst(item, legends_book_30))).
st(root_2, relation(has_item, inst(actor, you), inst(item, legends_book_30))).
st(root_3, relation(has_item, inst(actor, you), inst(item, legends_book_30))).
st(root_1, property(number, inst(item, perfume_24), plural)).
st(root_2, property(number, inst(item, perfume_24), plural)).
st(root_3, property(number, inst(item, perfume_24), plural)).
st(root_1, property(number, inst(item, legends_book_30), singular)).
st(root_2, property(number, inst(item, legends_book_30), singular)).
st(root_3, property(number, inst(item, legends_book_30), singular)).
st(root_1, property(name, inst(item, perfume_24), "perfume")).
st(root_2, property(name, inst(item, perfume_24), "perfume")).
st(root_3, property(name, inst(item, perfume_24), "perfume")).
st(root_1, property(name, inst(item, legends_book_30), "book of legends")).
st(root_2, property(name, inst(item, legends_book_30), "book of legends")).
st(root_3, property(name, inst(item, legends_book_30), "book of legends")).
st(root_1, property(type, inst(item, perfume_24), perfume)).
st(root_2, property(type, inst(item, perfume_24), perfume)).
st(root_3, property(type, inst(item, perfume_24), perfume)).
st(root_1, property(type, inst(item, legends_book_30), legends_book)).
st(root_2, property(type, inst(item, legends_book_30), legends_book)).
st(root_3, property(type, inst(item, legends_book_30), legends_book)).
st(root_1, inst(item, perfume_24)).
st(root_2, inst(item, perfume_24)).
st(root_3, inst(item, perfume_24)).
st(root_1, inst(item, legends_book_30)).
st(root_2, inst(item, legends_book_30)).
st(root_3, inst(item, legends_book_30)).
st(root_1, inst(actor, merchant_6)).
st(root_2, inst(actor, merchant_6)).
st(root_3, inst(actor, merchant_6)).
st(root_1, inst(actor, aristocrat_7)).
st(root_2, inst(actor, aristocrat_7)).
st(root_3, inst(actor, aristocrat_7)).
st(root_1, inst(actor, innkeeper_9)).
st(root_2, inst(actor, innkeeper_9)).
st(root_3, inst(actor, innkeeper_9)).
st(root_1, state(bored, inst(actor, merchant_6))).
st(root_3, state(bored, inst(actor, merchant_6))).
st(root_1, state(bored, inst(actor, aristocrat_7))).
st(root_2, state(bored, inst(actor, aristocrat_7))).
st(root_2, state(knows_gossip, inst(actor, innkeeper_9))).
st(root_3, state(knows_gossip, inst(actor, innkeeper_9))).
st(root_1, property(type, inst(actor, merchant_6), merchant)).
st(root_2, property(type, inst(actor, merchant_6), merchant)).
st(root_3, property(type, inst(actor, merchant_6), merchant)).
st(root_1, property(type, inst(actor, aristocrat_7), noble)).
st(root_2, property(type, inst(actor, aristocrat_7), noble)).
st(root_3, property(type, inst(actor, aristocrat_7), noble)).
st(root_1, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_2, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_3, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_1, property(determined, inst(actor, merchant_6), true)).
st(root_2, property(determined, inst(actor, merchant_6), true)).
st(root_3, property(determined, inst(actor, merchant_6), true)).
st(root_1, property(determined, inst(actor, aristocrat_7), true)).
st(root_2, property(determined, inst(actor, aristocrat_7), true)).
st(root_3, property(determined, inst(actor, aristocrat_7), true)).
st(root_1, property(determined, inst(actor, innkeeper_9), true)).
st(root_2, property(determined, inst(actor, innkeeper_9), true)).
st(root_3, property(determined, inst(actor, innkeeper_9), true)).
st(root_1, property(person, inst(actor, merchant_6), third)).
st(root_2, property(person, inst(actor, merchant_6), third)).
st(root_3, property(person, inst(actor, merchant_6), third)).
st(root_1, property(person, inst(actor, aristocrat_7), third)).
st(root_2, property(person, inst(actor, aristocrat_7), third)).
st(root_3, property(person, inst(actor, aristocrat_7), third)).
st(root_1, property(person, inst(actor, innkeeper_9), third)).
st(root_2, property(person, inst(actor, innkeeper_9), third)).
st(root_3, property(person, inst(actor, innkeeper_9), third)).
st(root_1, property(gender, inst(actor, merchant_6), masculine)).
st(root_2, property(gender, inst(actor, merchant_6), masculine)).
st(root_3, property(gender, inst(actor, merchant_6), masculine)).
st(root_1, property(gender, inst(actor, aristocrat_7), feminine)).
st(root_2, property(gender, inst(actor, aristocrat_7), feminine)).
st(root_3, property(gender, inst(actor, aristocrat_7), feminine)).
st(root_1, property(gender, inst(actor, innkeeper_9), masculine)).
st(root_2, property(gender, inst(actor, innkeeper_9), masculine)).
st(root_3, property(gender, inst(actor, innkeeper_9), masculine)).
st(root_1, property(number, inst(actor, merchant_6), singular)).
st(root_2, property(number, inst(actor, merchant_6), singular)).
st(root_3, property(number, inst(actor, merchant_6), singular)).
st(root_1, property(number, inst(actor, aristocrat_7), singular)).
st(root_2, property(number, inst(actor, aristocrat_7), singular)).
st(root_3, property(number, inst(actor, aristocrat_7), singular)).
st(root_1, property(number, inst(actor, innkeeper_9), singular)).
st(root_2, property(number, inst(actor, innkeeper_9), singular)).
st(root_3, property(number, inst(actor, innkeeper_9), singular)).
st(root_1, property(name, inst(actor, merchant_6), "merchant")).
st(root_2, property(name, inst(actor, merchant_6), "merchant")).
st(root_3, property(name, inst(actor, merchant_6), "merchant")).
st(root_1, property(name, inst(actor, aristocrat_7), "noble")).
st(root_2, property(name, inst(actor, aristocrat_7), "noble")).
st(root_3, property(name, inst(actor, aristocrat_7), "noble")).
st(root_1, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_2, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_3, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_1, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_2, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_3, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_1, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_2, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_3, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_1, property(has_skill, inst(actor, merchant_6), storytelling)).
st(root_2, property(has_skill, inst(actor, merchant_6), storytelling)).
st(root_3, property(has_skill, inst(actor, merchant_6), storytelling)).
st(root_1, property(has_skill, inst(actor, innkeeper_9), storytelling)).
st(root_2, property(has_skill, inst(actor, innkeeper_9), storytelling)).
st(root_3, property(has_skill, inst(actor, innkeeper_9), storytelling)).
st(root_1, property(type, inst(item, perfume_66), perfume)).
st(root_2, property(type, inst(item, perfume_66), perfume)).
st(root_3, property(type, inst(item, perfume_66), perfume)).
st(root_1, property(type, inst(item, medicine_chest_68), medicine_chest)).
st(root_2, property(type, inst(item, medicine_chest_68), medicine_chest)).
st(root_3, property(type, inst(item, medicine_chest_68), medicine_chest)).
st(root_1, property(type, inst(item, medicine_book_72), medicine_book)).
st(root_2, property(type, inst(item, medicine_book_72), medicine_book)).
st(root_3, property(type, inst(item, medicine_book_72), medicine_book)).
st(root_1, property(type, inst(item, perfume_116), perfume)).
st(root_2, property(type, inst(item, perfume_116), perfume)).
st(root_3, property(type, inst(item, perfume_116), perfume)).
st(root_1, property(type, inst(item, music_book_119), music_book)).
st(root_2, property(type, inst(item, music_book_119), music_book)).
st(root_3, property(type, inst(item, music_book_119), music_book)).
st(root_1, relation(has_item, inst(actor, merchant_6), inst(item, perfume_66))).
st(root_2, relation(has_item, inst(actor, merchant_6), inst(item, perfume_66))).
st(root_3, relation(has_item, inst(actor, merchant_6), inst(item, perfume_66))).
st(root_1, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68))).
st(root_2, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68))).
st(root_3, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68))).
st(root_1, relation(has_item, inst(actor, merchant_6), inst(item, medicine_book_72))).
st(root_2, relation(has_item, inst(actor, merchant_6), inst(item, medicine_book_72))).
st(root_3, relation(has_item, inst(actor, merchant_6), inst(item, medicine_book_72))).
st(root_1, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116))).
st(root_2, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116))).
st(root_3, relation(has_item, inst(actor, aristocrat_7), inst(item, perfume_116))).
st(root_1, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119))).
st(root_2, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119))).
st(root_3, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119))).
st(root_1, inst(item, perfume_66)).
st(root_2, inst(item, perfume_66)).
st(root_3, inst(item, perfume_66)).
st(root_1, inst(item, medicine_chest_68)).
st(root_2, inst(item, medicine_chest_68)).
st(root_3, inst(item, medicine_chest_68)).
st(root_1, inst(item, medicine_book_72)).
st(root_2, inst(item, medicine_book_72)).
st(root_3, inst(item, medicine_book_72)).
st(root_1, inst(item, perfume_116)).
st(root_2, inst(item, perfume_116)).
st(root_3, inst(item, perfume_116)).
st(root_1, inst(item, music_book_119)).
st(root_2, inst(item, music_book_119)).
st(root_3, inst(item, music_book_119)).
st(root_1, property(gender, inst(item, perfume_66), neuter)).
st(root_2, property(gender, inst(item, perfume_66), neuter)).
st(root_3, property(gender, inst(item, perfume_66), neuter)).
st(root_1, property(gender, inst(item, medicine_chest_68), neuter)).
st(root_2, property(gender, inst(item, medicine_chest_68), neuter)).
st(root_3, property(gender, inst(item, medicine_chest_68), neuter)).
st(root_1, property(gender, inst(item, medicine_book_72), neuter)).
st(root_2, property(gender, inst(item, medicine_book_72), neuter)).
st(root_3, property(gender, inst(item, medicine_book_72), neuter)).
st(root_1, property(gender, inst(item, perfume_116), neuter)).
st(root_2, property(gender, inst(item, perfume_116), neuter)).
st(root_3, property(gender, inst(item, perfume_116), neuter)).
st(root_1, property(gender, inst(item, music_book_119), neuter)).
st(root_2, property(gender, inst(item, music_book_119), neuter)).
st(root_3, property(gender, inst(item, music_book_119), neuter)).
st(root_1, property(number, inst(item, perfume_66), plural)).
st(root_2, property(number, inst(item, perfume_66), plural)).
st(root_3, property(number, inst(item, perfume_66), plural)).
st(root_1, property(number, inst(item, medicine_chest_68), singular)).
st(root_2, property(number, inst(item, medicine_chest_68), singular)).
st(root_3, property(number, inst(item, medicine_chest_68), singular)).
st(root_1, property(number, inst(item, medicine_book_72), singular)).
st(root_2, property(number, inst(item, medicine_book_72), singular)).
st(root_3, property(number, inst(item, medicine_book_72), singular)).
st(root_1, property(number, inst(item, perfume_116), plural)).
st(root_2, property(number, inst(item, perfume_116), plural)).
st(root_3, property(number, inst(item, perfume_116), plural)).
st(root_1, property(number, inst(item, music_book_119), singular)).
st(root_2, property(number, inst(item, music_book_119), singular)).
st(root_3, property(number, inst(item, music_book_119), singular)).
st(root_1, property(name, inst(item, perfume_66), "perfume")).
st(root_2, property(name, inst(item, perfume_66), "perfume")).
st(root_3, property(name, inst(item, perfume_66), "perfume")).
st(root_1, property(name, inst(item, medicine_chest_68), "medicine chest")).
st(root_2, property(name, inst(item, medicine_chest_68), "medicine chest")).
st(root_3, property(name, inst(item, medicine_chest_68), "medicine chest")).
st(root_1, property(name, inst(item, medicine_book_72), "book of medicine")).
st(root_2, property(name, inst(item, medicine_book_72), "book of medicine")).
st(root_3, property(name, inst(item, medicine_book_72), "book of medicine")).
st(root_1, property(name, inst(item, perfume_116), "perfume")).
st(root_2, property(name, inst(item, perfume_116), "perfume")).
st(root_3, property(name, inst(item, perfume_116), "perfume")).
st(root_1, property(name, inst(item, music_book_119), "music book")).
st(root_2, property(name, inst(item, music_book_119), "music book")).
st(root_3, property(name, inst(item, music_book_119), "music book")).
st(root_1, property(determined, inst(item, perfume_66), true)).
st(root_2, property(determined, inst(item, perfume_66), true)).
st(root_3, property(determined, inst(item, perfume_66), true)).
st(root_1, property(determined, inst(item, medicine_chest_68), true)).
st(root_2, property(determined, inst(item, medicine_chest_68), true)).
st(root_3, property(determined, inst(item, medicine_chest_68), true)).
st(root_1, property(determined, inst(item, medicine_book_72), true)).
st(root_2, property(determined, inst(item, medicine_book_72), true)).
st(root_3, property(determined, inst(item, medicine_book_72), true)).
st(root_1, property(determined, inst(item, perfume_116), true)).
st(root_2, property(determined, inst(item, perfume_116), true)).
st(root_3, property(determined, inst(item, perfume_116), true)).
st(root_1, property(determined, inst(item, music_book_119), true)).
st(root_2, property(determined, inst(item, music_book_119), true)).
st(root_3, property(determined, inst(item, music_book_119), true)).
st(root_1, property(person, inst(item, perfume_66), third)).
st(root_2, property(person, inst(item, perfume_66), third)).
st(root_3, property(person, inst(item, perfume_66), third)).
st(root_1, property(person, inst(item, medicine_chest_68), third)).
st(root_2, property(person, inst(item, medicine_chest_68), third)).
st(root_3, property(person, inst(item, medicine_chest_68), third)).
st(root_1, property(person, inst(item, medicine_book_72), third)).
st(root_2, property(person, inst(item, medicine_book_72), third)).
st(root_3, property(person, inst(item, medicine_book_72), third)).
st(root_1, property(person, inst(item, perfume_116), third)).
st(root_2, property(person, inst(item, perfume_116), third)).
st(root_3, property(person, inst(item, perfume_116), third)).
st(root_1, property(person, inst(item, music_book_119), third)).
st(root_2, property(person, inst(item, music_book_119), third)).
st(root_3, property(person, inst(item, music_book_119), third)).
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
path_length(root_1, 1).
path_length(root_2, 1).
path_length(root_3, 1).
at(root_1, is_powerful(inst(actor, aristocrat_7))).
at(root_2, is_powerful(inst(actor, aristocrat_7))).
at(root_3, is_powerful(inst(actor, aristocrat_7))).
at(root_1, is_powerless(inst(actor, merchant_6))).
at(root_2, is_powerless(inst(actor, merchant_6))).
at(root_3, is_powerless(inst(actor, merchant_6))).
at(root_1, is_powerless(inst(actor, innkeeper_9))).
at(root_2, is_powerless(inst(actor, innkeeper_9))).
at(root_3, is_powerless(inst(actor, innkeeper_9))).
at(root_1, text_mode(example)).
at(root_2, text_mode(example)).
at(root_3, text_mode(example)).
max_unique(119).
