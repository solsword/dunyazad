experiment_template(relaxed).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
setup_priority(on_sale, 2).
setup_priority(tavern, 1).
setup_priority(threatened_innocents, 3).
setup_priority(market, 6).
setup_priority(monster_attack, 4).
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
setup(root, tavern).
at(root, setup_arg(merchant, inst(actor, merchant_6))).
at(root, setup_arg(noble, inst(actor, aristocrat_7))).
at(root, setup_arg(peasant, inst(actor, laborer_8))).
at(root, setup_arg(innkeeper, inst(actor, innkeeper_9))).
st(root, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root, property(number, inst(item, dragon_scale_23), singular)).
st(root, property(number, inst(item, plants_book_26), singular)).
st(root, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root, property(type, inst(item, plants_book_26), plants_book)).
st(root, inst(item, dragon_scale_23)).
st(root, inst(item, plants_book_26)).
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
at(root, arg(option(1), buyer, inst(actor, you))).
at(root, action(option(1), trade)).
at(root, arg(option(1), price, inst(item, dragon_scale_23))).
at(root, consequence(option(3), _not, state(bored, inst(actor, aristocrat_7)))).
at(root, consequence(option(2), _not, state(bored, inst(actor, laborer_8)))).
at(root, consequence_of(option(1), o(deal, deal), _not, relation(has_item, inst(actor, you), inst(item, dragon_scale_23)))).
spontaneous(st(root, property(gender, inst(actor, merchant_6), masculine))).
spontaneous(st(root, property(gender, inst(actor, aristocrat_7), feminine))).
spontaneous(st(root, property(gender, inst(actor, laborer_8), masculine))).
spontaneous(st(root, property(gender, inst(actor, innkeeper_9), feminine))).
spontaneous(st(root, property(number, inst(actor, merchant_6), singular))).
spontaneous(st(root, property(number, inst(actor, aristocrat_7), singular))).
spontaneous(st(root, property(number, inst(actor, laborer_8), singular))).
spontaneous(st(root, property(number, inst(actor, innkeeper_9), singular))).
spontaneous(st(root, property(name, inst(actor, merchant_6), "merchant"))).
spontaneous(st(root, property(name, inst(actor, aristocrat_7), "noble"))).
spontaneous(st(root, property(name, inst(actor, laborer_8), "peasant"))).
spontaneous(st(root, property(name, inst(actor, innkeeper_9), "innkeeper"))).
st(root, property(gender, inst(actor, merchant_6), masculine)).
st(root, property(gender, inst(actor, aristocrat_7), feminine)).
st(root, property(gender, inst(actor, laborer_8), masculine)).
st(root, property(gender, inst(actor, innkeeper_9), feminine)).
st(root, property(number, inst(actor, merchant_6), singular)).
st(root, property(number, inst(actor, aristocrat_7), singular)).
st(root, property(number, inst(actor, laborer_8), singular)).
st(root, property(number, inst(actor, innkeeper_9), singular)).
st(root, property(name, inst(actor, merchant_6), "merchant")).
st(root, property(name, inst(actor, aristocrat_7), "noble")).
st(root, property(name, inst(actor, laborer_8), "peasant")).
st(root, property(name, inst(actor, innkeeper_9), "innkeeper")).
spontaneous(st(root, property(has_skill, inst(actor, merchant_6), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, aristocrat_7), literacy))).
st(root, property(has_skill, inst(actor, merchant_6), literacy)).
st(root, property(has_skill, inst(actor, aristocrat_7), literacy)).
at(root, consequence_of(option(1), o(deal, deal), relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_23)))).
at(root, arg(option(1), seller, inst(actor, merchant_6))).
at(root, arg(option(2), audience, inst(actor, laborer_8))).
at(root, arg(option(3), audience, inst(actor, aristocrat_7))).
at(root, action(option(2), tell_story)).
at(root, consequence_of(option(2), o(quality, boring), nullifies, potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, action(option(3), tell_story)).
at(root, consequence_of(option(3), o(quality, boring), nullifies, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, consequence_of(option(2), o(quality, entertaining), resolves, potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, consequence_of(option(3), o(quality, entertaining), resolves, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, consequence(option(2), resolves, potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, consequence(option(3), resolves, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, outcome(option(2), o(quality, entertaining))).
at(root, outcome(option(3), o(quality, entertaining))).
spontaneous(st(root, property(has_skill, inst(actor, laborer_8), wilderness_lore))).
st(root, property(has_skill, inst(actor, laborer_8), wilderness_lore)).
at(root, consequence_of(option(2), o(quality, boring), _not, state(bored, inst(actor, laborer_8)))).
at(root, consequence_of(option(3), o(quality, boring), _not, state(bored, inst(actor, aristocrat_7)))).
at(root, consequence_of(option(2), o(quality, entertaining), _not, state(bored, inst(actor, laborer_8)))).
at(root, consequence_of(option(3), o(quality, entertaining), _not, state(bored, inst(actor, aristocrat_7)))).
spontaneous(st(root, property(type, inst(item, dragon_scale_64), dragon_scale))).
spontaneous(st(root, property(type, inst(item, medicine_chest_68), medicine_chest))).
spontaneous(st(root, property(type, inst(item, plants_book_69), plants_book))).
spontaneous(st(root, property(type, inst(item, dragon_scale_115), dragon_scale))).
spontaneous(st(root, property(type, inst(item, music_book_119), music_book))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64)))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68)))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_6), inst(item, plants_book_69)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, dragon_scale_115)))).
spontaneous(st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119)))).
spontaneous(st(root, inst(item, dragon_scale_64))).
spontaneous(st(root, inst(item, medicine_chest_68))).
spontaneous(st(root, inst(item, plants_book_69))).
spontaneous(st(root, inst(item, dragon_scale_115))).
spontaneous(st(root, inst(item, music_book_119))).
st(root, property(type, inst(item, dragon_scale_64), dragon_scale)).
st(root, property(type, inst(item, medicine_chest_68), medicine_chest)).
st(root, property(type, inst(item, plants_book_69), plants_book)).
st(root, property(type, inst(item, dragon_scale_115), dragon_scale)).
st(root, property(type, inst(item, music_book_119), music_book)).
st(root, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68))).
st(root, relation(has_item, inst(actor, merchant_6), inst(item, plants_book_69))).
st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, dragon_scale_115))).
st(root, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119))).
st(root, inst(item, dragon_scale_64)).
st(root, inst(item, medicine_chest_68)).
st(root, inst(item, plants_book_69)).
st(root, inst(item, dragon_scale_115)).
st(root, inst(item, music_book_119)).
spontaneous(st(root, property(gender, inst(item, dragon_scale_64), neuter))).
spontaneous(st(root, property(gender, inst(item, medicine_chest_68), neuter))).
spontaneous(st(root, property(gender, inst(item, plants_book_69), neuter))).
spontaneous(st(root, property(gender, inst(item, dragon_scale_115), neuter))).
spontaneous(st(root, property(gender, inst(item, music_book_119), neuter))).
spontaneous(st(root, property(number, inst(item, dragon_scale_64), singular))).
spontaneous(st(root, property(number, inst(item, medicine_chest_68), singular))).
spontaneous(st(root, property(number, inst(item, plants_book_69), singular))).
spontaneous(st(root, property(number, inst(item, dragon_scale_115), singular))).
spontaneous(st(root, property(number, inst(item, music_book_119), singular))).
spontaneous(st(root, property(name, inst(item, dragon_scale_64), "dragon scale"))).
spontaneous(st(root, property(name, inst(item, medicine_chest_68), "medicine chest"))).
spontaneous(st(root, property(name, inst(item, plants_book_69), "book of herbal lore"))).
spontaneous(st(root, property(name, inst(item, dragon_scale_115), "dragon scale"))).
spontaneous(st(root, property(name, inst(item, music_book_119), "music book"))).
spontaneous(st(root, property(determined, inst(item, dragon_scale_64), true))).
spontaneous(st(root, property(determined, inst(item, medicine_chest_68), true))).
spontaneous(st(root, property(determined, inst(item, plants_book_69), true))).
spontaneous(st(root, property(determined, inst(item, dragon_scale_115), true))).
spontaneous(st(root, property(determined, inst(item, music_book_119), true))).
spontaneous(st(root, property(person, inst(item, dragon_scale_64), third))).
spontaneous(st(root, property(person, inst(item, medicine_chest_68), third))).
spontaneous(st(root, property(person, inst(item, plants_book_69), third))).
spontaneous(st(root, property(person, inst(item, dragon_scale_115), third))).
spontaneous(st(root, property(person, inst(item, music_book_119), third))).
st(root, property(gender, inst(item, dragon_scale_64), neuter)).
st(root, property(gender, inst(item, medicine_chest_68), neuter)).
st(root, property(gender, inst(item, plants_book_69), neuter)).
st(root, property(gender, inst(item, dragon_scale_115), neuter)).
st(root, property(gender, inst(item, music_book_119), neuter)).
st(root, property(number, inst(item, dragon_scale_64), singular)).
st(root, property(number, inst(item, medicine_chest_68), singular)).
st(root, property(number, inst(item, plants_book_69), singular)).
st(root, property(number, inst(item, dragon_scale_115), singular)).
st(root, property(number, inst(item, music_book_119), singular)).
st(root, property(name, inst(item, dragon_scale_64), "dragon scale")).
st(root, property(name, inst(item, medicine_chest_68), "medicine chest")).
st(root, property(name, inst(item, plants_book_69), "book of herbal lore")).
st(root, property(name, inst(item, dragon_scale_115), "dragon scale")).
st(root, property(name, inst(item, music_book_119), "music book")).
st(root, property(determined, inst(item, dragon_scale_64), true)).
st(root, property(determined, inst(item, medicine_chest_68), true)).
st(root, property(determined, inst(item, plants_book_69), true)).
st(root, property(determined, inst(item, dragon_scale_115), true)).
st(root, property(determined, inst(item, music_book_119), true)).
st(root, property(person, inst(item, dragon_scale_64), third)).
st(root, property(person, inst(item, medicine_chest_68), third)).
st(root, property(person, inst(item, plants_book_69), third)).
st(root, property(person, inst(item, dragon_scale_115), third)).
st(root, property(person, inst(item, music_book_119), third)).
at(root, consequence_of(option(1), o(deal, deal), relation(has_item, inst(actor, you), inst(item, plants_book_69)))).
at(root, arg(option(1), goods, inst(item, plants_book_69))).
at(root, setup_arg(goods, inst(item, plants_book_69))).
at(root, consequence_of(option(1), o(deal, deal), _not, relation(has_item, inst(actor, merchant_6), inst(item, plants_book_69)))).
spontaneous(st(root, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69)))).
st(root, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))).
at(root, potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69)))).
at(root, consequence_of(option(1), o(deal, deal), resolves, potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))))).
at(root, consequence_of(option(1), o(deal, deal), _not, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69)))).
at(root, arg(option(2), teller, inst(actor, you))).
at(root, arg(option(3), teller, inst(actor, you))).
st(root, property(has_skill, inst(actor, you), wilderness_lore)).
st(root, property(has_skill, inst(actor, you), storytelling)).
at(root, outcome(option(1), o(deal, no_deal))).
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
at(root, has_tool_for(inst(actor, you), healing)).
at(root, has_tool_for(inst(actor, you), wilderness_lore)).
at(root, has_tool_for(inst(actor, merchant_6), healing)).
at(root, has_tool_for(inst(actor, merchant_6), wilderness_lore)).
setting(root, road).
at(root, skill_link(wilderness_lore, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(wilderness_lore, contest, no_tool, pursue, between(subject, object), either(o(result, caught_up), o(result, lost)))).
at(root, likely_outcome(option(2), o(quality, entertaining))).
at(root, likely_outcome(option(3), o(quality, entertaining))).
node_status_reached(root, initialized).
node_status_reached(root, built).
node_status(root, built).
at(root, relevant_to(option(3), potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, relevant_to(option(2), potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, relevant_to(option(1), potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))))).
at(root, importance(8, potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, importance(8, potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, importance(8, potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))))).
at(root, some_importance(8)).
at(root, most_important(potential(opportunity, state(bored, inst(actor, aristocrat_7))))).
at(root, most_important(potential(opportunity, state(bored, inst(actor, laborer_8))))).
at(root, most_important(potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))))).
at(root, motivated(option(1))).
at(root, motivated(option(2))).
at(root, motivated(option(3))).
at(root, expectation(option(1), enables, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, initiated_by(potential(opportunity, state(bored, inst(actor, aristocrat_7))), inst(actor, aristocrat_7))).
at(root, initiated_by(potential(opportunity, state(bored, inst(actor, laborer_8))), inst(actor, laborer_8))).
at(root, initiated_by(potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))), inst(actor, merchant_6))).
at(root, can_trade(inst(actor, you), inst(item, dragon_scale_23))).
at(root, can_trade(inst(actor, you), inst(item, plants_book_26))).
at(root, can_trade(inst(actor, merchant_6), inst(item, dragon_scale_64))).
at(root, can_trade(inst(actor, merchant_6), inst(item, medicine_chest_68))).
at(root, can_trade(inst(actor, merchant_6), inst(item, plants_book_69))).
at(root, can_trade(inst(actor, aristocrat_7), inst(item, dragon_scale_115))).
at(root, can_trade(inst(actor, aristocrat_7), inst(item, music_book_119))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), wilderness_lore))).
at(root, player_goal(have_tool_for(inst(actor, you), storytelling))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(3), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, stakes(as_intended(inst(actor, you)), low)).
at(root, stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root, stakes(low)).
at(root, option_feel(option(1), safe)).
at(root, option_feel(option(2), safe)).
at(root, option_feel(option(3), safe)).
at(root, choice_structure(relaxed)).
at(root, relevant_skill(option(2), inst(actor, you), has, storytelling)).
at(root, relevant_skill(option(3), inst(actor, you), has, storytelling)).
at(root, is_powerful(inst(actor, aristocrat_7))).
at(root, is_powerless(inst(actor, merchant_6))).
at(root, is_powerless(inst(actor, laborer_8))).
at(root, is_powerless(inst(actor, innkeeper_9))).
at(root, option_feel(option(2), sure_thing)).
at(root, option_feel(option(3), sure_thing)).
at(root, outcome_perception(option(2), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(3), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(1), fails, as_intended(inst(actor, you)))).
unresolved_potential(root, option(1), potential(opportunity, state(bored, inst(actor, aristocrat_7)))).
unresolved_potential(root, option(1), potential(opportunity, state(bored, inst(actor, laborer_8)))).
unresolved_potential(root, option(1), potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69)))).
unresolved_potential(root, option(2), potential(opportunity, state(bored, inst(actor, aristocrat_7)))).
unresolved_potential(root, option(2), potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69)))).
unresolved_potential(root, option(3), potential(opportunity, state(bored, inst(actor, laborer_8)))).
unresolved_potential(root, option(3), potential(opportunity, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69)))).
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
st(root_1, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root_2, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root_3, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root_1, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_2, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_3, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_1, property(number, inst(item, dragon_scale_23), singular)).
st(root_2, property(number, inst(item, dragon_scale_23), singular)).
st(root_3, property(number, inst(item, dragon_scale_23), singular)).
st(root_1, property(number, inst(item, plants_book_26), singular)).
st(root_2, property(number, inst(item, plants_book_26), singular)).
st(root_3, property(number, inst(item, plants_book_26), singular)).
st(root_1, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root_2, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root_3, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root_1, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_2, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_3, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_1, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root_2, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root_3, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root_1, property(type, inst(item, plants_book_26), plants_book)).
st(root_2, property(type, inst(item, plants_book_26), plants_book)).
st(root_3, property(type, inst(item, plants_book_26), plants_book)).
st(root_1, inst(item, dragon_scale_23)).
st(root_2, inst(item, dragon_scale_23)).
st(root_3, inst(item, dragon_scale_23)).
st(root_1, inst(item, plants_book_26)).
st(root_2, inst(item, plants_book_26)).
st(root_3, inst(item, plants_book_26)).
st(root_1, inst(actor, merchant_6)).
st(root_2, inst(actor, merchant_6)).
st(root_3, inst(actor, merchant_6)).
st(root_1, inst(actor, aristocrat_7)).
st(root_2, inst(actor, aristocrat_7)).
st(root_3, inst(actor, aristocrat_7)).
st(root_1, inst(actor, laborer_8)).
st(root_2, inst(actor, laborer_8)).
st(root_3, inst(actor, laborer_8)).
st(root_1, inst(actor, innkeeper_9)).
st(root_2, inst(actor, innkeeper_9)).
st(root_3, inst(actor, innkeeper_9)).
st(root_1, state(bored, inst(actor, aristocrat_7))).
st(root_2, state(bored, inst(actor, aristocrat_7))).
st(root_1, state(bored, inst(actor, laborer_8))).
st(root_3, state(bored, inst(actor, laborer_8))).
st(root_1, property(type, inst(actor, merchant_6), merchant)).
st(root_2, property(type, inst(actor, merchant_6), merchant)).
st(root_3, property(type, inst(actor, merchant_6), merchant)).
st(root_1, property(type, inst(actor, aristocrat_7), noble)).
st(root_2, property(type, inst(actor, aristocrat_7), noble)).
st(root_3, property(type, inst(actor, aristocrat_7), noble)).
st(root_1, property(type, inst(actor, laborer_8), peasant)).
st(root_2, property(type, inst(actor, laborer_8), peasant)).
st(root_3, property(type, inst(actor, laborer_8), peasant)).
st(root_1, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_2, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_3, property(type, inst(actor, innkeeper_9), innkeeper)).
st(root_1, property(determined, inst(actor, merchant_6), true)).
st(root_2, property(determined, inst(actor, merchant_6), true)).
st(root_3, property(determined, inst(actor, merchant_6), true)).
st(root_1, property(determined, inst(actor, aristocrat_7), true)).
st(root_2, property(determined, inst(actor, aristocrat_7), true)).
st(root_3, property(determined, inst(actor, aristocrat_7), true)).
st(root_1, property(determined, inst(actor, laborer_8), true)).
st(root_2, property(determined, inst(actor, laborer_8), true)).
st(root_3, property(determined, inst(actor, laborer_8), true)).
st(root_1, property(determined, inst(actor, innkeeper_9), true)).
st(root_2, property(determined, inst(actor, innkeeper_9), true)).
st(root_3, property(determined, inst(actor, innkeeper_9), true)).
st(root_1, property(person, inst(actor, merchant_6), third)).
st(root_2, property(person, inst(actor, merchant_6), third)).
st(root_3, property(person, inst(actor, merchant_6), third)).
st(root_1, property(person, inst(actor, aristocrat_7), third)).
st(root_2, property(person, inst(actor, aristocrat_7), third)).
st(root_3, property(person, inst(actor, aristocrat_7), third)).
st(root_1, property(person, inst(actor, laborer_8), third)).
st(root_2, property(person, inst(actor, laborer_8), third)).
st(root_3, property(person, inst(actor, laborer_8), third)).
st(root_1, property(person, inst(actor, innkeeper_9), third)).
st(root_2, property(person, inst(actor, innkeeper_9), third)).
st(root_3, property(person, inst(actor, innkeeper_9), third)).
st(root_1, property(gender, inst(actor, merchant_6), masculine)).
st(root_2, property(gender, inst(actor, merchant_6), masculine)).
st(root_3, property(gender, inst(actor, merchant_6), masculine)).
st(root_1, property(gender, inst(actor, aristocrat_7), feminine)).
st(root_2, property(gender, inst(actor, aristocrat_7), feminine)).
st(root_3, property(gender, inst(actor, aristocrat_7), feminine)).
st(root_1, property(gender, inst(actor, laborer_8), masculine)).
st(root_2, property(gender, inst(actor, laborer_8), masculine)).
st(root_3, property(gender, inst(actor, laborer_8), masculine)).
st(root_1, property(gender, inst(actor, innkeeper_9), feminine)).
st(root_2, property(gender, inst(actor, innkeeper_9), feminine)).
st(root_3, property(gender, inst(actor, innkeeper_9), feminine)).
st(root_1, property(number, inst(actor, merchant_6), singular)).
st(root_2, property(number, inst(actor, merchant_6), singular)).
st(root_3, property(number, inst(actor, merchant_6), singular)).
st(root_1, property(number, inst(actor, aristocrat_7), singular)).
st(root_2, property(number, inst(actor, aristocrat_7), singular)).
st(root_3, property(number, inst(actor, aristocrat_7), singular)).
st(root_1, property(number, inst(actor, laborer_8), singular)).
st(root_2, property(number, inst(actor, laborer_8), singular)).
st(root_3, property(number, inst(actor, laborer_8), singular)).
st(root_1, property(number, inst(actor, innkeeper_9), singular)).
st(root_2, property(number, inst(actor, innkeeper_9), singular)).
st(root_3, property(number, inst(actor, innkeeper_9), singular)).
st(root_1, property(name, inst(actor, merchant_6), "merchant")).
st(root_2, property(name, inst(actor, merchant_6), "merchant")).
st(root_3, property(name, inst(actor, merchant_6), "merchant")).
st(root_1, property(name, inst(actor, aristocrat_7), "noble")).
st(root_2, property(name, inst(actor, aristocrat_7), "noble")).
st(root_3, property(name, inst(actor, aristocrat_7), "noble")).
st(root_1, property(name, inst(actor, laborer_8), "peasant")).
st(root_2, property(name, inst(actor, laborer_8), "peasant")).
st(root_3, property(name, inst(actor, laborer_8), "peasant")).
st(root_1, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_2, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_3, property(name, inst(actor, innkeeper_9), "innkeeper")).
st(root_1, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_2, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_3, property(has_skill, inst(actor, merchant_6), literacy)).
st(root_1, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_2, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_3, property(has_skill, inst(actor, aristocrat_7), literacy)).
st(root_1, property(has_skill, inst(actor, laborer_8), wilderness_lore)).
st(root_2, property(has_skill, inst(actor, laborer_8), wilderness_lore)).
st(root_3, property(has_skill, inst(actor, laborer_8), wilderness_lore)).
st(root_1, property(type, inst(item, dragon_scale_64), dragon_scale)).
st(root_2, property(type, inst(item, dragon_scale_64), dragon_scale)).
st(root_3, property(type, inst(item, dragon_scale_64), dragon_scale)).
st(root_1, property(type, inst(item, medicine_chest_68), medicine_chest)).
st(root_2, property(type, inst(item, medicine_chest_68), medicine_chest)).
st(root_3, property(type, inst(item, medicine_chest_68), medicine_chest)).
st(root_1, property(type, inst(item, plants_book_69), plants_book)).
st(root_2, property(type, inst(item, plants_book_69), plants_book)).
st(root_3, property(type, inst(item, plants_book_69), plants_book)).
st(root_1, property(type, inst(item, dragon_scale_115), dragon_scale)).
st(root_2, property(type, inst(item, dragon_scale_115), dragon_scale)).
st(root_3, property(type, inst(item, dragon_scale_115), dragon_scale)).
st(root_1, property(type, inst(item, music_book_119), music_book)).
st(root_2, property(type, inst(item, music_book_119), music_book)).
st(root_3, property(type, inst(item, music_book_119), music_book)).
st(root_1, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root_2, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root_3, relation(has_item, inst(actor, merchant_6), inst(item, dragon_scale_64))).
st(root_1, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68))).
st(root_2, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68))).
st(root_3, relation(has_item, inst(actor, merchant_6), inst(item, medicine_chest_68))).
st(root_1, relation(has_item, inst(actor, merchant_6), inst(item, plants_book_69))).
st(root_2, relation(has_item, inst(actor, merchant_6), inst(item, plants_book_69))).
st(root_3, relation(has_item, inst(actor, merchant_6), inst(item, plants_book_69))).
st(root_1, relation(has_item, inst(actor, aristocrat_7), inst(item, dragon_scale_115))).
st(root_2, relation(has_item, inst(actor, aristocrat_7), inst(item, dragon_scale_115))).
st(root_3, relation(has_item, inst(actor, aristocrat_7), inst(item, dragon_scale_115))).
st(root_1, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119))).
st(root_2, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119))).
st(root_3, relation(has_item, inst(actor, aristocrat_7), inst(item, music_book_119))).
st(root_1, inst(item, dragon_scale_64)).
st(root_2, inst(item, dragon_scale_64)).
st(root_3, inst(item, dragon_scale_64)).
st(root_1, inst(item, medicine_chest_68)).
st(root_2, inst(item, medicine_chest_68)).
st(root_3, inst(item, medicine_chest_68)).
st(root_1, inst(item, plants_book_69)).
st(root_2, inst(item, plants_book_69)).
st(root_3, inst(item, plants_book_69)).
st(root_1, inst(item, dragon_scale_115)).
st(root_2, inst(item, dragon_scale_115)).
st(root_3, inst(item, dragon_scale_115)).
st(root_1, inst(item, music_book_119)).
st(root_2, inst(item, music_book_119)).
st(root_3, inst(item, music_book_119)).
st(root_1, property(gender, inst(item, dragon_scale_64), neuter)).
st(root_2, property(gender, inst(item, dragon_scale_64), neuter)).
st(root_3, property(gender, inst(item, dragon_scale_64), neuter)).
st(root_1, property(gender, inst(item, medicine_chest_68), neuter)).
st(root_2, property(gender, inst(item, medicine_chest_68), neuter)).
st(root_3, property(gender, inst(item, medicine_chest_68), neuter)).
st(root_1, property(gender, inst(item, plants_book_69), neuter)).
st(root_2, property(gender, inst(item, plants_book_69), neuter)).
st(root_3, property(gender, inst(item, plants_book_69), neuter)).
st(root_1, property(gender, inst(item, dragon_scale_115), neuter)).
st(root_2, property(gender, inst(item, dragon_scale_115), neuter)).
st(root_3, property(gender, inst(item, dragon_scale_115), neuter)).
st(root_1, property(gender, inst(item, music_book_119), neuter)).
st(root_2, property(gender, inst(item, music_book_119), neuter)).
st(root_3, property(gender, inst(item, music_book_119), neuter)).
st(root_1, property(number, inst(item, dragon_scale_64), singular)).
st(root_2, property(number, inst(item, dragon_scale_64), singular)).
st(root_3, property(number, inst(item, dragon_scale_64), singular)).
st(root_1, property(number, inst(item, medicine_chest_68), singular)).
st(root_2, property(number, inst(item, medicine_chest_68), singular)).
st(root_3, property(number, inst(item, medicine_chest_68), singular)).
st(root_1, property(number, inst(item, plants_book_69), singular)).
st(root_2, property(number, inst(item, plants_book_69), singular)).
st(root_3, property(number, inst(item, plants_book_69), singular)).
st(root_1, property(number, inst(item, dragon_scale_115), singular)).
st(root_2, property(number, inst(item, dragon_scale_115), singular)).
st(root_3, property(number, inst(item, dragon_scale_115), singular)).
st(root_1, property(number, inst(item, music_book_119), singular)).
st(root_2, property(number, inst(item, music_book_119), singular)).
st(root_3, property(number, inst(item, music_book_119), singular)).
st(root_1, property(name, inst(item, dragon_scale_64), "dragon scale")).
st(root_2, property(name, inst(item, dragon_scale_64), "dragon scale")).
st(root_3, property(name, inst(item, dragon_scale_64), "dragon scale")).
st(root_1, property(name, inst(item, medicine_chest_68), "medicine chest")).
st(root_2, property(name, inst(item, medicine_chest_68), "medicine chest")).
st(root_3, property(name, inst(item, medicine_chest_68), "medicine chest")).
st(root_1, property(name, inst(item, plants_book_69), "book of herbal lore")).
st(root_2, property(name, inst(item, plants_book_69), "book of herbal lore")).
st(root_3, property(name, inst(item, plants_book_69), "book of herbal lore")).
st(root_1, property(name, inst(item, dragon_scale_115), "dragon scale")).
st(root_2, property(name, inst(item, dragon_scale_115), "dragon scale")).
st(root_3, property(name, inst(item, dragon_scale_115), "dragon scale")).
st(root_1, property(name, inst(item, music_book_119), "music book")).
st(root_2, property(name, inst(item, music_book_119), "music book")).
st(root_3, property(name, inst(item, music_book_119), "music book")).
st(root_1, property(determined, inst(item, dragon_scale_64), true)).
st(root_2, property(determined, inst(item, dragon_scale_64), true)).
st(root_3, property(determined, inst(item, dragon_scale_64), true)).
st(root_1, property(determined, inst(item, medicine_chest_68), true)).
st(root_2, property(determined, inst(item, medicine_chest_68), true)).
st(root_3, property(determined, inst(item, medicine_chest_68), true)).
st(root_1, property(determined, inst(item, plants_book_69), true)).
st(root_2, property(determined, inst(item, plants_book_69), true)).
st(root_3, property(determined, inst(item, plants_book_69), true)).
st(root_1, property(determined, inst(item, dragon_scale_115), true)).
st(root_2, property(determined, inst(item, dragon_scale_115), true)).
st(root_3, property(determined, inst(item, dragon_scale_115), true)).
st(root_1, property(determined, inst(item, music_book_119), true)).
st(root_2, property(determined, inst(item, music_book_119), true)).
st(root_3, property(determined, inst(item, music_book_119), true)).
st(root_1, property(person, inst(item, dragon_scale_64), third)).
st(root_2, property(person, inst(item, dragon_scale_64), third)).
st(root_3, property(person, inst(item, dragon_scale_64), third)).
st(root_1, property(person, inst(item, medicine_chest_68), third)).
st(root_2, property(person, inst(item, medicine_chest_68), third)).
st(root_3, property(person, inst(item, medicine_chest_68), third)).
st(root_1, property(person, inst(item, plants_book_69), third)).
st(root_2, property(person, inst(item, plants_book_69), third)).
st(root_3, property(person, inst(item, plants_book_69), third)).
st(root_1, property(person, inst(item, dragon_scale_115), third)).
st(root_2, property(person, inst(item, dragon_scale_115), third)).
st(root_3, property(person, inst(item, dragon_scale_115), third)).
st(root_1, property(person, inst(item, music_book_119), third)).
st(root_2, property(person, inst(item, music_book_119), third)).
st(root_3, property(person, inst(item, music_book_119), third)).
st(root_1, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))).
st(root_2, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))).
st(root_3, relation(selling, inst(actor, merchant_6), inst(item, plants_book_69))).
st(root_1, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_2, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_3, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_1, property(has_skill, inst(actor, you), storytelling)).
st(root_2, property(has_skill, inst(actor, you), storytelling)).
st(root_3, property(has_skill, inst(actor, you), storytelling)).
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
at(root_1, is_powerless(inst(actor, laborer_8))).
at(root_2, is_powerless(inst(actor, laborer_8))).
at(root_3, is_powerless(inst(actor, laborer_8))).
at(root_1, is_powerless(inst(actor, innkeeper_9))).
at(root_2, is_powerless(inst(actor, innkeeper_9))).
at(root_3, is_powerless(inst(actor, innkeeper_9))).
at(root_1, text_mode(example)).
at(root_2, text_mode(example)).
at(root_3, text_mode(example)).
max_unique(119).
