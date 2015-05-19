experiment_template(obvious).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
setup_priority(on_sale, 5).
setup_priority(tavern, 6).
setup_priority(threatened_innocents, 1).
setup_priority(market, 4).
setup_priority(monster_attack, 2).
setup_priority(healer, 3).
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
setup(root, threatened_innocents).
at(root, arg(option(3), subject, inst(actor, you))).
at(root, action(option(3), travel_onwards)).
at(root, setup_arg(goods, inst(item, item_10))).
at(root, setup_arg(attacker, inst(actor, tough_11))).
at(root, setup_arg(victim, inst(actor, businessperson_12))).
at(root, setup_arg(goods, inst(item, treasure_13))).
st(root, relation(has_item, inst(actor, you), inst(item, perfume_24))).
st(root, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root, property(number, inst(item, perfume_24), plural)).
st(root, property(number, inst(item, plants_book_26), singular)).
st(root, property(name, inst(item, perfume_24), "perfume")).
st(root, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root, property(type, inst(item, perfume_24), perfume)).
st(root, property(type, inst(item, plants_book_26), plants_book)).
st(root, inst(item, perfume_24)).
st(root, inst(item, plants_book_26)).
spontaneous(st(root, inst(item, item_10))).
spontaneous(st(root, inst(actor, tough_11))).
spontaneous(st(root, inst(actor, businessperson_12))).
spontaneous(st(root, inst(item, treasure_13))).
st(root, inst(item, item_10)).
st(root, inst(actor, tough_11)).
st(root, inst(actor, businessperson_12)).
st(root, inst(item, treasure_13)).
at(root, action(option(1), attack)).
at(root, consequence_of(option(1), o(aggressor_state, killed), state(killed, inst(actor, you)))).
at(root, arg(option(1), aggressor, inst(actor, you))).
at(root, consequence_of(option(1), o(aggressor_state, injured), state(injured, inst(actor, you)))).
at(root, consequence(option(1), state(injured, inst(actor, you)))).
at(root, outcome(option(1), o(aggressor_state, injured))).
spontaneous(st(root, relation(has_item, inst(actor, businessperson_12), inst(item, item_10)))).
spontaneous(st(root, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
spontaneous(st(root, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13)))).
st(root, relation(has_item, inst(actor, businessperson_12), inst(item, item_10))).
st(root, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))).
st(root, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
unresolved_potential(root, option(1), potential(problem, state(injured, inst(actor, you)))).
at(root, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
spontaneous(st(root, property(type, inst(item, item_10), perfume))).
spontaneous(st(root, property(type, inst(actor, tough_11), bandits))).
spontaneous(st(root, property(type, inst(actor, businessperson_12), merchant))).
spontaneous(st(root, property(type, inst(item, treasure_13), perfume))).
spontaneous(st(root, property(determined, inst(item, item_10), true))).
spontaneous(st(root, property(determined, inst(actor, tough_11), true))).
spontaneous(st(root, property(determined, inst(actor, businessperson_12), true))).
spontaneous(st(root, property(determined, inst(item, treasure_13), true))).
spontaneous(st(root, property(person, inst(item, item_10), third))).
spontaneous(st(root, property(person, inst(actor, tough_11), third))).
spontaneous(st(root, property(person, inst(actor, businessperson_12), third))).
spontaneous(st(root, property(person, inst(item, treasure_13), third))).
st(root, property(type, inst(item, item_10), perfume)).
st(root, property(type, inst(actor, tough_11), bandits)).
st(root, property(type, inst(actor, businessperson_12), merchant)).
st(root, property(type, inst(item, treasure_13), perfume)).
st(root, property(determined, inst(item, item_10), true)).
st(root, property(determined, inst(actor, tough_11), true)).
st(root, property(determined, inst(actor, businessperson_12), true)).
st(root, property(determined, inst(item, treasure_13), true)).
st(root, property(person, inst(item, item_10), third)).
st(root, property(person, inst(actor, tough_11), third)).
st(root, property(person, inst(actor, businessperson_12), third)).
st(root, property(person, inst(item, treasure_13), third)).
at(root, consequence_of(option(3), o(onwards, onwards), _not, relation(has_item, inst(actor, businessperson_12), inst(item, item_10)))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13)))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(type, inst(item, item_10), perfume))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(determined, inst(item, item_10), true))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(person, inst(item, item_10), third))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(type, inst(actor, tough_11), bandits))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(determined, inst(actor, tough_11), true))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(person, inst(actor, tough_11), third))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(type, inst(actor, businessperson_12), merchant))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(determined, inst(actor, businessperson_12), true))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(person, inst(actor, businessperson_12), third))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(type, inst(item, treasure_13), perfume))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(determined, inst(item, treasure_13), true))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(person, inst(item, treasure_13), third))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, inst(item, item_10))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, inst(actor, tough_11))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, inst(actor, businessperson_12))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, inst(item, treasure_13))).
at(root, outcome(option(3), o(onwards, onwards))).
at(root, consequence(option(3), _not, relation(has_item, inst(actor, businessperson_12), inst(item, item_10)))).
at(root, consequence(option(2), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(3), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(3), _not, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13)))).
at(root, consequence(option(3), _not, property(type, inst(item, item_10), perfume))).
at(root, consequence(option(3), _not, property(determined, inst(item, item_10), true))).
at(root, consequence(option(3), _not, property(person, inst(item, item_10), third))).
at(root, consequence(option(3), _not, property(type, inst(actor, tough_11), bandits))).
at(root, consequence(option(3), _not, property(determined, inst(actor, tough_11), true))).
at(root, consequence(option(3), _not, property(person, inst(actor, tough_11), third))).
at(root, consequence(option(3), _not, property(type, inst(actor, businessperson_12), merchant))).
at(root, consequence(option(3), _not, property(determined, inst(actor, businessperson_12), true))).
at(root, consequence(option(3), _not, property(person, inst(actor, businessperson_12), third))).
at(root, consequence(option(3), _not, property(type, inst(item, treasure_13), perfume))).
at(root, consequence(option(3), _not, property(determined, inst(item, treasure_13), true))).
at(root, consequence(option(3), _not, property(person, inst(item, treasure_13), third))).
at(root, consequence(option(3), _not, inst(item, item_10))).
at(root, consequence(option(3), _not, inst(actor, tough_11))).
at(root, consequence(option(3), _not, inst(actor, businessperson_12))).
at(root, consequence(option(3), _not, inst(item, treasure_13))).
at(root, consequence(option(3), nullifies, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence_of(option(3), o(onwards, onwards), nullifies, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence_of(option(1), o(get_loot, loot), relation(has_item, inst(actor, you), inst(item, plants_book_26)))).
spontaneous(st(root, property(gender, inst(item, item_10), neuter))).
spontaneous(st(root, property(gender, inst(actor, tough_11), neuter))).
spontaneous(st(root, property(gender, inst(actor, businessperson_12), masculine))).
spontaneous(st(root, property(gender, inst(item, treasure_13), neuter))).
spontaneous(st(root, property(number, inst(item, item_10), plural))).
spontaneous(st(root, property(number, inst(actor, tough_11), plural))).
spontaneous(st(root, property(number, inst(actor, businessperson_12), singular))).
spontaneous(st(root, property(number, inst(item, treasure_13), plural))).
spontaneous(st(root, property(name, inst(item, item_10), "perfume"))).
spontaneous(st(root, property(name, inst(actor, tough_11), "bandits"))).
spontaneous(st(root, property(name, inst(actor, businessperson_12), "merchant"))).
spontaneous(st(root, property(name, inst(item, treasure_13), "perfume"))).
st(root, property(gender, inst(item, item_10), neuter)).
st(root, property(gender, inst(actor, tough_11), neuter)).
st(root, property(gender, inst(actor, businessperson_12), masculine)).
st(root, property(gender, inst(item, treasure_13), neuter)).
st(root, property(number, inst(item, item_10), plural)).
st(root, property(number, inst(actor, tough_11), plural)).
st(root, property(number, inst(actor, businessperson_12), singular)).
st(root, property(number, inst(item, treasure_13), plural)).
st(root, property(name, inst(item, item_10), "perfume")).
st(root, property(name, inst(actor, tough_11), "bandits")).
st(root, property(name, inst(actor, businessperson_12), "merchant")).
st(root, property(name, inst(item, treasure_13), "perfume")).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(gender, inst(item, item_10), neuter))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(gender, inst(actor, tough_11), neuter))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(gender, inst(actor, businessperson_12), masculine))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(gender, inst(item, treasure_13), neuter))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(number, inst(item, item_10), plural))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(number, inst(actor, tough_11), plural))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(number, inst(actor, businessperson_12), singular))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(number, inst(item, treasure_13), plural))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(name, inst(item, item_10), "perfume"))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(name, inst(actor, tough_11), "bandits"))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(name, inst(actor, businessperson_12), "merchant"))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(name, inst(item, treasure_13), "perfume"))).
at(root, consequence(option(3), _not, property(gender, inst(item, item_10), neuter))).
at(root, consequence(option(3), _not, property(gender, inst(actor, tough_11), neuter))).
at(root, consequence(option(3), _not, property(gender, inst(actor, businessperson_12), masculine))).
at(root, consequence(option(3), _not, property(gender, inst(item, treasure_13), neuter))).
at(root, consequence(option(3), _not, property(number, inst(item, item_10), plural))).
at(root, consequence(option(3), _not, property(number, inst(actor, tough_11), plural))).
at(root, consequence(option(3), _not, property(number, inst(actor, businessperson_12), singular))).
at(root, consequence(option(3), _not, property(number, inst(item, treasure_13), plural))).
at(root, consequence(option(3), _not, property(name, inst(item, item_10), "perfume"))).
at(root, consequence(option(3), _not, property(name, inst(actor, tough_11), "bandits"))).
at(root, consequence(option(3), _not, property(name, inst(actor, businessperson_12), "merchant"))).
at(root, consequence(option(3), _not, property(name, inst(item, treasure_13), "perfume"))).
spontaneous(st(root, property(has_skill, inst(actor, tough_11), fighting))).
spontaneous(st(root, property(has_skill, inst(actor, businessperson_12), literacy))).
st(root, property(has_skill, inst(actor, tough_11), fighting)).
st(root, property(has_skill, inst(actor, businessperson_12), literacy)).
at(root, consequence_of(option(1), o(target_state, killed), state(killed, inst(actor, tough_11)))).
at(root, arg(option(1), target, inst(actor, tough_11))).
at(root, consequence_of(option(1), o(target_state, injured), state(injured, inst(actor, tough_11)))).
at(root, consequence_of(option(1), o(target_state, killed), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence_of(option(1), o(target_state, injured), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, consequence_of(option(1), o(success, victory), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, arg(option(2), victim, inst(actor, businessperson_12))).
at(root, arg(option(2), listening, inst(actor, tough_11))).
at(root, action(option(2), talk_down)).
at(root, consequence_of(option(2), o(attitude, convinced), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, arg(option(2), asking, inst(actor, you))).
at(root, consequence_of(option(2), o(enraged, enraged), relation(threatening, inst(actor, tough_11), inst(actor, you)))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(has_skill, inst(actor, tough_11), fighting))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(has_skill, inst(actor, businessperson_12), literacy))).
at(root, outcome(option(1), o(success, defeat))).
at(root, consequence(option(2), resolves, potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, outcome(option(2), o(attitude, convinced))).
at(root, consequence_of(option(1), o(success, victory), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence_of(option(1), o(target_state, killed), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence_of(option(1), o(target_state, injured), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence_of(option(2), o(attitude, convinced), _not, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
at(root, consequence(option(3), _not, property(has_skill, inst(actor, tough_11), fighting))).
at(root, consequence(option(3), _not, property(has_skill, inst(actor, businessperson_12), literacy))).
unresolved_potential(root, option(1), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12)))).
spontaneous(st(root, property(type, inst(item, speech_book_83), speech_book))).
spontaneous(st(root, relation(has_item, inst(actor, businessperson_12), inst(item, speech_book_83)))).
spontaneous(st(root, inst(item, speech_book_83))).
st(root, property(type, inst(item, speech_book_83), speech_book)).
st(root, relation(has_item, inst(actor, businessperson_12), inst(item, speech_book_83))).
st(root, inst(item, speech_book_83)).
at(root, consequence_of(option(3), o(onwards, onwards), _not, relation(has_item, inst(actor, businessperson_12), inst(item, speech_book_83)))).
at(root, consequence(option(3), _not, relation(has_item, inst(actor, businessperson_12), inst(item, speech_book_83)))).
spontaneous(st(root, property(gender, inst(item, speech_book_83), neuter))).
spontaneous(st(root, property(number, inst(item, speech_book_83), singular))).
spontaneous(st(root, property(name, inst(item, speech_book_83), "book of speeches"))).
spontaneous(st(root, property(determined, inst(item, speech_book_83), true))).
spontaneous(st(root, property(person, inst(item, speech_book_83), third))).
st(root, property(gender, inst(item, speech_book_83), neuter)).
st(root, property(number, inst(item, speech_book_83), singular)).
st(root, property(name, inst(item, speech_book_83), "book of speeches")).
st(root, property(determined, inst(item, speech_book_83), true)).
st(root, property(person, inst(item, speech_book_83), third)).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(type, inst(item, speech_book_83), speech_book))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, inst(item, speech_book_83))).
at(root, consequence(option(3), _not, property(type, inst(item, speech_book_83), speech_book))).
at(root, consequence(option(3), _not, inst(item, speech_book_83))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(gender, inst(item, speech_book_83), neuter))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(number, inst(item, speech_book_83), singular))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(name, inst(item, speech_book_83), "book of speeches"))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(determined, inst(item, speech_book_83), true))).
at(root, consequence_of(option(3), o(onwards, onwards), _not, property(person, inst(item, speech_book_83), third))).
at(root, consequence(option(3), _not, property(gender, inst(item, speech_book_83), neuter))).
at(root, consequence(option(3), _not, property(number, inst(item, speech_book_83), singular))).
at(root, consequence(option(3), _not, property(name, inst(item, speech_book_83), "book of speeches"))).
at(root, consequence(option(3), _not, property(determined, inst(item, speech_book_83), true))).
at(root, consequence(option(3), _not, property(person, inst(item, speech_book_83), third))).
at(root, category(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), urgent)).
at(root, category(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), immediate)).
st(root, property(has_skill, inst(actor, you), tinkering)).
st(root, property(has_skill, inst(actor, you), wilderness_lore)).
at(root, outcome(option(1), o(target_state, unharmed))).
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
setting(root, city).
at(root, skill_link(acrobatics, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(acrobatics, contest, no_tool, pursue, between(subject, object), either(o(result, caught_up), o(result, lost)))).
at(root, unlikely_outcome(option(1), o(aggressor_state, unharmed))).
at(root, unlikely_outcome(option(1), o(target_state, injured))).
at(root, unlikely_outcome(option(1), o(success, victory))).
at(root, likely_outcome(option(1), o(aggressor_state, injured))).
at(root, likely_outcome(option(1), o(target_state, unharmed))).
at(root, likely_outcome(option(1), o(success, defeat))).
at(root, unlikely_outcome(option(1), o(target_state, killed))).
at(root, unlikely_outcome(option(1), o(get_loot, loot))).
at(root, likely_outcome(option(1), o(get_loot, nothing))).
at(root, outcome(option(1), o(get_loot, nothing))).
at(root, outcome(option(2), o(enraged, not_enraged))).
node_status_reached(root, initialized).
node_status_reached(root, built).
node_status(root, built).
at(root, relevant_to(option(1), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
at(root, relevant_to(option(2), potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))))).
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
at(root, expectation(option(1), enables, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(1), enables, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(2), enables, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(3), enables, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, initiated_by(potential(problem, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))), inst(actor, tough_11))).
at(root, can_trade(inst(actor, you), inst(item, perfume_24))).
at(root, can_trade(inst(actor, you), inst(item, plants_book_26))).
at(root, can_trade(inst(actor, businessperson_12), inst(item, item_10))).
at(root, can_trade(inst(actor, businessperson_12), inst(item, treasure_13))).
at(root, can_trade(inst(actor, businessperson_12), inst(item, speech_book_83))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), tinkering))).
at(root, player_goal(have_tool_for(inst(actor, you), wilderness_lore))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, player_goal(avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(2), threatens, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), threatens, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), threatens, preserve_health(inst(actor, tough_11)))).
at(root, expectation(option(1), fails, preserve_health(inst(actor, you)))).
at(root, likely_outcome(option(3), o(onwards, onwards))).
at(root, expectation(option(1), fails, as_intended(inst(actor, you)))).
at(root, expectation(option(3), achieves, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), tinkering))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, as_intended(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), tinkering))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(3), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, as_intended(inst(actor, you)))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), tinkering))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), wilderness_lore))).
at(root, expectation(option(3), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, stakes(preserve_health(inst(actor, you)), high)).
at(root, stakes(preserve_health(inst(actor, tough_11)), high)).
at(root, stakes(as_intended(inst(actor, you)), low)).
at(root, stakes(have_tool_for(inst(actor, you), wilderness_lore), low)).
at(root, stakes(avoid_threats_to(inst(actor, you)), high)).
at(root, stakes(avoid_threats_to(inst(actor, businessperson_12)), high)).
at(root, stakes(high)).
at(root, option_feel(option(3), safe)).
at(root, choice_structure(obvious)).
at(root, relevant_skill(option(1), inst(actor, tough_11), has, fighting)).
at(root, relevant_skill(option(1), inst(actor, you), missing, fighting)).
at(root, relevant_tool(option(1), inst(actor, tough_11), missing, fighting)).
at(root, is_powerful(inst(actor, tough_11))).
at(root, is_powerless(inst(actor, businessperson_12))).
at(root, outcome_perception(option(2), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, outcome_perception(option(3), advances, avoid_threats_to(inst(actor, businessperson_12)))).
at(root, outcome_perception(option(1), fails, preserve_health(inst(actor, you)))).
at(root, option_feel(option(3), sure_thing)).
at(root, option_feel(option(2), risky)).
at(root, option_feel(option(1), longshot)).
at(root, choice_structure(uncomfortable)).
at(root, outcome_perception(option(2), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(1), fails, as_intended(inst(actor, you)))).
resolves_vignette(root, option(2)).
successor(root, option(1), root_1).
successor(root, option(2), root_2).
successor(root, option(3), root_3).
story_node(root_1).
story_node(root_2).
story_node(root_3).
st(root_1, state(injured, inst(actor, you))).
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
st(root_1, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_2, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_3, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_1, property(number, inst(item, perfume_24), plural)).
st(root_2, property(number, inst(item, perfume_24), plural)).
st(root_3, property(number, inst(item, perfume_24), plural)).
st(root_1, property(number, inst(item, plants_book_26), singular)).
st(root_2, property(number, inst(item, plants_book_26), singular)).
st(root_3, property(number, inst(item, plants_book_26), singular)).
st(root_1, property(name, inst(item, perfume_24), "perfume")).
st(root_2, property(name, inst(item, perfume_24), "perfume")).
st(root_3, property(name, inst(item, perfume_24), "perfume")).
st(root_1, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_2, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_3, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_1, property(type, inst(item, perfume_24), perfume)).
st(root_2, property(type, inst(item, perfume_24), perfume)).
st(root_3, property(type, inst(item, perfume_24), perfume)).
st(root_1, property(type, inst(item, plants_book_26), plants_book)).
st(root_2, property(type, inst(item, plants_book_26), plants_book)).
st(root_3, property(type, inst(item, plants_book_26), plants_book)).
st(root_1, inst(item, perfume_24)).
st(root_2, inst(item, perfume_24)).
st(root_3, inst(item, perfume_24)).
st(root_1, inst(item, plants_book_26)).
st(root_2, inst(item, plants_book_26)).
st(root_3, inst(item, plants_book_26)).
st(root_1, inst(item, item_10)).
st(root_2, inst(item, item_10)).
st(root_1, inst(actor, tough_11)).
st(root_2, inst(actor, tough_11)).
st(root_1, inst(actor, businessperson_12)).
st(root_2, inst(actor, businessperson_12)).
st(root_1, inst(item, treasure_13)).
st(root_2, inst(item, treasure_13)).
st(root_1, relation(has_item, inst(actor, businessperson_12), inst(item, item_10))).
st(root_2, relation(has_item, inst(actor, businessperson_12), inst(item, item_10))).
st(root_1, relation(threatening, inst(actor, tough_11), inst(actor, businessperson_12))).
st(root_1, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
st(root_2, relation(has_item, inst(actor, businessperson_12), inst(item, treasure_13))).
st(root_1, property(type, inst(item, item_10), perfume)).
st(root_2, property(type, inst(item, item_10), perfume)).
st(root_1, property(type, inst(actor, tough_11), bandits)).
st(root_2, property(type, inst(actor, tough_11), bandits)).
st(root_1, property(type, inst(actor, businessperson_12), merchant)).
st(root_2, property(type, inst(actor, businessperson_12), merchant)).
st(root_1, property(type, inst(item, treasure_13), perfume)).
st(root_2, property(type, inst(item, treasure_13), perfume)).
st(root_1, property(determined, inst(item, item_10), true)).
st(root_2, property(determined, inst(item, item_10), true)).
st(root_1, property(determined, inst(actor, tough_11), true)).
st(root_2, property(determined, inst(actor, tough_11), true)).
st(root_1, property(determined, inst(actor, businessperson_12), true)).
st(root_2, property(determined, inst(actor, businessperson_12), true)).
st(root_1, property(determined, inst(item, treasure_13), true)).
st(root_2, property(determined, inst(item, treasure_13), true)).
st(root_1, property(person, inst(item, item_10), third)).
st(root_2, property(person, inst(item, item_10), third)).
st(root_1, property(person, inst(actor, tough_11), third)).
st(root_2, property(person, inst(actor, tough_11), third)).
st(root_1, property(person, inst(actor, businessperson_12), third)).
st(root_2, property(person, inst(actor, businessperson_12), third)).
st(root_1, property(person, inst(item, treasure_13), third)).
st(root_2, property(person, inst(item, treasure_13), third)).
st(root_1, property(gender, inst(item, item_10), neuter)).
st(root_2, property(gender, inst(item, item_10), neuter)).
st(root_1, property(gender, inst(actor, tough_11), neuter)).
st(root_2, property(gender, inst(actor, tough_11), neuter)).
st(root_1, property(gender, inst(actor, businessperson_12), masculine)).
st(root_2, property(gender, inst(actor, businessperson_12), masculine)).
st(root_1, property(gender, inst(item, treasure_13), neuter)).
st(root_2, property(gender, inst(item, treasure_13), neuter)).
st(root_1, property(number, inst(item, item_10), plural)).
st(root_2, property(number, inst(item, item_10), plural)).
st(root_1, property(number, inst(actor, tough_11), plural)).
st(root_2, property(number, inst(actor, tough_11), plural)).
st(root_1, property(number, inst(actor, businessperson_12), singular)).
st(root_2, property(number, inst(actor, businessperson_12), singular)).
st(root_1, property(number, inst(item, treasure_13), plural)).
st(root_2, property(number, inst(item, treasure_13), plural)).
st(root_1, property(name, inst(item, item_10), "perfume")).
st(root_2, property(name, inst(item, item_10), "perfume")).
st(root_1, property(name, inst(actor, tough_11), "bandits")).
st(root_2, property(name, inst(actor, tough_11), "bandits")).
st(root_1, property(name, inst(actor, businessperson_12), "merchant")).
st(root_2, property(name, inst(actor, businessperson_12), "merchant")).
st(root_1, property(name, inst(item, treasure_13), "perfume")).
st(root_2, property(name, inst(item, treasure_13), "perfume")).
st(root_1, property(has_skill, inst(actor, tough_11), fighting)).
st(root_2, property(has_skill, inst(actor, tough_11), fighting)).
st(root_1, property(has_skill, inst(actor, businessperson_12), literacy)).
st(root_2, property(has_skill, inst(actor, businessperson_12), literacy)).
st(root_1, property(type, inst(item, speech_book_83), speech_book)).
st(root_2, property(type, inst(item, speech_book_83), speech_book)).
st(root_1, relation(has_item, inst(actor, businessperson_12), inst(item, speech_book_83))).
st(root_2, relation(has_item, inst(actor, businessperson_12), inst(item, speech_book_83))).
st(root_1, inst(item, speech_book_83)).
st(root_2, inst(item, speech_book_83)).
st(root_1, property(gender, inst(item, speech_book_83), neuter)).
st(root_2, property(gender, inst(item, speech_book_83), neuter)).
st(root_1, property(number, inst(item, speech_book_83), singular)).
st(root_2, property(number, inst(item, speech_book_83), singular)).
st(root_1, property(name, inst(item, speech_book_83), "book of speeches")).
st(root_2, property(name, inst(item, speech_book_83), "book of speeches")).
st(root_1, property(determined, inst(item, speech_book_83), true)).
st(root_2, property(determined, inst(item, speech_book_83), true)).
st(root_1, property(person, inst(item, speech_book_83), third)).
st(root_2, property(person, inst(item, speech_book_83), third)).
st(root_1, property(has_skill, inst(actor, you), tinkering)).
st(root_2, property(has_skill, inst(actor, you), tinkering)).
st(root_3, property(has_skill, inst(actor, you), tinkering)).
st(root_1, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_2, property(has_skill, inst(actor, you), wilderness_lore)).
st(root_3, property(has_skill, inst(actor, you), wilderness_lore)).
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
at(root_1, is_powerful(inst(actor, tough_11))).
at(root_2, is_powerful(inst(actor, tough_11))).
at(root_1, is_powerless(inst(actor, businessperson_12))).
at(root_2, is_powerless(inst(actor, businessperson_12))).
at(root_1, text_mode(example)).
at(root_2, text_mode(example)).
at(root_3, text_mode(example)).
max_unique(83).
