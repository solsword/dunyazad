experiment_template(relaxed).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).
node_status(root, uninitialized).
setup_priority(on_sale, 6).
setup_priority(tavern, 5).
setup_priority(threatened_innocents, 4).
setup_priority(market, 2).
setup_priority(monster_attack, 3).
setup_priority(healer, 1).
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
setup(root, market).
at(root, setup_arg(merchant_one, inst(actor, merchant_14))).
at(root, setup_arg(peasant, inst(actor, laborer_17))).
at(root, setup_arg(lowlife, inst(actor, bad_guy_19))).
st(root, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root, property(number, inst(item, dragon_scale_23), singular)).
st(root, property(number, inst(item, ring_25), singular)).
st(root, property(number, inst(item, plants_book_26), singular)).
st(root, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root, property(name, inst(item, ring_25), "ring")).
st(root, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root, property(type, inst(item, ring_25), ring)).
st(root, property(type, inst(item, plants_book_26), plants_book)).
st(root, inst(item, dragon_scale_23)).
st(root, inst(item, ring_25)).
st(root, inst(item, plants_book_26)).
spontaneous(st(root, inst(actor, merchant_14))).
spontaneous(st(root, inst(actor, laborer_17))).
spontaneous(st(root, inst(actor, bad_guy_19))).
st(root, inst(actor, merchant_14)).
st(root, inst(actor, laborer_17)).
st(root, inst(actor, bad_guy_19)).
spontaneous(st(root, state(bored, inst(actor, laborer_17)))).
spontaneous(st(root, state(bored, inst(actor, bad_guy_19)))).
st(root, state(bored, inst(actor, laborer_17))).
st(root, state(bored, inst(actor, bad_guy_19))).
at(root, potential(opportunity, state(bored, inst(actor, laborer_17)))).
at(root, potential(opportunity, state(bored, inst(actor, bad_guy_19)))).
spontaneous(st(root, property(type, inst(actor, merchant_14), merchant))).
spontaneous(st(root, property(type, inst(actor, laborer_17), peasant))).
spontaneous(st(root, property(type, inst(actor, bad_guy_19), thief))).
spontaneous(st(root, property(determined, inst(actor, merchant_14), true))).
spontaneous(st(root, property(determined, inst(actor, laborer_17), true))).
spontaneous(st(root, property(determined, inst(actor, bad_guy_19), true))).
spontaneous(st(root, property(person, inst(actor, merchant_14), third))).
spontaneous(st(root, property(person, inst(actor, laborer_17), third))).
spontaneous(st(root, property(person, inst(actor, bad_guy_19), third))).
st(root, property(type, inst(actor, merchant_14), merchant)).
st(root, property(type, inst(actor, laborer_17), peasant)).
st(root, property(type, inst(actor, bad_guy_19), thief)).
st(root, property(determined, inst(actor, merchant_14), true)).
st(root, property(determined, inst(actor, laborer_17), true)).
st(root, property(determined, inst(actor, bad_guy_19), true)).
st(root, property(person, inst(actor, merchant_14), third)).
st(root, property(person, inst(actor, laborer_17), third)).
st(root, property(person, inst(actor, bad_guy_19), third)).
at(root, arg(option(1), buyer, inst(actor, you))).
at(root, action(option(1), trade)).
at(root, arg(option(1), price, inst(item, ring_25))).
at(root, consequence(option(2), _not, state(bored, inst(actor, laborer_17)))).
at(root, consequence(option(4), _not, state(bored, inst(actor, bad_guy_19)))).
at(root, consequence_of(option(1), o(deal, deal), _not, relation(has_item, inst(actor, you), inst(item, ring_25)))).
spontaneous(st(root, property(gender, inst(actor, merchant_14), masculine))).
spontaneous(st(root, property(gender, inst(actor, laborer_17), feminine))).
spontaneous(st(root, property(gender, inst(actor, bad_guy_19), feminine))).
spontaneous(st(root, property(number, inst(actor, merchant_14), singular))).
spontaneous(st(root, property(number, inst(actor, laborer_17), singular))).
spontaneous(st(root, property(number, inst(actor, bad_guy_19), singular))).
spontaneous(st(root, property(name, inst(actor, merchant_14), "merchant"))).
spontaneous(st(root, property(name, inst(actor, laborer_17), "peasant"))).
spontaneous(st(root, property(name, inst(actor, bad_guy_19), "thief"))).
st(root, property(gender, inst(actor, merchant_14), masculine)).
st(root, property(gender, inst(actor, laborer_17), feminine)).
st(root, property(gender, inst(actor, bad_guy_19), feminine)).
st(root, property(number, inst(actor, merchant_14), singular)).
st(root, property(number, inst(actor, laborer_17), singular)).
st(root, property(number, inst(actor, bad_guy_19), singular)).
st(root, property(name, inst(actor, merchant_14), "merchant")).
st(root, property(name, inst(actor, laborer_17), "peasant")).
st(root, property(name, inst(actor, bad_guy_19), "thief")).
spontaneous(st(root, property(has_skill, inst(actor, merchant_14), literacy))).
spontaneous(st(root, property(has_skill, inst(actor, bad_guy_19), thievery))).
st(root, property(has_skill, inst(actor, merchant_14), literacy)).
st(root, property(has_skill, inst(actor, bad_guy_19), thievery)).
at(root, arg(option(1), seller, inst(actor, merchant_14))).
at(root, consequence_of(option(1), o(deal, deal), relation(has_item, inst(actor, merchant_14), inst(item, ring_25)))).
at(root, arg(option(2), audience, inst(actor, laborer_17))).
at(root, arg(option(4), audience, inst(actor, bad_guy_19))).
at(root, action(option(2), tell_story)).
at(root, consequence_of(option(2), o(quality, boring), nullifies, potential(opportunity, state(bored, inst(actor, laborer_17))))).
at(root, action(option(4), tell_story)).
at(root, consequence_of(option(4), o(quality, boring), nullifies, potential(opportunity, state(bored, inst(actor, bad_guy_19))))).
at(root, consequence_of(option(2), o(quality, entertaining), resolves, potential(opportunity, state(bored, inst(actor, laborer_17))))).
at(root, consequence_of(option(4), o(quality, entertaining), resolves, potential(opportunity, state(bored, inst(actor, bad_guy_19))))).
at(root, consequence(option(2), resolves, potential(opportunity, state(bored, inst(actor, laborer_17))))).
at(root, consequence(option(4), resolves, potential(opportunity, state(bored, inst(actor, bad_guy_19))))).
at(root, outcome(option(2), o(quality, entertaining))).
at(root, outcome(option(4), o(quality, entertaining))).
at(root, consequence_of(option(2), o(quality, boring), _not, state(bored, inst(actor, laborer_17)))).
at(root, consequence_of(option(4), o(quality, boring), _not, state(bored, inst(actor, bad_guy_19)))).
at(root, consequence_of(option(2), o(quality, entertaining), _not, state(bored, inst(actor, laborer_17)))).
at(root, consequence_of(option(4), o(quality, entertaining), _not, state(bored, inst(actor, bad_guy_19)))).
spontaneous(st(root, property(type, inst(item, oboe_91), oboe))).
spontaneous(st(root, relation(has_item, inst(actor, merchant_14), inst(item, oboe_91)))).
spontaneous(st(root, inst(item, oboe_91))).
st(root, property(type, inst(item, oboe_91), oboe)).
st(root, relation(has_item, inst(actor, merchant_14), inst(item, oboe_91))).
st(root, inst(item, oboe_91)).
spontaneous(st(root, property(gender, inst(item, oboe_91), neuter))).
spontaneous(st(root, property(number, inst(item, oboe_91), singular))).
spontaneous(st(root, property(name, inst(item, oboe_91), "oboe"))).
spontaneous(st(root, property(determined, inst(item, oboe_91), true))).
spontaneous(st(root, property(person, inst(item, oboe_91), third))).
st(root, property(gender, inst(item, oboe_91), neuter)).
st(root, property(number, inst(item, oboe_91), singular)).
st(root, property(name, inst(item, oboe_91), "oboe")).
st(root, property(determined, inst(item, oboe_91), true)).
st(root, property(person, inst(item, oboe_91), third)).
at(root, consequence_of(option(1), o(deal, deal), relation(has_item, inst(actor, you), inst(item, oboe_91)))).
at(root, arg(option(1), goods, inst(item, oboe_91))).
at(root, consequence_of(option(1), o(deal, deal), _not, relation(has_item, inst(actor, merchant_14), inst(item, oboe_91)))).
spontaneous(st(root, relation(selling, inst(actor, merchant_14), inst(item, oboe_91)))).
st(root, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))).
at(root, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91)))).
at(root, consequence_of(option(1), o(deal, deal), resolves, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))))).
at(root, consequence_of(option(1), o(deal, deal), _not, relation(selling, inst(actor, merchant_14), inst(item, oboe_91)))).
at(root, arg(option(2), teller, inst(actor, you))).
at(root, arg(option(4), teller, inst(actor, you))).
st(root, property(has_skill, inst(actor, you), music)).
st(root, property(has_skill, inst(actor, you), storytelling)).
at(root, outcome(option(1), o(deal, no_deal))).
at(root, initiator(option(1), inst(actor, you))).
at(root, initiator(option(2), inst(actor, you))).
at(root, initiator(option(4), inst(actor, you))).
at(root, option(2)).
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
at(root, has_tool_for(inst(actor, you), healing)).
at(root, has_tool_for(inst(actor, you), wilderness_lore)).
at(root, has_tool_for(inst(actor, merchant_14), music)).
setting(root, wilderness).
at(root, skill_link(wilderness_lore, contest, no_tool, flee, between(fearful, from), either(o(success, escape), o(success, failure)))).
at(root, skill_link(wilderness_lore, contest, no_tool, pursue, between(subject, object), either(o(result, caught_up), o(result, lost)))).
at(root, likely_outcome(option(2), o(quality, entertaining))).
at(root, likely_outcome(option(4), o(quality, entertaining))).
node_status_reached(root, initialized).
node_status_reached(root, built).
node_status(root, built).
at(root, relevant_to(option(2), potential(opportunity, state(bored, inst(actor, laborer_17))))).
at(root, relevant_to(option(4), potential(opportunity, state(bored, inst(actor, bad_guy_19))))).
at(root, relevant_to(option(1), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))))).
at(root, importance(8, potential(opportunity, state(bored, inst(actor, laborer_17))))).
at(root, importance(8, potential(opportunity, state(bored, inst(actor, bad_guy_19))))).
at(root, importance(8, potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))))).
at(root, some_importance(8)).
at(root, most_important(potential(opportunity, state(bored, inst(actor, laborer_17))))).
at(root, most_important(potential(opportunity, state(bored, inst(actor, bad_guy_19))))).
at(root, most_important(potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))))).
at(root, motivated(option(1))).
at(root, motivated(option(2))).
at(root, motivated(option(4))).
at(root, expectation(option(1), enables, have_tool_for(inst(actor, you), music))).
at(root, initiated_by(potential(opportunity, state(bored, inst(actor, laborer_17))), inst(actor, laborer_17))).
at(root, initiated_by(potential(opportunity, state(bored, inst(actor, bad_guy_19))), inst(actor, bad_guy_19))).
at(root, initiated_by(potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))), inst(actor, merchant_14))).
at(root, can_trade(inst(actor, you), inst(item, dragon_scale_23))).
at(root, can_trade(inst(actor, you), inst(item, ring_25))).
at(root, can_trade(inst(actor, you), inst(item, plants_book_26))).
at(root, can_trade(inst(actor, merchant_14), inst(item, oboe_91))).
at(root, setup_arg(goods_one, inst(item, oboe_91))).
at(root, player_goal(preserve_health(inst(actor, you)))).
at(root, player_goal(avoid_threats_to(inst(actor, you)))).
at(root, player_goal(as_intended(inst(actor, you)))).
at(root, player_goal(have_tool_for(inst(actor, you), music))).
at(root, player_goal(have_tool_for(inst(actor, you), storytelling))).
at(root, player_goal(have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(4), achieves, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, as_intended(inst(actor, you)))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(1), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(2), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(2), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, expectation(option(4), irrelevant, preserve_health(inst(actor, you)))).
at(root, expectation(option(4), irrelevant, avoid_threats_to(inst(actor, you)))).
at(root, expectation(option(4), irrelevant, have_tool_for(inst(actor, you), music))).
at(root, expectation(option(4), irrelevant, have_tool_for(inst(actor, you), storytelling))).
at(root, expectation(option(4), irrelevant, have_tool_for(inst(actor, you), literacy))).
at(root, stakes(as_intended(inst(actor, you)), low)).
at(root, stakes(have_tool_for(inst(actor, you), music), low)).
at(root, stakes(low)).
at(root, option_feel(option(1), safe)).
at(root, option_feel(option(2), safe)).
at(root, option_feel(option(4), safe)).
at(root, choice_structure(relaxed)).
at(root, relevant_skill(option(2), inst(actor, you), has, storytelling)).
at(root, relevant_skill(option(4), inst(actor, you), has, storytelling)).
at(root, is_powerful(inst(actor, bad_guy_19))).
at(root, is_powerless(inst(actor, merchant_14))).
at(root, is_powerless(inst(actor, laborer_17))).
at(root, option_feel(option(2), sure_thing)).
at(root, option_feel(option(4), sure_thing)).
at(root, outcome_perception(option(2), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(4), achieves, as_intended(inst(actor, you)))).
at(root, outcome_perception(option(1), fails, as_intended(inst(actor, you)))).
unresolved_potential(root, option(1), potential(opportunity, state(bored, inst(actor, laborer_17)))).
unresolved_potential(root, option(1), potential(opportunity, state(bored, inst(actor, bad_guy_19)))).
unresolved_potential(root, option(1), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91)))).
unresolved_potential(root, option(2), potential(opportunity, state(bored, inst(actor, bad_guy_19)))).
unresolved_potential(root, option(2), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91)))).
unresolved_potential(root, option(4), potential(opportunity, state(bored, inst(actor, laborer_17)))).
unresolved_potential(root, option(4), potential(opportunity, relation(selling, inst(actor, merchant_14), inst(item, oboe_91)))).
successor(root, option(1), root_1).
successor(root, option(2), root_2).
successor(root, option(4), root_4).
story_node(root_1).
story_node(root_2).
story_node(root_4).
st(root_1, property(type, inst(actor, you), person)).
st(root_2, property(type, inst(actor, you), person)).
st(root_4, property(type, inst(actor, you), person)).
st(root_1, property(has_skill, inst(actor, you), literacy)).
st(root_2, property(has_skill, inst(actor, you), literacy)).
st(root_4, property(has_skill, inst(actor, you), literacy)).
st(root_1, inst(actor, you)).
st(root_2, inst(actor, you)).
st(root_4, inst(actor, you)).
st(root_1, state(party_member, inst(actor, you))).
st(root_2, state(party_member, inst(actor, you))).
st(root_4, state(party_member, inst(actor, you))).
st(root_1, property(name, inst(actor, you), "Dunyazad")).
st(root_2, property(name, inst(actor, you), "Dunyazad")).
st(root_4, property(name, inst(actor, you), "Dunyazad")).
st(root_1, property(person, inst(actor, you), second)).
st(root_2, property(person, inst(actor, you), second)).
st(root_4, property(person, inst(actor, you), second)).
st(root_1, property(gender, inst(actor, you), feminine)).
st(root_2, property(gender, inst(actor, you), feminine)).
st(root_4, property(gender, inst(actor, you), feminine)).
st(root_1, property(determined, inst(actor, you), false)).
st(root_2, property(determined, inst(actor, you), false)).
st(root_4, property(determined, inst(actor, you), false)).
st(root_1, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root_2, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root_4, relation(has_item, inst(actor, you), inst(item, dragon_scale_23))).
st(root_1, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_2, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_4, relation(has_item, inst(actor, you), inst(item, ring_25))).
st(root_1, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_2, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_4, relation(has_item, inst(actor, you), inst(item, plants_book_26))).
st(root_1, property(number, inst(item, dragon_scale_23), singular)).
st(root_2, property(number, inst(item, dragon_scale_23), singular)).
st(root_4, property(number, inst(item, dragon_scale_23), singular)).
st(root_1, property(number, inst(item, ring_25), singular)).
st(root_2, property(number, inst(item, ring_25), singular)).
st(root_4, property(number, inst(item, ring_25), singular)).
st(root_1, property(number, inst(item, plants_book_26), singular)).
st(root_2, property(number, inst(item, plants_book_26), singular)).
st(root_4, property(number, inst(item, plants_book_26), singular)).
st(root_1, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root_2, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root_4, property(name, inst(item, dragon_scale_23), "dragon scale")).
st(root_1, property(name, inst(item, ring_25), "ring")).
st(root_2, property(name, inst(item, ring_25), "ring")).
st(root_4, property(name, inst(item, ring_25), "ring")).
st(root_1, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_2, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_4, property(name, inst(item, plants_book_26), "book of herbal lore")).
st(root_1, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root_2, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root_4, property(type, inst(item, dragon_scale_23), dragon_scale)).
st(root_1, property(type, inst(item, ring_25), ring)).
st(root_2, property(type, inst(item, ring_25), ring)).
st(root_4, property(type, inst(item, ring_25), ring)).
st(root_1, property(type, inst(item, plants_book_26), plants_book)).
st(root_2, property(type, inst(item, plants_book_26), plants_book)).
st(root_4, property(type, inst(item, plants_book_26), plants_book)).
st(root_1, inst(item, dragon_scale_23)).
st(root_2, inst(item, dragon_scale_23)).
st(root_4, inst(item, dragon_scale_23)).
st(root_1, inst(item, ring_25)).
st(root_2, inst(item, ring_25)).
st(root_4, inst(item, ring_25)).
st(root_1, inst(item, plants_book_26)).
st(root_2, inst(item, plants_book_26)).
st(root_4, inst(item, plants_book_26)).
st(root_1, inst(actor, merchant_14)).
st(root_2, inst(actor, merchant_14)).
st(root_4, inst(actor, merchant_14)).
st(root_1, inst(actor, laborer_17)).
st(root_2, inst(actor, laborer_17)).
st(root_4, inst(actor, laborer_17)).
st(root_1, inst(actor, bad_guy_19)).
st(root_2, inst(actor, bad_guy_19)).
st(root_4, inst(actor, bad_guy_19)).
st(root_1, state(bored, inst(actor, laborer_17))).
st(root_4, state(bored, inst(actor, laborer_17))).
st(root_1, state(bored, inst(actor, bad_guy_19))).
st(root_2, state(bored, inst(actor, bad_guy_19))).
st(root_1, property(type, inst(actor, merchant_14), merchant)).
st(root_2, property(type, inst(actor, merchant_14), merchant)).
st(root_4, property(type, inst(actor, merchant_14), merchant)).
st(root_1, property(type, inst(actor, laborer_17), peasant)).
st(root_2, property(type, inst(actor, laborer_17), peasant)).
st(root_4, property(type, inst(actor, laborer_17), peasant)).
st(root_1, property(type, inst(actor, bad_guy_19), thief)).
st(root_2, property(type, inst(actor, bad_guy_19), thief)).
st(root_4, property(type, inst(actor, bad_guy_19), thief)).
st(root_1, property(determined, inst(actor, merchant_14), true)).
st(root_2, property(determined, inst(actor, merchant_14), true)).
st(root_4, property(determined, inst(actor, merchant_14), true)).
st(root_1, property(determined, inst(actor, laborer_17), true)).
st(root_2, property(determined, inst(actor, laborer_17), true)).
st(root_4, property(determined, inst(actor, laborer_17), true)).
st(root_1, property(determined, inst(actor, bad_guy_19), true)).
st(root_2, property(determined, inst(actor, bad_guy_19), true)).
st(root_4, property(determined, inst(actor, bad_guy_19), true)).
st(root_1, property(person, inst(actor, merchant_14), third)).
st(root_2, property(person, inst(actor, merchant_14), third)).
st(root_4, property(person, inst(actor, merchant_14), third)).
st(root_1, property(person, inst(actor, laborer_17), third)).
st(root_2, property(person, inst(actor, laborer_17), third)).
st(root_4, property(person, inst(actor, laborer_17), third)).
st(root_1, property(person, inst(actor, bad_guy_19), third)).
st(root_2, property(person, inst(actor, bad_guy_19), third)).
st(root_4, property(person, inst(actor, bad_guy_19), third)).
st(root_1, property(gender, inst(actor, merchant_14), masculine)).
st(root_2, property(gender, inst(actor, merchant_14), masculine)).
st(root_4, property(gender, inst(actor, merchant_14), masculine)).
st(root_1, property(gender, inst(actor, laborer_17), feminine)).
st(root_2, property(gender, inst(actor, laborer_17), feminine)).
st(root_4, property(gender, inst(actor, laborer_17), feminine)).
st(root_1, property(gender, inst(actor, bad_guy_19), feminine)).
st(root_2, property(gender, inst(actor, bad_guy_19), feminine)).
st(root_4, property(gender, inst(actor, bad_guy_19), feminine)).
st(root_1, property(number, inst(actor, merchant_14), singular)).
st(root_2, property(number, inst(actor, merchant_14), singular)).
st(root_4, property(number, inst(actor, merchant_14), singular)).
st(root_1, property(number, inst(actor, laborer_17), singular)).
st(root_2, property(number, inst(actor, laborer_17), singular)).
st(root_4, property(number, inst(actor, laborer_17), singular)).
st(root_1, property(number, inst(actor, bad_guy_19), singular)).
st(root_2, property(number, inst(actor, bad_guy_19), singular)).
st(root_4, property(number, inst(actor, bad_guy_19), singular)).
st(root_1, property(name, inst(actor, merchant_14), "merchant")).
st(root_2, property(name, inst(actor, merchant_14), "merchant")).
st(root_4, property(name, inst(actor, merchant_14), "merchant")).
st(root_1, property(name, inst(actor, laborer_17), "peasant")).
st(root_2, property(name, inst(actor, laborer_17), "peasant")).
st(root_4, property(name, inst(actor, laborer_17), "peasant")).
st(root_1, property(name, inst(actor, bad_guy_19), "thief")).
st(root_2, property(name, inst(actor, bad_guy_19), "thief")).
st(root_4, property(name, inst(actor, bad_guy_19), "thief")).
st(root_1, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_2, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_4, property(has_skill, inst(actor, merchant_14), literacy)).
st(root_1, property(has_skill, inst(actor, bad_guy_19), thievery)).
st(root_2, property(has_skill, inst(actor, bad_guy_19), thievery)).
st(root_4, property(has_skill, inst(actor, bad_guy_19), thievery)).
st(root_1, property(type, inst(item, oboe_91), oboe)).
st(root_2, property(type, inst(item, oboe_91), oboe)).
st(root_4, property(type, inst(item, oboe_91), oboe)).
st(root_1, relation(has_item, inst(actor, merchant_14), inst(item, oboe_91))).
st(root_2, relation(has_item, inst(actor, merchant_14), inst(item, oboe_91))).
st(root_4, relation(has_item, inst(actor, merchant_14), inst(item, oboe_91))).
st(root_1, inst(item, oboe_91)).
st(root_2, inst(item, oboe_91)).
st(root_4, inst(item, oboe_91)).
st(root_1, property(gender, inst(item, oboe_91), neuter)).
st(root_2, property(gender, inst(item, oboe_91), neuter)).
st(root_4, property(gender, inst(item, oboe_91), neuter)).
st(root_1, property(number, inst(item, oboe_91), singular)).
st(root_2, property(number, inst(item, oboe_91), singular)).
st(root_4, property(number, inst(item, oboe_91), singular)).
st(root_1, property(name, inst(item, oboe_91), "oboe")).
st(root_2, property(name, inst(item, oboe_91), "oboe")).
st(root_4, property(name, inst(item, oboe_91), "oboe")).
st(root_1, property(determined, inst(item, oboe_91), true)).
st(root_2, property(determined, inst(item, oboe_91), true)).
st(root_4, property(determined, inst(item, oboe_91), true)).
st(root_1, property(person, inst(item, oboe_91), third)).
st(root_2, property(person, inst(item, oboe_91), third)).
st(root_4, property(person, inst(item, oboe_91), third)).
st(root_1, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))).
st(root_2, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))).
st(root_4, relation(selling, inst(actor, merchant_14), inst(item, oboe_91))).
st(root_1, property(has_skill, inst(actor, you), music)).
st(root_2, property(has_skill, inst(actor, you), music)).
st(root_4, property(has_skill, inst(actor, you), music)).
st(root_1, property(has_skill, inst(actor, you), storytelling)).
st(root_2, property(has_skill, inst(actor, you), storytelling)).
st(root_4, property(has_skill, inst(actor, you), storytelling)).
node_status_reached(root_1, uninitialized).
node_status_reached(root_2, uninitialized).
node_status_reached(root_4, uninitialized).
node_status_reached(root, branched).
node_status_reached(root, polished).
node_status(root_1, uninitialized).
node_status(root_2, uninitialized).
node_status(root_4, uninitialized).
node_status(root, polished).
path_length(root_1, 1).
path_length(root_2, 1).
path_length(root_4, 1).
at(root_1, is_powerful(inst(actor, bad_guy_19))).
at(root_2, is_powerful(inst(actor, bad_guy_19))).
at(root_4, is_powerful(inst(actor, bad_guy_19))).
at(root_1, is_powerless(inst(actor, merchant_14))).
at(root_2, is_powerless(inst(actor, merchant_14))).
at(root_4, is_powerless(inst(actor, merchant_14))).
at(root_1, is_powerless(inst(actor, laborer_17))).
at(root_2, is_powerless(inst(actor, laborer_17))).
at(root_4, is_powerless(inst(actor, laborer_17))).
at(root_1, text_mode(example)).
at(root_2, text_mode(example)).
at(root_4, text_mode(example)).
max_unique(91).
