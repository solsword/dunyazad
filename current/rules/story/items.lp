item(sword, weapon, "sword", singular).
item(spear, weapon, "spear", singular).
item(javelin, weapon, "javelin", singular).
item(dagger, weapon, "daggers", plural).
item(axe, weapon, "axe", singular).
item(mace, weapon, "mace", singular).
item(glaive, weapon, "glaive", singular).
item(bow, weapon, "bow", singular).
item(rapier, weapon, "rapier", singular).

tool_for(IType, fighting) :- item(IType, weapon, Name, Number).

item(coins, treasure, "coins", plural).
item(gem, treasure, "gem", singular).
item(spices, treasure, "spices", plural).
item(perfume, treasure, "perfume", plural). % TODO: Mass nouns?

item(hammer, tool, "hammer", singular).
item(pliers, tool, "hammer", singular).
item(medicine_chest, tool, "medicine chest", singular).

tool_for(hammer, tinkering).
tool_for(pliers, tinkering).
tool_for(medicine_chest, healing).

item(harp, instrument, "harp", singular).
item(mandolin, instrument, "mandolin", singular).
item(flute, instrument, "flute", singular).
item(oboe, instrument, "oboe", singular).
item(trumpet, instrument, "trumpet", singular).
item(accordian, instrument, "accordian", singular).

tool_for(IType, music) :- item(IType, instrument, Name, Number).

item(ring, charm, "ring", singular).
item(amulet, charm, "amulet", singular).
item(bracelet, charm, "bracelet", singular).
item(necklace, charm, "necklace", singular).
item(anklet, charm, "anklet", singular).
item(earrings, book, "earrings", plural).
item(hair_clip, charm, "hair clip", singular).
item(belt, charm, "belt", singular).
item(vest, charm, "vest", singular).
item(comb, charm, "comb", singular).

item(holy_book, book, "holy book", singular).
item(plants_book, book, "book of herbal lore", singular).
item(animals_book, book, "guide to wild beasts", singular).
item(speech_book, book, "book of speeches", singular).
item(music_book, book, "music book", singular).
item(medicine_book, book, "book of medicine", singular).
item(legends_book, book, "book of legends", singular).

teaches_skill(holy_book, prayer).
tool_for(holy_book, prayer).

teaches_skill(plants_book, wilderness_lore).
teaches_skill(plants_book, healing).
tool_for(plants_book, wilderness_lore).
tool_for(plants_book, healing).

teaches_skill(animals_book, wilderness_lore).
tool_for(animals_book, wilderness_lore).

teaches_skill(speech_book, elocution).

teaches_skill(music_book, music).
tool_for(music_book, music).

teaches_skill(medicine_book, healing).
tool_for(medicine_book, healing).

teaches_skill(legends_book, storytelling).
tool_for(legends_book, storytelling).

nonbook_tool(Tool, Skill) :-
  tool_for(Tool, Skill),
  0 = { item(Tool, book, Name, Number) }.

% Universal tool requirements:

at(N, requires_tool(Action)) :-
  story_node(N),
  requires_tool(Action).

% Tool requirement enforcement:

error(m("Action requires tool", Action)) :-
  at(N, action(Option, Action)),
  at(N, requires_tool(Action)),
  at(N, initiator(Option, Init)),
  0 = {
    st(N, relation(has_item, Init, Tool)) :
      st(N, property(type, Tool, Type)),
      at(N, skill_link(Action, Skill)),
      tool_for(Type, Skill)
  }.


%error(m("Action requires tool", pacify)) :-
%  at(root,action(option(1),pacify)),
%  at(root,requires_tool(pacify)),
%  at(root,initiator(option(1),inst(actor,member_1))),
%  0 = {
%    st(root,relation(has_item,inst(actor,member_1),inst(item,trumpet_45))) :
%      st(root,property(type,inst(item,trumpet_45),instrument)),
%      tool_for(instrument,pacify)
%  }.
