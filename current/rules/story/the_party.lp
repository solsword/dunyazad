% the_party.lp
% Rules about the player's party.

% TODO: These?
%st(root, property(travel_mode, inst(actor, the_party), walking)).
%st(root, property(environment, inst(actor, the_party), hills)).

% Party Setup:
% ------------

_sr(R) :- story_root(R), story_op(R, initialize_node).

% Core setup:

st(R, inst(actor, the_party)) :- _sr(R).
st(R, property(name, inst(actor, the_party), "party")) :- _sr(R).
st(R, property(person, inst(actor, the_party), first)) :- _sr(R).
st(R, property(number, inst(actor, the_party), plural)) :- _sr(R).
% default gender

st(R, inst(party_member, you)) :- _sr(R).
st(R, property(name, inst(party_member, you), "Dunyazad")) :- _sr(R).
st(R, property(person, inst(party_member, you), first)) :- _sr(R).
st(R, property(gender, inst(party_member, you), feminine)) :- _sr(R).
% default number

% Party member parameters:

party_member_name("Arel").
party_member_name("Jain").
party_member_name("Frodde").
party_member_name("Zair").
party_member_name("Estok").
party_member_name("Ime").

party_member_rel("friend").
party_member_rel("cousin").
party_member_rel("servant").
party_member_rel("teacher").
party_member_rel("student").
party_member_rel("beloved").

% Party member unpacking:

st(R, inst(party_member, Member)) :- party_member(Member, Relation), _sr(R).
1 = {
  st(R, property(name, inst(party_member, Member), Name))
    : party_member_name(Name)
} :- party_member(Member, Relation), _sr(R).
1 = {
  st(R, property(gender, inst(party_member, Member), Gender))
    : symbol(person_gender, Gender)
} :- party_member(Member, Relation), _sr(R).
1 = {
  st(R, property(relationship, inst(party_member, Member), Relation))
} :- party_member(Member, Relation), _sr(R).
st(R, property(proper, inst(party_member, Member), true)) :-
  party_member(Member, Relation),
  _sr(R).
1 <= {
  st(R, property(has_skill, inst(party_member, Member), Skill)) : skill(Skill)
} <= 2 :- party_member(Member, Relation), _sr(R).

% Party member selection:

1 = {
  party_member(member_1, Relation) : party_member_rel(Relation)
} :- _sr(R).
1 >= {
  party_member(member_2, Relation) : party_member_rel(Relation)
} :- _sr(R).
error(m("Polyamory is too complciated to handle.")) :-
  2 <= { party_member(M, "beloved") : party_member(M, R) }.
error(m("Student and master both.")) :-
  party_member(M1, "teacher"),
  party_member(M2, "student").

% Supply unpacking:

st(R, inst(item, Item)) :- supplies(Item, Type, Name, Plural), _sr(R).
st(R, property(type, inst(item, Item), Type)) :-
  supplies(Item, Type, Name, Number),
  _sr(R).
st(R, property(name, inst(item, Item), Name)) :-
  supplies(Item, Type, Name, Number),
  _sr(R).
st(R, property(number, inst(item, Item), Number)) :-
  supplies(Item, Type, Name, Number),
  _sr(R).
st(R, relation(has_item, inst(actor, the_party), inst(item, Item))) :-
  supplies(Item, Type, Name, Number),
  _sr(R).

% Supply details

2 <= {
  supply_item(weapon);
  supply_item(treasure);
  supply_item(tool);
  supply_item(charm);
  supply_item(book)
} <= 3 :- _sr(R).

1 = {
  supplies(sword, weapon, "sword", singular);
  supplies(spear, weapon, "spear", singular);
  supplies(javelin, weapon, "javelin", singular);
  supplies(dagger, weapon, "daggers", plural);
  supplies(axe, weapon, "axe", singular);
  supplies(mace, weapon, "mace", singular);
  supplies(glaive, weapon, "glaive", singular);
  supplies(bow, weapon, "bow", singular);
  supplies(rapier, weapon, "rapier", singular)
} :- supply_item(weapon).

1 = {
  supplies(coins, treasure, "coins", plural);
  supplies(gem, treasure, "gem", singular);
  supplies(spices, treasure, "spices", plural);
  supplies(perfume, treasure, "perfume", plural) % TODO: Mass nouns?
} :- supply_item(treasure).

1 = {
  supplies(caltrops, tool, "caltrops", plural);
  supplies(hammer, tool, "hammer", singular);
  supply_item(instrument)
} :- supply_item(tool).

1 = {
  supplies(harp, instrument, "harp", singular);
  supplies(mandolin, instrument, "mandolin", singular);
  supplies(flute, instrument, "flute", singular);
  supplies(oboe, instrument, "oboe", singular);
  supplies(trumpet, instrument, "trumpet", singular);
  supplies(accordian, instrument, "accordian", singular)
} :- supply_item(instrument).

1 = {
  supplies(ring, charm, "ring", singular);
  supplies(amulet, charm, "amulet", singular);
  supplies(bracelet, charm, "bracelet", singular);
  supplies(necklace, charm, "necklace", singular);
  supplies(anklet, charm, "anklet", singular);
  supplies(earrings, book, "earrings", plural);
  supplies(hair_clip, charm, "hair clip", singular);
  supplies(belt, charm, "belt", singular);
  supplies(vest, charm, "vest", singular);
  supplies(comb, charm, "comb", singular)
} :- supply_item(charm).

1 = {
  supplies(holy_book, book, "holy book", singular);
  supplies(plants_book, book, "book of herbal lore", singular);
  supplies(animals_book, book, "guide to wild beasts", singular);
  supplies(trade_book, book, "book about trading principles", singular);
  supplies(music_book, book, "music book", singular);
  supplies(medicine_book, book, "book of medicine", singular);
  supplies(legends_book, book, "book of legends", singular)
} :- supply_item(book).
