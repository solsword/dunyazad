% the_party.lp
% Rules about the player's party.

% Party Setup:
% ------------

_sr(R) :- story_root(R), story_op(R, initialize_node).

% Core setup:

starting_skill(tinkering).
starting_skill(wilderness_lore).
starting_skill(music).
starting_skill(elocution).
starting_skill(healing).
starting_skill(thievery).
%starting_skill(prayer).
starting_skill(acrobatics).
starting_skill(storytelling).
starting_skill(fighting).
starting_skill(sorcery).

st(R, inst(actor, you)) :- _sr(R).
st(R, state(party_member, inst(actor, you))) :- _sr(R).
st(R, property(type, inst(actor, you), person)) :- _sr(R).

surface_property(name, inst(actor, you), "Dunyazad").
surface_property(person, inst(actor, you), second).
surface_property(gender, inst(actor, you), feminine).
surface_property(number, inst(actor, you), singular).
surface_property(determined, inst(actor, you), false).
% You start with literacy and three other random skills (from the list of
% "starting" skills):
3 = {
  st(R, property(has_skill, inst(actor, you), Skill)) : starting_skill(Skill);
  error(m("Missing starting skill."))
} :- _sr(R).
st(R, property(has_skill, inst(actor, you), literacy)) :- _sr(R).
%st(R, property(has_skill, inst(actor, you), storytelling)) :- _sr(R).

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

% Party member selection:

1 = {
  party_size(PS) : party_size_value(PS)
} :- _sr(R).

1 = {
  party_member(member_one, Relation) : party_member_rel(Relation);
  error(m("Party member of no relation.", member_one))
} :-
  _sr(R),
  party_size(PS),
  PS >= 2.
1 = {
  party_member(member_two, Relation) : party_member_rel(Relation);
  error(m("Party member of no relation.", member_two))
} :-
  _sr(R),
  party_size(PS),
  PS >= 3.
error(m("Can't handle party sizes larger than 3.")) :-
  party_size(PS),
  PS > 3.
error(m("Multiple party members have the same relation.")) :-
  party_member(M1, Relation),
  party_member(M2, Relation),
  M1 != M2.
error(m("Polyamory is too complciated to handle.")) :-
  2 <= { party_member(M, "beloved") : party_member(M, R) }.
error(m("Student and master both.")) :-
  party_member(M1, "teacher"),
  party_member(M2, "student").

% Party member unpacking:

st(R, inst(actor, Member)) :- party_member(Member, Relation), _sr(R).

st(R, state(party_member, inst(actor, Member))) :-
  party_member(Member, Relation),
  _sr(R).

1 = {
  surface_property(name, inst(actor, Member), Name)
    : party_member_name(Name);
  error(m("Party member without name.", Member))
} :- party_member(Member, Relation).

error(m("Multiple party members have the same name.")) :-
  surface_property(name, inst(actor, M1), Name),
  surface_property(name, inst(actor, M2), Name),
  M1 != M2,
  _sr(R).

1 = {
  surface_property(gender, inst(actor, Member), masculine);
  surface_property(gender, inst(actor, Member), feminine);
  error(m("Party member without gender.", Member))
} :- party_member(Member, Relation), _sr(R).

surface_property(determined, inst(actor, Member), false) :-
  party_member(Member, Relation).

st(R, property(relationship, inst(actor, Member), Relation)) :-
  party_member(Member, Relation), _sr(R).

st(R, property(type, inst(actor, Member), person)) :-
  party_member(Member, Relation),
  _sr(R).

% Each party member starts with 2 skills:
2 = {
  st(R, property(has_skill, inst(actor, Member), Skill))
    : skill(Skill),
      not restricted_skill(Skill);
  error(m("Party member missing starting skill 1.", Member));
  error(m("Party member missing starting skill 2.", Member))
} :- party_member(Member, Relation), _sr(R).

error(m("Party member initial skill overlap.", Skill)) :-
  skill(Skill),
  st(R, property(has_skill, inst(actor, M1), Skill)),
  st(R, property(has_skill, inst(actor, M2), Skill)),
  st(R, state(party_member, inst(actor, M1))),
  st(R, state(party_member, inst(actor, M2))),
  M1 != M2,
  _sr(R).

% Supply unpacking:

get_unique_key(s(supplies(Owner, Item))) :- supplies(Owner, Item), _sr(R).
supply_inst(R, Owner, inst(item, @inst(Item, K)), Item) :-
  unique_key(s(supplies(Owner, Item)), K),
  supplies(Owner, Item),
  _sr(R).
st(R, Inst) :-
  supply_inst(R, Owner, Inst, Type).
st(R, property(type, Inst, Type)) :-
  supply_inst(R, Owner, Inst, Type).
surface_property(name, Inst, Name) :-
  supply_inst(R, Owner, Inst, Type),
  item_def(Type, GType, Name, Number).
surface_property(number, Inst, Number) :-
  supply_inst(R, Owner, Inst, Type),
  item_def(Type, GType, Name, Number).
st(R, relation(has_item, inst(actor, Owner), Inst)) :-
  supply_inst(R, Owner, Inst, Type).

% Supply details

% You start with some basic items:
1 = {
  supplies(you, Treasure) :
    item_def(Treasure, treasure, Name, Number)
} :- _sr(R).
1 >= {
  supplies(you, Charm) :
    item_def(Charm, charm, Name, Number)
} :- _sr(R).

1 = {
  supplies(you, Book) :
    item_def(Book, book, Name, Number);
  error(m("No starting book."))
} :- _sr(R).

% You can start with a tool for one of your skills:
0 <= {
  supplies(you, Tool) :
    nonbook_tool(Tool, Skill),
    st(R, property(has_skill, inst(actor, you), Skill)),
    _sr(R)
} <= 2.

% Your party members start with non-book tools for their skills:
1 = {
  supplies(Member, Tool) :
    nonbook_tool(Tool, Skill),
    item_def(Tool, Type, Name, Number);
  error(m("Member didn't start with tool for skill.", Member, Skill))
} :-
  1 <= { nonbook_tool(Tool, Skill) },
  party_member(Member, Relation),
  st(R, property(has_skill, inst(actor, Member), Skill)),
  _sr(R).

% Your literate party members might also start with some random books:
2 >= {
  supplies(Member, Book) :
    party_member(Member, Relation),
    st(R, property(has_skill, inst(actor, Member), literacy)),
    item_def(Book, book, BName, BNumber)
} :-
  _sr(R).
