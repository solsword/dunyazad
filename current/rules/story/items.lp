% items.lp
% Special rules for items. See content/items/*.lp for item
% definitions.

% Item definition unpacking:
subclass(Imm, Spec) :-
  item_def(Spec, Imm, Name, Number).

concrete(Spec) :-
  item_def(Spec, Imm, Name, Number).

default_name_for(Class, Name) :-
  item_def(Class, Superclass, Name, Number).

default_number_for(Class, Number) :-
  item_def(Class, Superclass, Name, Number).

% All items are by default neuter:
default_gender_for(Class, neuter) :-
  subclass(item, Class).

% The concept of a 'nonbook tool' is used for setup
nonbook_tool(Tool, Skill) :-
  tool_for(Tool, Skill),
  0 = { item_def(Tool, book, Name, Number) }.

% Universal tool requirements:

at(N, has_tool_for(Actor, Skill)) :-
  st(N, relation(has_item, Actor, Item)),
  is_instance(N, Item, Category),
  tool_for(Category, Skill),
  story_op(N, initialize_node).

% communal property and the concept of "can trade"

at(N, can_trade(Person, Thing)) :-
  st(N, relation(has_item, Person, Thing)),
  story_op(N, initialize_node).

at(N, can_trade(Person, Thing)) :-
  st(N, state(party_member, Person)),
  st(N, state(party_member, OtherMember)),
  st(N, relation(has_item, OtherMember, Thing)),
  story_op(N, initialize_node).
