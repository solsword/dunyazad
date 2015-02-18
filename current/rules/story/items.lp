% items.lp
% Special rules for items. See content/items/*.lp for item definitions.

% Item definition unpacking:
subclass(Imm, Spec) :-
  item_def(Spec, Imm, Name, Number).

default_name_for(Class, Name) :-
  item_def(Class, Superclass, Name, Number).

default_number_for(Class, Number) :-
  item_def(Class, Superclass, Name, Number).

% All items are by default neuter:
default_gender_for(Class, neuter) :-
  item_def(Class, Superclass, Name, Number).

% The concept of a 'nonbook tool' is used for setup
nonbook_tool(Tool, Skill) :-
  tool_for(Tool, Skill),
  0 = { item_def(Tool, book, Name, Number) }.

% Universal tool requirements:

at(N, requires_tool(Action)) :-
  story_node(N),
  requires_tool(Action).

% Tool requirement enforcement:

% TODO: remove this?
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
