% have tool for

% Note this is probably the most opaque goal...

at(N, goal_stakes(have_tool_for(inst(actor, ID), Skill), low)) :-
  st(N, inst(actor, ID)),
  st(N, property(has_skill, inst(actor, ID), Skill)).

state_achieves(
  have_tool_for(inst(actor, Actor), Skill),
  relation(has_item, inst(actor, Actor), inst(item, Item))
) :-
  st(N, inst(actor, Actor)),
  st(N, property(has_skill, inst(actor, Actor), Skill)),
  st(N, inst(item, Item)),
  is_instance(N, inst(item, Item), Category),
  tool_for(Category, Skill).
