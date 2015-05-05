% have tool for

% Note this is probably the most opaque goal...

at(N, stakes(have_tool_for(Actor, Skill), low)) :-
  at(N, expectation(X, AnyExp, have_tool_for(Actor, Skill))),
  AnyExp != irrelevant.

state_achieves(
  have_tool_for(inst(actor, Actor), Skill),
  relation(has_item, inst(actor, Actor), inst(item, Item))
) :-
  st(N, inst(actor, Actor)),
  st(N, property(has_skill, inst(actor, Actor), Skill)),
  st(N, inst(item, Item)),
  is_instance(N, inst(item, Item), Category),
  tool_for(Category, Skill).
