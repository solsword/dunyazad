% preserve original form

at(N, goal_stakes(preserve_original_form(inst(actor, ID)), high)) :-
  st(N, inst(actor, ID)).

state_fails(
  preserve_original_form(inst(actor, Key)),
  property(polymorphed, inst(actor, Key), Any)
) :-
  st(N, inst(actor, Key)),
  any_class(Any).
