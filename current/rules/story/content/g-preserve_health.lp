% preserve health

at(N, goal_stakes(preserve_health(inst(actor, ID)), high)) :-
  st(N, inst(actor, ID)).

state_fails(
  preserve_health(inst(actor, Key)),
  state(injured, inst(actor, Key))
) :-
  st(N, inst(actor, Key)).

state_fails(
  preserve_health(inst(actor, Key)),
  state(killed, inst(actor, Key))
) :-
  st(N, inst(actor, Key)).
