% preserve health

state_fails(
  preserve_health(inst(Type, Key)),
  state(injured, inst(Type, Key))
) :-
  at(N, inst(Type, Key)).

state_fails(
  preserve_health(inst(Type, Key)),
  state(killed, inst(Type, Key))
) :-
  at(N, inst(Type, Key)).
