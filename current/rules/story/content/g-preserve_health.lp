% preserve health

state_fails(
  preserve_health(inst(Type, Key)),
  state(injured, inst(Type, Key))
) :-
  st(N, inst(Type, Key)).

state_fails(
  preserve_health(inst(Type, Key)),
  state(killed, inst(Type, Key))
) :-
  st(N, inst(Type, Key)).
