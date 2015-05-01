% preserve health

at(N, stakes(preserve_health(Actor), high)) :-
  at(N, expectation(X, AnyExp, preserve_health(Actor))),
  AnyExp != irrelevant.

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
