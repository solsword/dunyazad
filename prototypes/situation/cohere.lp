% Coherence constraints...

% Character introduction

appears(T, Thing) :-
  argument(T, Arg, Thing).

introduced(B, id(Type, ID)) :-
  id(Type, ID),
  beginning(B).

introduced(T, Thing) :-
  time(T),
  appears(X, Thing),
  X < T.

error(m("Unintroduced subject", T, Subj)) :-
  argument(T, subject, Subj),
  not introduced(T, Subj).

% Boredom

error(m("Boring action (repeated immediately)", T+1, Act)) :-
  event(T, Act),
  event(T+1, Act).

error(m("Boring action (too many occurances)", Act, Count)) :-
  action(Act),
  boredom_limit(L),
  Count = { event(T, Act) : time(T) },
  Count > L.
