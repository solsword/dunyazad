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

error(m("Unintroduced subject.", T, Subj)) :-
  argument(T, subject, Subj),
  not introduced(T, Subj).

% Boredom

error(m("Boring action", T+1, Act)) :-
  event(T, Act),
  event(T+1, Act).

% Motivation

% tit-for-tat:

motivated(T, revenge(X)) :-
  event(T, Act),
  event(X, Prev),
  X < T,
  harmful_to(X, Victim),
  harmful_to(T, Aggressor),
  responsible_for(X, Aggressor),
  responsible_for(T, Victim),
  Victim != Aggressor.

motivated(T, gratitude(X)) :-
  event(T, Act),
  event(X, Prev),
  X < T,
  helpful_to(X, Beneficiary),
  helpful_to(T, Benificent),
  responsible_for(X, Benificent),
  responsible_for(T, Beneficiary),
  Benificent != Beneficiary.

unmotivated(T) :-
  event(T, Act),
  0 = { motivated(T, Motive) }.

unmotivated_count(X) :-
  X = { unmotivated(T) : time(T) }.

error(m("Too many unmotivated actions.")) :-
  unmotivated_count(X),
  unmotivated_action_limit(L),
  X > L.
