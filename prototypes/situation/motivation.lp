% Motivation

% Actions should mostly be motivated:

unmotivated(T) :-
  event(T, Act),
  0 = { motivated(T, Motive) }.

unmotivated_count(X) :-
  X = { unmotivated(T) : time(T) }.

error(m("Too many unmotivated actions.")) :-
  unmotivated_count(X),
  unmotivated_action_limit(L),
  X > L.

% And not too many actions should happen just because:

destiny_motivated_count(X) :-
  X = { motivated(T, destiny) : time(T) }.

error(m("Too many actions motivated by destiny.")) :-
  destiny_motivated_count(X),
  destiny_action_limit(L),
  X > L.

% self-interest:

motivated(T, self_interest) :-
  event(T, Act),
  helpful_to(T, Self),
  responsible_for(T, Self).

% confessing to your love:
motivated(T, love) :-
  event(T, confess_to),
  condition_holds(T, rel(subject, object, feelings, loves)).

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

% "fate" actions:

% "fated" actions are allowed every so often, plus near the beginning of the
% story
motivated(T, destiny) :-
  event(T, Act),
  responsible_for(T, fate),
  beginning(B).

% positive and negative relationships:

motivated(T, enmity) :-
  event(T, Act),
  harmful_to(T, Victim),
  responsible_for(T, Vindictive),
  at(T, rel(Vindictive, Victim, feelings, hates)).

motivated(T, friendship) :-
  event(T, Act),
  helpful_to(T, Friend),
  responsible_for(T, Friendly),
  0 < {
    at(T, rel(Friendly, Friend, feelings, likes));
    at(T, rel(Friendly, Friend, feelings, loves))
  }.

% random kindness to acquaintances and strangers:

motivated(T, charity) :-
  event(T, Act),
  helpful_to(T, Recipient),
  responsible_for(T, Charitable),
  0 < {
    at(T, rel(Charitable, Recipient, relationship, strangers));
    at(T, rel(Charitable, Recipient, relationship, acquaintances))
  },
  0 = {
    at(T, rel(Charitable, Recipient, feelings, dislikes));
    at(T, rel(Charitable, Recipient, feelings, hates))
  }.

% Don't help people you dislike without motivation:

error(m("Helpful to hated person.", T, Act, Subj, Obj)) :-
  unmotivated(T),
  event(T, Act),
  responsible_for(T, Subj),
  helpful_to(T, Obj),
  0 < {
    at(T, rel(Subj, Obj, feelings, dislikes));
    at(T, rel(Subj, Obj, feelings, hates))
  }.
