% Motivation

% Actions should mostly be motivated:

unmotivated(T) :-
  event(T, Act),
  0 = { motivated(T, Motive) }.

undesirable(10, m("Surplus unmotivated action.", T)) :-
  unmotivated(T),
  unmotivated_action_limit(L),
  L < { unmotivated(X) : time(X) }.

% And not too many actions should happen just because:

undesirable(10, m("Surplus action motivated by destiny.", T)) :-
%error(m("Surplus action motivated by destiny.", T)) :-
  motivated(T, destiny),
  destiny_action_limit(L),
  L < { motivated(X, destiny) : time(X) }.

% And not too many actions should be motivated just by self-interest:

undesirable(10, m("Surplus action motivated by self-interest.", T)) :-
%error(m("Surplus action motivated by self-interest.", T)) :-
  motivated(T, self_interest),
  selfish_action_limit(L),
  L < { motivated(X, self_interest) : time(X) }.

% Nor should such things happen one after the other:
undesirable(20, m("Repeated action motivated by destiny.", T+1)) :-
%error(m("Repeated action motivated by destiny.", T+1)) :-
  motivated(T, destiny),
  motivated(T+1, destiny).

% self-interest:

motivated(T, self_interest) :-
  helpful_to(T, Self),
  responsible_for(T, Self).

% confessing to your love:
motivated(T, love) :-
  event(T, confess_to),
  condition_holds(T, rel(subject, object, feelings, loves)).

% tit-for-tat:

motivated(T, revenge(X)) :-
  X < T,
  harmful_to(X, Victim),
  harmful_to(T, Aggressor),
  responsible_for(X, Aggressor),
  responsible_for(T, Victim),
  Victim != Aggressor.

motivated(T, gratitude(X)) :-
  X < T,
  1 = { helpful_to(X, Beneficiary); solves_problem_for(X, Beneficiary) },
  1 = { helpful_to(T, Benificent); solves_problem_for(T, Benificent) },
  responsible_for(X, Benificent),
  responsible_for(T, Beneficiary),
  Benificent != Beneficiary.

% "fate" actions:

% "fated" actions are allowed every so often, plus near the beginning of the
% story
motivated(T, destiny) :-
  responsible_for(T, fate).

% positive and negative relationships:

motivated(T, enmity) :-
  harmful_to(T, Victim),
  responsible_for(T, Vindictive),
  at(T, rel(Vindictive, Victim, feelings, hates)).

motivated(T, friendship) :-
  responsible_for(T, Friendly),
  character(Friend),
  Friend != Friendly,
  1 <= { helpful_to(T, Friend); solves_problem_for(T, Friend) },
  1 <= {
    at(T, rel(Friendly, Friend, feelings, likes));
    at(T, rel(Friendly, Friend, feelings, loves))
  }.

% random kindness to non-hated acquaintances and strangers:

motivated(T, charity) :-
  responsible_for(T, Charitable),
  solves_problem_for(T, Recipient),
  not causes_problem_for(T, Charitable), % nobody is super-generous
  1 <= {
    at(T, rel(Charitable, Recipient, relationship, strangers));
    at(T, rel(Charitable, Recipient, relationship, acquaintances))
  },
  0 = {
    at(T, rel(Charitable, Recipient, feelings, dislikes));
    at(T, rel(Charitable, Recipient, feelings, hates))
  }.

% Don't help people you dislike without motivation:

error(m("Helpful to hated person.", T, Subj, Obj)) :-
  unmotivated(T),
  helpful_to(T, Obj),
  responsible_for(T, Subj),
  1 <= {
    at(T, rel(Subj, Obj, feelings, dislikes));
    at(T, rel(Subj, Obj, feelings, hates))
  }.
