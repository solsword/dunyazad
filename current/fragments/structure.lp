% The overall story structure is as follows: in the first part of the story,
% each vignette is either a chance to help or a chance to rest. Once three
% chances to help and at least one chance to rest have happened, there's a
% transition vignette which inevitably inflicts the player with a persistent
% problem, followed by a comeuppance vignette where the player is rewarded or
% punished for their behavior when offered a chance to help.

1 = {
  structural_purpose(N, chance_to_help);
  structural_purpose(N, chance_to_rest);
  structural_purpose(N, transition);
  structural_purpose(N, comeuppance)
} :-
  story_node(N),
  vignette(N, N).

% All nodes in a vignette share the same structural purpose. Note that this can
% result in a non-initial node with multiple structural purposes if a branch
% into a vignette is added after the vignette is completed.
structural_purpose(N, SP) :-
  vignette(N, V),
  structural_purpose(V, SP).

% Some constraints on structural purpose:

error(m("Too many chances to help", N)) :-
  structural_purpose(N, chance_to_help),
  vignette(N, N),
  3 >= {
    before(B, N) : vignette(B, B), structural_purpose(B, chance_to_help)
  }.

vignette_before(Prev, Next) :-
  before(Prev, Next),
  vignette(Prev, Prev),
  vignette(Next, Next).

vignette_successor(Prev, Next) :-
  vignette_before(Prev, Next),
  0 = {
    before(Next, Between) : vignette(Between, Between), before(Prev, Between)
  }.

error(m("Repeated chance to rest", N)) :-
  vignette(N, N),
  structural_purpose(N, chance_to_rest),
  vignette_successor(Prev, N),
  structural_purpose(Prev, chance_to_rest).

error(m("Successive chances to help", N)) :-
  vignette(N, N),
  structural_purpose(N, chance_to_help),
  vignette_successor(Prev, N),
  structural_purpose(Prev, chance_to_help),
  vignette_successor(PPrev, Prev),
  structural_purpose(PPrev, chance_to_help).

error(m("Missed transition timing", N)) :-
  vignette(N, N),
  3 = {
    vignette_before(X, N) : structural_purpose(X, chance_to_help)
  },
  not structural_purpose(N, chance_to_rest),
  not structural_purpose(N, transition).

error(m("Multiple transition", N)) :-
  vignette(N, N),
  structural_purpose(N, transition),
  vignette_before(X, N),
  structural_purpose(X, transition).

error(m("Skipped transition", N)) :-
  vignette(N, N),
  structural_purpose(N, comeuppance),
  0 = {
    vignette_before(X, N) : structural_purpose(X, transition)
  }.

error(m("Multiple comeuppance", N)) :-
  vignette(N, N),
  structural_purpose(N, comeuppance),
  vignette_before(X, N),
  structural_purpose(X, comeuppance).

% After the comeuppance, the story ends:
error(m("Traveled past comeuppance.", N)) :-
  at(N, action(X, travel_onwards)),
  structural_purpose(N, comeuppance).

error(m("Reached destination before comeuppance.", N)) :-
  at(N, action(X, reach_destination)),
  not structural_purpose(N, comeuppance).
