% option constraints:
% -------------------

% relevance
relevant(option(X)) :-
  deals_with(option(X), Something).

:-
  option(X),
  not relevant(option(X)).

% boredom
:-
  action(option(X), Act),
  action(option(Y), Act),
  X != Y.

% first-person perspective

:-
  initiator(option(X), Init),
  Init != inst(actor, the_party).
