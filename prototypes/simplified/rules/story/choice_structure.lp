% option constraints:
% -------------------

% relevance
at(N, relevant(option(X))) :-
  at(N, deals_with(option(X), Something)),
  active_node(N).

error(m("Irrelevant option", N, option(X))) :-
  at(N, option(X)),
  not at(N, relevant(option(X))),
  active_node(N).

% redundancy
error(m("Redundant option", N, option(X))) :-
  at(N, action(option(X), Act)),
  at(N, action(option(Y), Act)),
  X != Y,
  active_node(N).

% first-person perspective

error(m("Non-first-person option", N, option(X))) :-
  at(N, initiator(option(X), Init)),
  Init != inst(actor, the_party),
  active_node(N).
