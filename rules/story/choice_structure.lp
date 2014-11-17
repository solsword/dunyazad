% option constraints:
% -------------------

% relevance
at(N, relevant(option(X))) :-
  1 = {
    at(N, deals_with(option(X), Something))
      : at(N, deals_with(option(X), Something));
    at(N, emphasizes(option(X), Something))
      : at(N, emphasizes(option(X), Something))
  },
  at(N, option(X)),
  story_op(N, build_options).

error(m("Irrelevant option", N, option(X))) :-
  at(N, option(X)),
  not at(N, relevant(option(X))),
  story_op(N, build_options).

% redundancy
error(m("Redundant option", N, option(X))) :-
  at(N, action(option(X), Act)),
  at(N, action(option(Y), Act)),
  X != Y,
  story_op(N, build_options).

% first-person perspective

error(m("Non-first-person option", N, option(X))) :-
  at(N, initiator(option(X), Init)),
  Init != inst(actor, the_party),
  story_op(N, build_options).
