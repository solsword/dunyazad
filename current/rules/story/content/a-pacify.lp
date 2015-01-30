% pacify
action(pacify).

argument(pacify, pacifier, actor).
argument(pacify, aggressive, actor).
initiator(pacify, pacifier).

outcome(pacify, pacified).
outcome(pacify, enraged).

skill_link(pacify, music).
requires_tool(pacify).
%skill_link(pacify, negotiation).

% you can only pacify unintelligent beings: TODO: Change this?
error(m("Pacification of intelligent target.", N, X)) :-
  at(N, arg(X, aggressive, Aggressive)),
  not st(N, state(unintelligent, Aggressive)),
  story_op(N, build_options).

% pacifying is one way to deal with a threat
at(N, deals_with(X, relation(threatening, Aggressive, Someone))) :-
  at(N, action(X, pacify)),
  at(N, outcome(X, pacified));
  at(N, arg(X, aggressive, Aggressive)),
  st(N, relation(threatening, Aggressive, Someone)),
  story_op(N, build_options).

% if things go badly and the target is threatening the actor, it emphasizes the
% problem.
at(N, emphasizes(X, relation(threatening, Aggressive, Pacifier))) :-
  at(N, action(X, pacify)),
  at(N, outcome(X, enraged)),
  at(N, arg(X, aggressive, Aggressive)),
  at(N, arg(X, pacifier, Pacifier)),
  st(N, relation(threatening, Aggressive, Pacifier)),
  story_op(N, build_options).

% otherwise if things go badly it changes the target of aggression.
at(N, nullifies(X, relation(threatening, Aggressive, Someone))) :-
  at(N, action(X, pacify)),
  at(N, outcome(X, enraged)),
  at(N, arg(X, aggressive, Aggressive)),
  at(N, arg(X, pacifier, Pacifier)),
  st(N, relation(threatening, Aggressive, Someone)),
  Pacifier != Someone,
  story_op(N, build_options).

at(N, consequence(X, relation(threatening, Aggressive, Pacifier))) :-
  at(N, action(X, pacify)),
  at(N, outcome(X, enraged)),
  at(N, arg(X, aggressive, Aggressive)),
  at(N, arg(X, pacifier, Pacifier)),
  st(N, relation(threatening, Aggressive, Someone)),
  Pacifier != Someone,
  story_op(N, build_options).

% Surface details
% ---------------

%option_text(
%  N,
%  X,
%  @fmt(
%    "N#{}/they V#pacify/prs/{} N#{}/them.",
%    Pac, Pac, Aggr
%  )
%) :-
%  at(N, action(X, pacify)),
%  at(N, arg(X, pacifier, inst(T1, Pac))),
%  at(N, arg(X, aggressive, inst(T2, Aggr))),
%  story_op(N, add_surface).
%
%action_text(
%  N,
%  X,
%  @fmt(
%    "N#{}/they V#pacify/prs/{} N#{}/them, and N#{}/they V#become/prs/{} calm.",
%    Pac, Pac, Aggr, Aggr, Aggr
%  )
%) :-
%  at(N, action(X, pacify)),
%  at(N, arg(X, pacifier, inst(T1, Pac))),
%  at(N, arg(X, aggressive, inst(T2, Aggr))),
%  at(N, outcome(X, pacified)),
%  story_op(N, add_surface).
%
%action_text(
%  N,
%  X,
%  @fmt(
%    "N#{}/they V#pacify/prs/{} N#{}/them, but N#{}/they V#become/prs/{} enraged.",
%    Pac, Pac, Aggr, Aggr, Aggr
%  )
%) :-
%  at(N, action(X, pacify)),
%  at(N, arg(X, pacifier, inst(T1, Pac))),
%  at(N, arg(X, aggressive, inst(T2, Aggr))),
%  at(N, outcome(X, enraged)),
%  story_op(N, add_surface).
