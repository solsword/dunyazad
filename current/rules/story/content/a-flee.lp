% flee
action(flee).

argument(flee, fearful, actor).
argument(flee, from, actor).
initiator(flee, fearful).

outcome(flee, escape).
outcome(flee, failure).

at(N, consequence(X, _not, relation(threatening, From, Fearful))) :-
  at(N, action(X, flee)),
  at(N, outcome(X, escape)),
  at(N, arg(X, fearful, Fearful)),
  at(N, arg(X, from, From)),
  st(N, relation(threatening, From, Fearful)),
  story_op(N, build_options).

at(N, emphasizes(X, relation(threatening, From, Fearful))) :-
  at(N, action(X, flee)),
  at(N, outcome(X, failure)),
  at(N, arg(X, fearful, Fearful)),
  at(N, arg(X, from, From)),
  st(N, relation(threatening, From, Fearful)),
  story_op(N, build_options).

0 <= {
  at(N, consequence(X, state(injured, Fearful)))
} <= 1 :-
  at(N, action(X, flee)),
  at(N, arg(X, fearful, Fearful)),
  at(N, outcome(X, failure)),
  story_op(N, build_options).
% TODO: something about the attacker's intent


% Surface details
% ---------------

option_text(
  N,
  X,
  @fmt(
    "N#{}/they V#flee/prs/{} from N#{}/them.",
    Fearful, Fearful, From
  )
) :-
  at(N, action(X, flee)),
  at(N, arg(X, from, inst(T1, From))),
  at(N, arg(X, fearful, inst(T2, Fearful))),
  story_op(N, add_surface).

action_text(
  N,
  X,
  @fmt(
    "N#{}/they V#flee/prs/{} from N#{}/them and escape.",
    Fearful, Fearful, From
  )
) :-
  at(N, action(X, flee)),
  at(N, arg(X, from, inst(T1, From))),
  at(N, arg(X, fearful, inst(T2, Fearful))),
  at(N, outcome(X, escape)),
  story_op(N, add_surface).

action_text(
  N,
  X,
  @fmt(
    "N#{}/they V#flee/prs/{} from N#{}/them but cannot escape.",
    Fearful, Fearful, From
  )
) :-
  at(N, action(X, flee)),
  at(N, arg(X, from, inst(T1, From))),
  at(N, arg(X, fearful, inst(T2, Fearful))),
  at(N, outcome(X, failure)),
  story_op(N, add_surface).
