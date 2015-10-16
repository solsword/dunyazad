% pacify
action(pacify).

% arguments

argument(pacify, pacifier, actor).
argument(pacify, aggressive, actor).
initiator(pacify, pacifier).
default_intent(pacify, o(mood, relaxed)).

% outcomes

outcome_val(pacify, mood, relaxed).
outcome_val(pacify, mood, enraged).

outcome_val(pacify, get_injured, injured).
outcome_val(pacify, get_injured, safe).

outcome_excludes(pacify, o(mood, relaxed), o(get_injured, injured)).

% skills

skill_link(
  music, required, tool,
  pacify, pacifier,
  o(mood, relaxed)
).

% TODO: Should we really have such high expectations?
%skill_link(
%  music, promotes, tool,
%  pacify, pacifier,
%  o(mood, relaxed)
%).

skill_link(
  music, required, no_tool,
  pacify, pacifier,
  o(get_injured, safe)
).


% you can only pacify unintelligent beings: TODO: Change this?
error(m("Pacification of intelligent target.", N, X)) :-
  at(N, arg(X, aggressive, Aggressive)),
  not st(N, property(has_skill, Aggressive, unintelligent)),
  story_op(N, build_options).

% pacifying is one way to deal with a threat
at(
  N,
  consequence_of(
    X,
    o(mood, relaxed),
    resolves,
    potential(problem, relation(threatening, Aggressive, Someone))
  )
) :-
  at(N, action(X, pacify)),
  at(N, arg(X, aggressive, Aggressive)),
  st(N, relation(threatening, Aggressive, Someone)),
  story_op(N, build_options).

% if things go badly the subject threatens you
at(
  N,
  consequence_of(
    X,
    o(mood, enraged),
    relation(threatening, Aggressive, Pacifier)
  )
) :-
  at(N, action(X, pacify)),
  at(N, arg(X, aggressive, Aggressive)),
  at(N, arg(X, pacifier, Pacifier)),
  story_op(N, build_options).

% and ignores anyone else
at(
  N,
  consequence_of(
    X,
    o(mood, enraged),
    nullifies,
    potential(problem, relation(threatening, Aggressive, Someone))
  )
) :-
  at(N, action(X, pacify)),
  at(N, arg(X, aggressive, Aggressive)),
  at(N, arg(X, pacifier, Pacifier)),
  st(N, relation(threatening, Aggressive, Someone)),
  Someone != Pacifier,
  story_op(N, build_options).

% You can get injured if things go badly.
at(N, consequence_of(X, o(get_injured, injured), state(injured, Pacifier))) :-
  at(N, action(X, pacify)),
  at(N, arg(X, pacifier, Pacifier)),
  story_op(N, build_options).
