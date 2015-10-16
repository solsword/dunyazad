% tell_story
action(tell_story).

% arguments

argument(tell_story, teller, actor).
argument(tell_story, audience, actor).
initiator(tell_story, teller).
default_intent(tell_story, o(quality, entertaining)).

% outcomes

outcome_val(tell_story, quality, entertaining).
outcome_val(tell_story, quality, boring).

% skills

skill_link(
  storytelling, promotes, no_tool,
  tell_story, teller,
  o(quality, entertaining)
).

error(m("Unintelligent storyteller.", N, X)) :-
  at(N, action(X, tell_story)),
  at(N, arg(X, teller, Unintelligent)),
  st(N, property(has_skill, Unintelligent, unintelligent)),
  story_op(N, build_options).

error(m("Unintelligent audience.", N, X)) :-
  at(N, action(X, tell_story)),
  at(N, arg(X, audience, Unintelligent)),
  st(N, property(has_skill, Unintelligent, unintelligent)),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(quality, entertaining),
    resolves,
    potential(opportunity, state(bored, Audience))
  )
) :-
  at(N, action(X, tell_story)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(quality, boring),
    nullifies,
    potential(opportunity, state(bored, Audience))
  )
) :-
  at(N, action(X, tell_story)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).

% TODO: Other effects?
