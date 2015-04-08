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
  elocution, promotes, no_tool,
  tell_story, teller,
  o(quality, entertaining)
).

skill_link(
  storytelling, promotes, no_tool,
  tell_story, teller,
  o(quality, entertaining)
).

at(
  N,
  consequence_of(
    X,
    o(quality, entertaining),
    takes_advantage_of,
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
    forfeits,
    potential(opportunity, state(bored, Audience))
  )
) :-
  at(N, action(X, tell_story)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).

% TODO: Other effects?
