% tell_story
action(tell_story).

argument(tell_story, teller, actor).
argument(tell_story, audience, actor).
initiator(tell_story, teller).

outcome_val(tell_story, quality, entertaining).
outcome_val(tell_story, quality, boring).

skill_link(tell_story, elocution).
skill_link(tell_story, storytelling).

at(
  N,
  consequence_of(X, o(quality, entertaining), _not, state(bored, Audience))
) :-
  at(N, action(X, tell_story)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).

% TODO: Other effects?
