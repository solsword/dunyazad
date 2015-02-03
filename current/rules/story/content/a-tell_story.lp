% tell_story
action(tell_story).
argument(tell_story, teller, actor).
argument(tell_story, audience, actor).
initiator(tell_story, teller).

outcome(tell_story, entertaining).
outcome(tell_story, boring).

skill_link(tell_story, storytelling).

at(N, deals_with(X, state(bored, Audience))) :-
  at(N, action(X, tell_story)),
  at(N, outcome(X, entertaining)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).

at(N, emphasizes(X, state(bored, Audience))) :-
  at(N, action(X, tell_story)),
  at(N, outcome(X, boring)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).

% TODO: Other effects?
