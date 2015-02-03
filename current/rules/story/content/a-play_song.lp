% play_song
action(play_song).
argument(play_song, musician, actor).
argument(play_song, audience, actor).
initiator(play_song, musician).
% TODO: the instrument?

outcome(play_song, harmonious).
outcome(play_song, discordant).

skill_link(play_song, music).

at(N, deals_with(X, state(bored, Audience))) :-
  at(N, action(X, play_song)),
  at(N, outcome(X, harmonious)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).

at(N, emphasizes(X, state(bored, Audience))) :-
  at(N, action(X, play_song)),
  at(N, outcome(X, discordant)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).
% TODO: angering the audience?

% TODO: Other effects?
