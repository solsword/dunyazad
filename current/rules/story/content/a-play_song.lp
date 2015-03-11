% play_song
action(play_song).

argument(play_song, musician, actor).
argument(play_song, audience, actor).
initiator(play_song, musician).
% TODO: the instrument?

outcome_val(play_song, quality, harmonious).
outcome_val(play_song, quality, plain).
% TODO: add discordant?

skill_link(
  music, required, tool,
  play_song, musician,
  o(quality, harmonious)
).

at(N, consequence_of(X, o(quality, harmonious), _not, state(bored, Audience))):-
  at(N, action(X, play_song)),
  at(N, arg(X, audience, Audience)),
  st(N, state(bored, Audience)),
  story_op(N, build_options).

% TODO: angering the audience?

% TODO: Other effects?
