% time.lp
% Events are strictly ordered in time.

beginning_of_time(0).
time(1..100).
end_of_time(101).

% The current story has a beginning and an end:
% If there are no events, they're both at 0:
0 <= { current_beginning(0); current_ending(0) }.
% Otherwise compute them:
not_current_first(T) :-
  story(current, happens(T, Evt)),
  story(current, happens(Before, Other)),
  Before < T.
not_current_last(T) :-
  story(current, happens(T, Evt)),
  story(current, happens(After, Other)),
  After > T.
current_beginning(T) :-
  story(current, happens(T, Evt)),
  not not_current_first(T).
current_ending(T) :-
  story(current, happens(T, Evt)),
  not not_current_last(T).
% Ensure that only one of the two methods above was used:
error(m("No story beginning.")) :-
  1 < { current_beginning(T) : current_beginning(T) }.
error(m("No story ending.")) :-
  1 < { current_ending(T) : current_ending(T) }.

active_timestep(Story, T) :-
  story(Story, happens(T, Evt)).

% All events in every story must happen at some point!
error(m("Unordered event.", ID)) :-
  story(Story, id(evt, ID)),
  0 = { story(Story, happens(T, id(evt, ID))) : time(T) }.

% A notion of "is_before" ordering based on timesteps:
story(Story, ordered(id(evt, Before), is_before, id(evt, After))) :-
  story(Story, id(evt, Before)),
  story(Story, id(evt, After)),
  story(Story, happens(Tafter, id(evt, After))),
  story(Story, happens(Tbefore, id(evt, Before))),
  Tafter > Tbefore.
