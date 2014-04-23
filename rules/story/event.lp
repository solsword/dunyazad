% event.lp
% Rules governing events.

% Every event happens at some point:
1 = {
  error(m("Event never happens.", Story, id(evt, E)));
  story(Story, happens(T, id(evt, E))) : time(T)
} :-
  story(Story, id(evt, E)).

% Every event has exactly one action:
% TODO: allow more?

1 = {
  error(m("Event with no action argument.", Story, id(evt, E)));
  story(Story, arg(id(evt, E), action, id(act, A))) :
    possible_id(act, A)
} :-
  story(Story, id(evt, E)).

% Every event's action is part of the story:

1 = {
  error(m("No action for event.", id(evt, E), id(act, A)));
  story(Story, id(act, A))
} :-
  story(Story, arg(id(evt, E), action, id(act, A))).

story(current, id(evt, 1)).
story(current, id(act, 1)).
story(current, id(flt, 1)).
story(current, id(chr, 1)).
story(current, arg(id(evt, 1), action, id(act, 1))).
story(current, arg(id(act, 1), type, fall_ill)).
