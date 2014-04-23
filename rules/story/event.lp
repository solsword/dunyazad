% event.lp
% Rules governing events.

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
