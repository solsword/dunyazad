% events.lp
% Management of the choice structure and event info.

#const max_options=7.

evt(e(provocation, 1)).

1 <= {
  evt(e(option, X)) : X = 1..max_options
} <= max_options.

evt(e(outcome, X)) :-
  evt(e(option, X)).

event_next(evt(e(provocation, 1)), evt(e(option, X))) :-
  evt(e(option, X)).

event_next(evt(e(option, X)), evt(e(outcome, X))) :-
  evt(e(option, X)).

first_event(evt(e(provocation, 1))).

% transitive afterness:
event_after(evt(Before), evt(After)) :-
  evt(Before), evt(After),
  event_next(evt(Before), evt(After)).

event_after(evt(Before), evt(After)) :-
  evt(Before), evt(Between), evt(After),
  event_next(evt(Before), evt(Between)),
  event_after(evt(Between), evt(After)).
