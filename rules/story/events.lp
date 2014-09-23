% events.lp
% Management of the timeline and event info.

1 = {
  last_event(evt(X));
  event_next(evt(X), evt(Y)) : evt(Y), Y > X
} :-
  evt(X).

first_event(evt(X)) :-
  evt(X),
  0 = {
    event_next(evt(Y), evt(X)) : evt(Y), Y != X
  }.

error(m("Multiple first events")) :-
  first_event(evt(X)), first_event(evt(Y)), X != Y.

error(m("Multiple last events")) :-
  last_event(evt(X)), last_event(evt(Y)), X != Y.

% transitive afterness:
event_after(evt(Before), evt(After)) :-
  evt(Before), evt(After),
  event_next(evt(Before), evt(After)).

event_after(evt(Before), evt(After)) :-
  evt(Before), evt(Between), evt(After),
  event_next(evt(Before), evt(Between)),
  event_after(evt(Between), evt(After)).
