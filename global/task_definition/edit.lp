% edit.lp
% Rules about how to construct a modified story state given the existing story
% state and some rules about what the modification is. Allows for writing
% simple/generic modifications and solving for modified stories consistent with
% those (perhaps partially specified) changes.

% In the absence of explicit modifications, story elements are preserved:
story(proposed, X) :- story(current, X), not drop(X).

% Source-tracked proposals:
story(proposed, X) :- proposed_by(Proposer, X).

% Source-tracked proposals are exclusive:
:- proposed_by(Proposer, X), proposed_by(Other, X), Proposer != Other.

% Available IDs may be assigned to newly created story objects:
available_id(Type, X) :-
  id(X),
  type(Type),
  not story(current, id(Type, X)).

% Events are a special case:
available_id(evt, X) :-
  evt_id(X),
  not story(current, id(evt, X)).

% We limit the number of event additions in order to avoid having to compute
% too many possible event orderings:
max_appends(5).
max_insertions(5).

%%%%%%%%%%%%%%%
% Edit rules: %
%%%%%%%%%%%%%%%

% Low-level editing:
%-------------------

% drop:
drop(Fact) :-
  edit(drop, Fact).

% add:
story(proposed, Fact) :-
  edit(add, Fact).

% replace:
story(proposed, Replace) :-
  story(current, Fact),
  edit(replace, Fact, Replace).

% Reference creation:
%--------------------

% new:
% Proposes an available id...
1 = {
  error(m("Out of avaialable IDs for type.", edit(new, Type, Ref)));
  proposed_by(
    edit(new, Type, Ref),
    id(Type, ID)
  ) : available_id(Type, ID)
} :-
  edit(new, Type, Ref).
% ...and binds the proposed id.
bound(Ref, id(Type, ID)) :-
  proposed_by(edit(new, Type, Ref), id(Type, ID)),
  edit(new, Type, Ref).

% some:
% May propose an available id but doesn't have to...
0 <= {
  proposed_by(edit(some, Type, Ref), id(Type, ID)) : available_id(Type, ID)
} <= 1 :-
  edit(some, Type, Ref).
% ...but doesn't needlessly create events...
error(m("Some created an unused id.", edit(some, Type, Ref))) :-
  proposed_by(edit(some, Type, Ref), id(Type, ID)),
  not bound(Ref, id(Type, ID)),
  edit(some, Type, Ref).
% ...and will bind any proposed id, including one forwarded from the story or
% proposed by a different some or new.
1 = {
  error(m("Nothing for some to bind to!", edit(some, Type, Ref)));
  bound(Ref, id(Type, ID))
} :-
  edit(some, Type, Ref),
  story(proposed, id(Type, ID)).

% Event manipulation:
%--------------------

% append_event:
% Proposes a new event...
1 = {
  error(m("Out of avaialable event IDs.", edit(append_event, Ref)));
  proposed_by(edit(append_event, Ref), id(evt, ID)) : available_id(evt, ID)
} :-
  edit(append_event, Ref).
% ...binds the newly-proposed event...
bound(Ref, id(evt, ID)) :-
  proposed_by(edit(append_event, Ref), id(evt, ID)),
  edit(append_event, Ref).
% ...figures out when the event happens...
story(proposed, happens(T, Evt)) :-
  time(T),
  current_ending(E),
  max_appends(MApp),
  max_insertions(MIns),
  T <= E + MApp + MIns,
  bound(Ref, Evt),
  edit(append_event, Ref).
% ...and ensures that the newly-proposed event is after all existing events:
error(m("Appended event before existing event.", edit(append_event, Ref))) :-
  edit(append_event, Ref),
  bound(Ref, id(evt, ID)),
  story(proposed, ordered(id(evt, ID), is_before, id(evt, Other))),
  Other != ID.

% Make usre we're not trying to append too many events:
error(m("Too many events to append.", Count)) :-
  max_appends(Max),
  Count = { edit(append_event, Ref) : edit(append_event, Ref) },
  Count > Max.

% insert_event:
% Creates a marker...
event_insertion_after(Pre) :-
  edit(insert_event_after, Pre, Ref).
% ...proposes a new event...
1 = {
  proposed_by(edit(insert_event_after, Pre, Ref), id(evt, ID)) :
    available_id(evt, ID)
} :-
  edit(insert_event_after, Pre, Ref).
% ...binds the newly-proposed event...
1 = {
  bound(Ref, id(evt, ID)) :
    proposed_by(edit(insert_event_after, Pre, Ref), id(evt, ID))
} :-
  edit(insert_event_after, Pre, Ref).
% ...finds a time for the inserted event to occur...
story(proposed, happens(T, id(evt, ID))) :-
  time(T),
  current_ending(E),
  max_appends(MApp),
  max_insertions(MIns),
  T <= E + MApp + MIns,
  bound(Ref, id(evt, ID)),
  edit(insert_event_after, Pre, Ref).
% ...such that no previously-existing events are between it and the event it
% was supposed to be inserted after...
error(
  m(
    "Event inserted didn't follow original event.",
    edit(insert_event_after, Pre, Ref),
    happens(AnyTime, Offender)
  )
) :-
  story(current, happens(AnyTime, Offender)),
  story(proposed, happens(NotQuiteBefore, Pre)),
  story(proposed, happens(BadTime, Offender)),
  story(proposed, happens(SomeTime, Evt)),
  NotQuiteBefore < BadTime,
  BadTime < SomeTime,
  bound(Ref, Evt),
  edit(insert_event_after, Pre, Ref).
% ...and we allow events to be inserted relative to other new events.
error(
  m(
    "Event inserted didn't follow original event (reference).",
    edit(insert_event_after, Pre, Ref),
    happens(AnyTime, Offender)
  )
) :-
  story(current, happens(AnyTime, Offender)),
  story(proposed, happens(NotQuiteBefore, Pre)),
  story(proposed, happens(BadTime, Offender)),
  story(proposed, happens(SomeTime, Evt)),
  NotQuiteBefore < BadTime,
  BadTime < SomeTime,
  bound(Ref, Evt),
  bound(PreRef, Pre),
  edit(insert_event_after, PreRef, Ref).

error(m("Too many events to insert.", Count)) :-
  max_insertions(Max),
  Count = {
    edit(insert_event_after, Pre, Ref) :
      edit(insert_event_after, Pre, Ref)
  },
  Count > Max.

% Event-shuffling on insertion:
% If some event(s) were inserted before you, don't automatically happen in the
% proposed story. Instead move forward but keep your relative position to other
% story events.
bumped(happens(T, Evt)) :-
  story(current, happens(T, Evt)),
  0 < {
    event_insertion_after(Pre) :
      story(current, happens(Before, Pre)),
      Before <= T
  }.
% Instead of happening when you did in the old story, happen later:
drop(Original) :- bump(Original).
story(proposed, happens(NewT, Evt)) :-
  bump(happens(T, Evt)),
  time(NewT),
  current_ending(E),
  max_appends(MApp),
  max_insertions(MIns),
  NewT <= E + MApp + MIns,
  NewT > T.
% TODO: Allow this on specific occasions?
error(m("Events swapped orders!", Evt1, Evt2)) :-
  story(proposed, order(Evt1, is_before, Evt2)),
  story(current, order(Evt2, is_before, Evt1)).
