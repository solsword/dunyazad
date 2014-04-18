% state.lp
% Rules about states and relationships.

% Errors that enforce basic rules:
%---------------------------------

error(
  m(
    "Failed to maintain state compatability.",
    Story,
    T,
    state(Subject, S1),
    state(Subject, S2)
  )
) :-
  story(Story, at(T, state(Subject, S1))),
  story(Story, at(T, state(Subject, S2))),
  incompatible(S1, S2).

error(
  m(
    "Multivalued property.",
    Story,
    T,
    property(Subject, Prop, V1),
    property(Subject, Prop, V2)
  )
) :-
  story(Story, at(T, property(Subject, Prop, V1))),
  story(Story, at(T, property(Subject, Prop, V2))).

error(
  m(
    "Failed to maintain state/relationship compatability.",
    Story,
    T,
    state(Subject, State),
    rel(Subject, Rel, Object)
  )
) :-
  story(Story, at(T, state(Subject, State))),
  story(Story, at(T, state(Subject, Rel, Object))),
  incompatible(State, Rel).

error(
  m(
    "Failed to honor state/relationship dependency.",
    Story,
    T,
    rel(Subject, Rel, Object),
    state(Subject, State)
  )
) :-
  story(Story, at(T, state(Subject, Rel, Object))),
  not story(Story, at(T, state(Subject, State))),
  dependent(Rel, State).

error(
  m(
    "Failed to maintain relationship exclusivity.",
    Story,
    T,
    rel(Subject, Rel, O1),
    rel(Subject, Rel, O2)
  )
) :-
  story(Story, at(T, rel(Subject, Rel, O1))),
  story(Story, at(T, rel(Subject, Rel, O2))),
  exclusive(Rel).

% General behavior:
%------------------

% States, properties, and relationships carry over from timestep to timestep
% unless terminated...

story(Story, at(T+1, state(Subject, State))) :-
  active_timestep(T),
  story(Story, at(T, state(Subject, State))),
  not story(Story, terminated(T, state(Subject, State))).

story(Story, at(T+1, property(Subject, Prop, Val))) :-
  active_timestep(T),
  story(Story, at(T, property(Subject, Prop, Val))),
  not story(Story, terminated(T, property(Subject, Prop, Val))).

story(Story, at(T+1, rel(Subject, State, Object))) :-
  active_timestep(T),
  story(Story, at(T, rel(Subject, State, Object))),
  not story(Story, terminated(T, rel(Subject, State, Object))).

% ...and once initiated are present at the next timestep.

story(Story, at(T+1, state(Subject, State))) :-
  active_timestep(T),
  story(Story, initiated(T, state(Subject, State))).

story(Story, at(T+1, property(Subject, Prop, Val))) :-
  active_timestep(T),
  story(Story, initiated(T, property(Subject, Prop, Val))).

story(Story, at(T+1, rel(Subject, State, Object))) :-
  active_timestep(T),
  story(Story, initiated(T, rel(Subject, State, Object))).
