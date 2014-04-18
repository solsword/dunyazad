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

% Construction primitives:
%-------------------------

% "value" statements create collections of exlusive states.

state(S) :- value(Category, S).

incompatible(S1, S2) :- value(Category, S1), value(Category, S2).

% General behavior:
%------------------

% States and relationships carry over from timestep to timestep unless
% terminated:

story(Story, at(T+1, state(Subject, State))) :-
  story(Story, at(T, state(Subject, State))),
  not story(Story, terminated(T, state(Subject, State))).

story(Story, at(T+1, rel(Subject, State, Object))) :-
  story(Story, at(T, rel(Subject, State, Object))),
  not story(Story, terminated(T, rel(Subject, State, Object))).

% And once initiated are present at the next timestep:

story(Story, at(T+1, state(Subject, State))) :-
  story(Story, initiated(T, state(Subject, State))).

story(Story, at(T+1, rel(Subject, State, Object))) :-
  story(Story, initiated(T, rel(Subject, State, Object))).
