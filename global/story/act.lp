% act.lp
% Rules that define what different action-definition predicates like
% "precondition" mean.

% Preconditions:
%---------------

error(
  m(
    "Could not fulfil precondition.",
    Story,
    Evt,
    precondition(Action, Condition)
  )
) :-
  story(Story, action(Evt, Action)),
  precondition(Action, Condition),
  not satisfied(Story, Evt, precondition(Action, Condition)).

satisfied(Story, Evt, precondition(Action, state(Arg, State))) :-
  precondition(Action, state(Arg, State)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, state(Target, State))).

satisfied(Story, Evt, precondition(Action, not_state(Arg, State))) :-
  precondition(Action, not_state(Arg, State)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)),
  not story(Story, at(T, state(Target, State))).

satisfied(Story, Evt, precondition(Action, rel(A1, Rel, A2))) :-
  precondition(Action, rel(A1, Rel, A2)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, A1, T1)),
  story(Story, arg(Evt, A2, T2)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, rel(T1, Rel, T2))).

satisfied(Story, Evt, precondition(Action, not_rel(A1, Rel, A2))) :-
  precondition(Action, not_rel(A1, Rel, A2)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, A1, T1)),
  story(Story, arg(Evt, A2, T2)),
  story(Story, happens(T, Evt)),
  not story(Story, at(T, rel(T1, Rel, T2))).

% Consequences:
%---------------

% Consequence satisfaction:
error(
  m(
    "Could not enact consequence.",
    Story,
    consequence(Action, Consequence)
  )
) :-
  story(Story, action(Evt, Action)),
  consequence(Action, Consequence),
  not satisfied(Story, Evt, consequence(Action, Consequence)).

satisfied(Story, Evt, consequence(Action, state(Arg, State))) :-
  consequence(Action, state(Arg, State)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)),
  story(Story, at(T+1, state(Target, State))).

satisfied(Story, Evt, consequence(Action, not_state(Arg, State))) :-
  consequence(Action, not_state(Arg, State)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)),
  not story(Story, at(T+1, state(Target, State))).

satisfied(Story, Evt, consequence(Action, rel(A1, Rel, A2))) :-
  consequence(Action, rel(A1, Rel, A2)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, A1, T1)),
  story(Story, arg(Evt, A2, T2)),
  story(Story, happens(T, Evt)),
  story(Story, at(T+1, rel(T1, Rel, T2))).

satisfied(Story, Evt, consequence(Action, not_rel(A1, Rel, A2))) :-
  consequence(Action, not_rel(A1, Rel, A2)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, A1, T1)),
  story(Story, arg(Evt, A2, T2)),
  story(Story, happens(T, Evt)),
  not story(Story, at(T+1, rel(T1, Rel, T2))).

% Consequences temrinate states/relations:
% TODO: Something more elegant here?
story(Story, terminated(T, state(Target, State))) :-
  consequence(Action, not_state(Arg, State)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)).

story(Story, terminated(T, state(Target, Previous))) :-
  consequence(Action, state(Arg, State)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, state(Target, Previous))),
  incompatible(Previous, State).

story(Story, terminated(T, rel(T1, Rel, T2))) :-
  consequence(Action, not_rel(A1, Rel, A2)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, A1, T1)),
  story(Story, arg(Evt, A2, T2)),
  story(Story, happens(T, Evt)).

story(Story, terminated(T, rel(T1, Rel, Other))) :-
  consequence(Action, rel(A1, Rel, A2)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, A1, T1)),
  story(Story, arg(Evt, A2, T2)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, rel(T1, Rel, Other))),
  Other != T2,
  exclusive(Rel).

story(Story, terminated(T, rel(Target, Rel, Other))) :-
  consequence(Action, state(Arg, State)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, rel(Target, Rel, Other))),
  incompatible(State, Rel).

story(Story, terminated(T, state(Target, State))) :-
  consequence(Action, rel(Arg, State, Other)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, state(Target, State))),
  incompatible(State, Rel).

% TODO: Rel->State dependence

% TODO: More kinds of compatability?

% Consequences initiate new states/relations:
story(Story, initiated(T, state(Target, State))) :-
  consequence(Action, state(Arg, State)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)),
  story(Story, happens(T, Evt)).

story(Story, initiated(T, rel(T1, Rel, T2))) :-
  consequence(Action, rel(A1, Rel, A2)),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, A1, T1)),
  story(Story, arg(Evt, A2, T2)),
  story(Story, happens(T, Evt)).
