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
    precondition(Atype, Condition)
  )
) :-
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  precondition(Atype, Condition),
  not satisfied(Story, Evt, precondition(Atype, Condition)).

satisfied(Story, Evt, precondition(Atype, arg(a(Subj), ArgName, Value))) :-
  precondition(Atype, arg(a(Subj), ArgName, Value)),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Subj, Subject)),
  story(Story, arg(Subject, ArgName, Value)).

satisfied(Story, Evt, precondition(Atype, arg(Subject, ArgName, a(Val)))) :-
  precondition(Atype, arg(Subject, ArgName, Value)),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Val, Value)),
  story(Story, arg(Subject, ArgName, Value)).

satisfied(Story, Evt, precondition(Atype, arg(a(Subj), ArgName, a(Val)))) :-
  precondition(Atype, arg(a(Subj), ArgName, Value)),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Subj, Subject)),
  story(Story, arg(Action, Val, Value)),
  story(Story, arg(Subject, ArgName, Value)).

satisfied(Story, Evt, precondition(Atype, state(a(Subj), Value))) :-
  precondition(Atype, state(a(Subj), Value)),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Subj, Subject)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, State)),
  story(Story, arg(State, type, state)),
  story(Story, arg(State, subject, Subject)),
  story(Story, arg(State, value, Value)).

satisfied(Story, Evt, precondition(Atype, not_state(a(Subj), Value))) :-
  precondition(Atype, not_state(a(Subj), Value)),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Subj, Subject)),
  story(Story, happens(T, Evt)),
  0 = {
    story(Story, at(T, State)) :
      story(Story, arg(State, type, state)),
      story(Story, arg(State, subject, Subject)),
      story(Story, arg(State, value, Value))
  }.

satisfied(Story, Evt, precondition(Atype, property(a(Subj), Dom, Val))) :-
  precondition(Atype, property(a(Subj), Dom, Val)),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Subj, Subject)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, Prop)),
  story(Story, arg(Prop, type, property)),
  story(Story, arg(Prop, subject, Subject)),
  story(Story, arg(Prop, domain, Dom)),
  story(Story, arg(Prop, value, Val)).

satisfied(Story, Evt, precondition(Atype, not_property(a(Subj), Dom, Val))) :-
  precondition(Atype, not_property(a(Subj), Dom, Val)),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Subj, Subject)),
  story(Story, happens(T, Evt)),
  0 = {
    story(Story, at(T, Prop)) :
      story(Story, arg(Prop, type, property)),
      story(Story, arg(Prop, subject, Subject)),
      story(Story, arg(Prop, domain, Dom)),
      story(Story, arg(Prop, value, Val))
  }.

satisfied(Story, Evt, precondition(Atype, rel(a(A1), Val, a(A2)))) :-
  precondition(Atype, rel(a(A1), Val, a(A2))),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, A1, T1)),
  story(Story, arg(Action, A2, T2)),
  story(Story, happens(T, Evt)),
  story(Story, at(T, Rel)),
  story(Story, arg(Rel, type, relation)),
  story(Story, arg(Rel, subject, T1)),
  story(Story, arg(Rel, value, Val)),
  story(Story, arg(Rel, object, T2)).

satisfied(Story, Evt, precondition(Atype, not_rel(a(A1), Val, a(A2)))) :-
  precondition(Atype, not_rel(a(A1), Val, a(A2))),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, A1, T1)),
  story(Story, arg(Action, A2, T2)),
  story(Story, happens(T, Evt)),
  0 = {
    story(Story, at(T, Rel)) :
      story(Story, arg(Rel, type, relation)),
      story(Story, arg(Rel, subject, T1)),
      story(Story, arg(Rel, value, Val)),
      story(Story, arg(Rel, object, T2))
  }.

% Consequences:
%---------------

% Consequences initiate or terminate fluents.

% The consequences of an action are initiated/terminated in the timestep that
% the action happens:
story(Story, arg(Fluent, initiated, T)) :-
  story(Story, arg(Action, initiates, Fluent)),
  story(Story, arg(Evt, action, Action)),
  story(Story, happens(T, Evt)).
story(Story, arg(Fluent, terminated, T)) :-
  story(Story, arg(Action, terminates, Fluent)),
  story(Story, arg(Evt, action, Action)),
  story(Story, happens(T, Evt)).

% consequence rules can initiate or terminate fluents:
story(Story, Fluent) :- consequence_initiates(Story, Action, Trigger, Fluent).
story(Story, arg(Action, initiates, Fluent)) :-
  consequence_initiates(Story, Action, Trigger, Fluent).

story(Story, Fluent) :- consequence_terminates(Story, Action, Trigger, Fluent).
story(Story, arg(Action, terminates, Fluent)) :-
  consequence_terminates(Story, Action, Trigger, Fluent).

1 = {
  error(m("No fluent to initiate.", Story, Action, Something));
  consequence_initiates(
    Story,
    Action,
    consequence(Atype, initiates, Something),
    id(flt, FID)
  ) : story(Story, id(flt, FID))
} :-
  consequence(Atype, initiates, Something),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)).

1 = {
  error(m("No fluent to terminate.", Story, Action, Something));
  consequence_terminates(
    Story,
    Action,
    consequence(Atype, terminates, Something),
    id(flt, FID)
  ) : story(Story, id(flt, FID))
} :-
  consequence(Atype, terminates, Something),
  story(Story, arg(Evt, action, Action)),
  story(Story, arg(Action, type, Atype)).

3 = {
  story(Story, arg(Fluent, type, state));
  story(Story, arg(Fluent, subject, Subject));
  story(Story, arg(Fluent, value, Value));
  error(
    m(
      "Malformed consequence state fluent.",
      consequence_initiates(
        Story,
        Action,
        consequence(Atype, initiates, state(a(Arg), Value)),
        Fluent
      ),
      1..3
    )
  )
} :-
  consequence_initiates(
    Story,
    Action,
    consequence(Atype, initiates, state(a(Arg), Value)),
    Fluent
  ),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Arg, Subject)).

3 = {
  story(Story, arg(Fluent, type, state));
  story(Story, arg(Fluent, subject, Subject));
  story(Story, arg(Fluent, value, Value));
  error(
    m(
      "Terminated wrong state fluent.",
      consequence_terminates(
        Story,
        Action,
        consequence(Atype, terminates, state(a(Arg), Value)),
        Fluent
      ),
      1..3
    )
  )
} :-
  consequence_terminates(
    Story,
    Action,
    consequence(Atype, terminates, state(a(Arg), Value)),
    Fluent
  ),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Arg, Subject)).

4 = {
  story(Story, arg(Fluent, type, property));
  story(Story, arg(Fluent, subject, Subject));
  story(Story, arg(Fluent, domain, Domain));
  story(Story, arg(Fluent, value, Value));
  error(
    m(
      "Malformed consequence property fluent.",
      consequence_initiates(
        Story,
        Action,
        consequence(Atype, initiates, property(a(Arg), Domain, Value)),
        Fluent
      ),
      1..4
    )
  )
} :-
  consequence_initiates(
    Story,
    Action,
    consequence(Atype, initiates, property(a(Arg), Domain, Value)),
    Fluent
  ),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Arg, Subject)).

4 = {
  story(Story, arg(Fluent, type, property));
  story(Story, arg(Fluent, subject, Subject));
  story(Story, arg(Fluent, domain, Domain));
  story(Story, arg(Fluent, value, Value));
  error(
    m(
      "Terminated wrong property.",
      consequence_terminates(
        Story,
        Action,
        consequence(Atype, terminates, property(a(Arg), Domain, Value)),
        Fluent
      ),
      1..4
    )
  )
} :-
  consequence_terminates(
    Story,
    Action,
    consequence(Atype, terminates, property(a(Arg), Domain, Value)),
    Fluent
  ),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, Arg, Subject)).

4 = {
  story(Story, arg(Fluent, type, relation));
  story(Story, arg(Fluent, subject, Subject));
  story(Story, arg(Fluent, value, Value));
  story(Story, arg(Fluent, object, Object));
  error(
    m(
      "Malformed consequence relation fluent.",
      consequence_initiates(
        Story,
        Action,
        consequence(Atype, initiates, rel(a(SArg), Value, a(OArg))),
        Fluent
      ),
      1..4
    )
  )
} :-
  consequence_initiates(
    Story,
    Action,
    consequence(Atype, initiates, rel(a(SArg), Value, a(OArg))),
    Fluent
  ),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, SArg, Subject)),
  story(Story, arg(Action, OArg, Object)).

4 = {
  story(Story, arg(Fluent, type, relation));
  story(Story, arg(Fluent, subject, Subject));
  story(Story, arg(Fluent, value, Value));
  story(Story, arg(Fluent, object, Object));
  error(
    m(
      "Terminated wrong relation.",
      consequence_terminates(
        Story,
        Action,
        consequence(Atype, terminates, rel(a(SArg), Value, a(OArg))),
        Fluent
      ),
      1..4
    )
  )
} :-
  consequence_terminates(
    Story,
    Action,
    consequence(Atype, terminates, rel(a(SArg), Value, a(OArg))),
    Fluent
  ),
  story(Story, arg(Action, type, Atype)),
  story(Story, arg(Action, SArg, Subject)),
  story(Story, arg(Action, OArg, Object)).

% TODO: Should consequences automatically terminate inconsistent fluents?
% (implement this at the site of incompatability)
