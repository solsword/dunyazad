% act.lp
% Rules that define what different action-definition predicates like
% "precondition" mean.

% Basic Rules:
%---------------

% Every action has a type:
1 = {
  error(m("Action without type.", id(act, A)));
  story(Story, arg(id(act, A), type, Atype)) : action(Atype)
} :-
  story(Story, id(act, A)).

% For each declared argument of its type, every action has an argument:
1 = {
  error(m("Action missing argument.", id(act, A), Atype, Arg, Type));
  story(Story, arg(id(act, A), Arg, id(Type, ID))) : possible_id(Type, ID)
} :-
  act_arg(Atype, Arg, Type),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, id(act, A)).

% Preconditions:
%---------------

error(
  m(
    "Could not fulfil precondition.",
    Story,
    id(evt, E),
    precondition(Atype, Condition)
  )
) :-
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  precondition(Atype, Condition),
  not satisfied(Story, id(evt, E), precondition(Atype, Condition)).

satisfied(
  Story,
  id(evt, E),
  precondition(Atype, arg(a(Subj), ArgName, Value))
) :-
  precondition(Atype, arg(a(Subj), ArgName, Value)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Subj, Subject)),
  story(Story, arg(Subject, ArgName, Value)).

satisfied(
  Story,
  id(evt, E),
  precondition(Atype, arg(Subject, ArgName, a(Val)))
) :-
  precondition(Atype, arg(Subject, ArgName, Value)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Val, Value)),
  story(Story, arg(Subject, ArgName, Value)).

satisfied(
  Story,
  id(evt, E),
  precondition(Atype, arg(a(Subj), ArgName, a(Val)))
) :-
  precondition(Atype, arg(a(Subj), ArgName, Value)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Subj, Subject)),
  story(Story, arg(id(act, A), Val, Value)),
  story(Story, arg(Subject, ArgName, Value)).

satisfied(Story, id(evt, E), precondition(Atype, state(a(Subj), Value))) :-
  precondition(Atype, state(a(Subj), Value)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Subj, Subject)),
  story(Story, happens(T, id(evt, E))),
  story(Story, at(T, State)),
  story(Story, arg(State, type, state)),
  story(Story, arg(State, subject, Subject)),
  story(Story, arg(State, value, Value)).

satisfied(Story, id(evt, E), precondition(Atype, not_state(a(Subj), Value))) :-
  precondition(Atype, not_state(a(Subj), Value)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Subj, Subject)),
  story(Story, happens(T, id(evt, E))),
  0 = {
    story(Story, at(T, State)) :
      story(Story, arg(State, type, state)),
      story(Story, arg(State, subject, Subject)),
      story(Story, arg(State, value, Value))
  }.

satisfied(
  Story,
  id(evt, E),
  precondition(Atype, property(a(Subj), Dom, Val))
) :-
  precondition(Atype, property(a(Subj), Dom, Val)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Subj, Subject)),
  story(Story, happens(T, id(evt, E))),
  story(Story, at(T, Prop)),
  story(Story, arg(Prop, type, property)),
  story(Story, arg(Prop, subject, Subject)),
  story(Story, arg(Prop, domain, Dom)),
  story(Story, arg(Prop, value, Val)).

satisfied(
  Story,
  id(evt, E),
  precondition(Atype, not_property(a(Subj), Dom, Val))
) :-
  precondition(Atype, not_property(a(Subj), Dom, Val)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Subj, Subject)),
  story(Story, happens(T, id(evt, E))),
  0 = {
    story(Story, at(T, Prop)) :
      story(Story, arg(Prop, type, property)),
      story(Story, arg(Prop, subject, Subject)),
      story(Story, arg(Prop, domain, Dom)),
      story(Story, arg(Prop, value, Val))
  }.

satisfied(Story, id(evt, E), precondition(Atype, rel(a(A1), Val, a(A2)))) :-
  precondition(Atype, rel(a(A1), Val, a(A2))),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), A1, T1)),
  story(Story, arg(id(act, A), A2, T2)),
  story(Story, happens(T, id(evt, E))),
  story(Story, at(T, Rel)),
  story(Story, arg(Rel, type, relation)),
  story(Story, arg(Rel, subject, T1)),
  story(Story, arg(Rel, value, Val)),
  story(Story, arg(Rel, object, T2)).

satisfied(Story, id(evt, E), precondition(Atype, not_rel(a(A1), Val, a(A2)))) :-
  precondition(Atype, not_rel(a(A1), Val, a(A2))),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), A1, T1)),
  story(Story, arg(id(act, A), A2, T2)),
  story(Story, happens(T, id(evt, E))),
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
  story(Story, arg(id(act, A), initiates, Fluent)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, happens(T, id(evt, E))).
story(Story, arg(Fluent, terminated, T)) :-
  story(Story, arg(id(act, A), terminates, Fluent)),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, happens(T, id(evt, E))).

% consequence rules can initiate or terminate fluents:
story(Story, Fluent) :-
  consequence_initiates(Story, id(act, A), Trigger, Fluent).
story(Story, arg(id(act, A), initiates, Fluent)) :-
  consequence_initiates(Story, id(act, A), Trigger, Fluent).

story(Story, Fluent) :-
  consequence_terminates(Story, id(act, A), Trigger, Fluent).
story(Story, arg(id(act, A), terminates, Fluent)) :-
  consequence_terminates(Story, id(act, A), Trigger, Fluent).

1 = {
  error(m("No fluent to initiate.", Story, id(act, A), Something));
  consequence_initiates(
    Story,
    id(act, A),
    consequence(Atype, initiates, Something),
    id(flt, FID)
  ) : story(Story, id(flt, FID))
} :-
  consequence(Atype, initiates, Something),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)).

1 = {
  error(m("No fluent to terminate.", Story, id(act, A), Something));
  consequence_terminates(
    Story,
    id(act, A),
    consequence(Atype, terminates, Something),
    id(flt, FID)
  ) : story(Story, id(flt, FID))
} :-
  consequence(Atype, terminates, Something),
  story(Story, arg(id(evt, E), action, id(act, A))),
  story(Story, arg(id(act, A), type, Atype)).

3 = {
  story(Story, arg(Fluent, type, state));
  story(Story, arg(Fluent, subject, Subject));
  story(Story, arg(Fluent, value, Value));
  error(
    m(
      "Malformed consequence state fluent.",
      consequence_initiates(
        Story,
        id(act, A),
        consequence(Atype, initiates, state(a(Arg), Value)),
        Fluent
      ),
      1..3
    )
  )
} :-
  consequence_initiates(
    Story,
    id(act, A),
    consequence(Atype, initiates, state(a(Arg), Value)),
    Fluent
  ),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Arg, Subject)).

3 = {
  story(Story, arg(Fluent, type, state));
  story(Story, arg(Fluent, subject, Subject));
  story(Story, arg(Fluent, value, Value));
  error(
    m(
      "Terminated wrong state fluent.",
      consequence_terminates(
        Story,
        id(act, A),
        consequence(Atype, terminates, state(a(Arg), Value)),
        Fluent
      ),
      1..3
    )
  )
} :-
  consequence_terminates(
    Story,
    id(act, A),
    consequence(Atype, terminates, state(a(Arg), Value)),
    Fluent
  ),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Arg, Subject)).

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
        id(act, A),
        consequence(Atype, initiates, property(a(Arg), Domain, Value)),
        Fluent
      ),
      1..4
    )
  )
} :-
  consequence_initiates(
    Story,
    id(act, A),
    consequence(Atype, initiates, property(a(Arg), Domain, Value)),
    Fluent
  ),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Arg, Subject)).

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
        id(act, A),
        consequence(Atype, terminates, property(a(Arg), Domain, Value)),
        Fluent
      ),
      1..4
    )
  )
} :-
  consequence_terminates(
    Story,
    id(act, A),
    consequence(Atype, terminates, property(a(Arg), Domain, Value)),
    Fluent
  ),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), Arg, Subject)).

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
        id(act, A),
        consequence(Atype, initiates, rel(a(SArg), Value, a(OArg))),
        Fluent
      ),
      1..4
    )
  )
} :-
  consequence_initiates(
    Story,
    id(act, A),
    consequence(Atype, initiates, rel(a(SArg), Value, a(OArg))),
    Fluent
  ),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), SArg, Subject)),
  story(Story, arg(id(act, A), OArg, Object)).

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
        id(act, A),
        consequence(Atype, terminates, rel(a(SArg), Value, a(OArg))),
        Fluent
      ),
      1..4
    )
  )
} :-
  consequence_terminates(
    Story,
    id(act, A),
    consequence(Atype, terminates, rel(a(SArg), Value, a(OArg))),
    Fluent
  ),
  story(Story, arg(id(act, A), type, Atype)),
  story(Story, arg(id(act, A), SArg, Subject)),
  story(Story, arg(id(act, A), OArg, Object)).

% TODO: Should consequences automatically terminate inconsistent fluents?
% (implement this at the site of incompatability)
