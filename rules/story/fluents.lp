% fluents.lp
% Rules about fluents (states, properties, and relations).

% Fluent Structure:
%------------------

% Fluents have ids, and have args as follows:

% states: type ('state'), subject, value
% properties: type ('property'), subject, domain, value
% relations: type ('relation'), subject, value, object

fltype(state).
fltype(property).
fltype(relation).

flarg(T, type) :- fltype(T).
flarg(T, initiated) :- fltype(T).
flarg(T, terminated) :- fltype(T).
flarg(T, subject) :- fltype(T).
flarg(T, value) :- fltype(T).

flarg(property, domain).
flarg(relation, object).

1 = {
  error(m("Fluent with no type.", Story, id(flt, F)));
  story(Story, arg(id(flt, F), type, Type)) : fltype(Type)
} :-
  story(Story, id(flt, F)).

error(m("Fluent missing argument.", Story, id(flt, F), Arg)) :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), type, Type)),
  flarg(Type, Arg),
  0 = {
    story(Story, arg(id(flt, F), Arg, Val)) :
      story(Story, arg(id(flt, F), Arg, Val))
  }.

error(m("Fluent with unknown argument.", Story, id(flt, F), Arg)) :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), type, Type)),
  story(Story, arg(id(flt, F), Arg, Value)),
  not flarg(Type, Arg).

error(m("Fluent with multi-valued argument.",Story, id(flt, F), Arg, V1, V2)) :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), Arg, V1)),
  story(Story, arg(id(flt, F), Arg, V2)),
  V1 != V2.

error(m("Conflicting states.", Story, T, F1, F2, S, V)) :-
  story(Story, at(T, F1)),
  story(Story, at(T, F2)),
  story(Story, arg(F1, type, state)),
  story(Story, arg(F2, type, state)),
  story(Story, arg(F1, subject, S)),
  story(Story, arg(F2, subject, S)),
  story(Story, arg(F1, value, V)),
  story(Story, arg(F2, value, V)),
  F1 != F2.

error(m("Conflicting properties.", Story, T, F1, F2, S, D)) :-
  story(Story, at(T, F1)),
  story(Story, at(T, F2)),
  story(Story, arg(F1, type, property)),
  story(Story, arg(F2, type, property)),
  story(Story, arg(F1, subject, S)),
  story(Story, arg(F2, subject, S)),
  story(Story, arg(F1, domain, D)),
  story(Story, arg(F2, domain, D)),
  F1 != F2.

error(m("Conflicting relations.", Story, T, F1, F2, S, V, O)) :-
  story(Story, at(T, F1)),
  story(Story, at(T, F2)),
  story(Story, arg(F1, type, relation)),
  story(Story, arg(F2, type, relation)),
  story(Story, arg(F1, subject, S)),
  story(Story, arg(F2, subject, S)),
  story(Story, arg(F1, value, V)),
  story(Story, arg(F2, value, V)),
  story(Story, arg(F1, object, O)),
  story(Story, arg(F2, object, O)),
  F1 != F2.

% Base behvaior:
%---------------

% All fluents are initiated and terminated, and are "at" all timesteps in
% between (starting the timestep after they're initiated and ending the
% timestep on which they're terminated).

story(Story, at(T, id(flt, F))) :-
  time(T),
  story(Story, arg(id(flt, F), initiated, Ti)),
  story(Story, arg(id(flt, F), terminated, Tt)),
  T > Ti, T <= Tt.

1 = {
  error(
    m("Non-hypothetical fluent never initiated.", Story, id(flt, F))
  );
  story(Story, arg(id(flt, F), initiated, T)) : active_timestep(Story, T);
  story(Story, arg(id(flt, F), initiated, BoT)) : beginning_of_time(BoT)
} :-
  story(Story, id(flt, F)),
  not story(Story, hypothetical(id(flt, F))).

% Fluents are initiated by something even if that thing is "setup":
story(Story, arg(setup, initiates, id(flt, F))) :-
  story(Story, arg(id(flt, F), initiated, BoT)),
  beginning_of_time(BoT).
1 = {
  error(m("Auto-initiated fluent.", Story, T, id(flt, F)));
  story(Story, arg(Action, initiates, id(flt, F))) :
    story(Story, arg(Evt, action, Action)),
    story(Story, happens(T, Evt))
} :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), initiated, T)),
  active_timestep(Story, T).

1 = {
  error(
    m("Non-hypothetical fluent never terminated.", Story, id(flt, F))
  );
  story(Story, arg(id(flt, F), terminated, T)) : active_timestep(Story, T);
  story(Story, arg(id(flt, F), terminated, EoT)) : end_of_time(EoT)
} :-
  story(Story, id(flt, F)),
  not story(Story, hypothetical(id(flt, F))).

% Fluents are terminated by something even if that thing is "cleanup":
story(Story, arg(cleanup, terminates, id(flt, F))) :-
  story(Story, arg(id(flt, F), terminated, EoT)),
  end_of_time(EoT).
1 = {
  error(m("Auto-terminated fluent.", Story, T, id(flt, F)));
  story(Story, arg(Action, terminates, id(flt, F))) :
    story(Story, arg(Evt, action, Action)),
    story(Story, happens(T, Evt))
} :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), terminated, T)),
  active_timestep(Story, T).

% No fluent should be initiated after it is terminated:
error(m("Non-hypothetical fluent never holds.", Story, id(flt, F))) :-
  story(Story, id(flt, F)),
  not story(Story, hypothetical(id(flt, F))),
  0 = { story(Story, at(T, id(flt, F))) : time(T) }.

% Fluent ontology rules:
%-----------------------

% Rather than allowing arbitrary fluents, for the sake of consistency, an
% ontology of fluents of various types limits the allowed argument values. 

% Every fluent has a subject:
1 = {
  error(m("Fluent missing subject.", Story, id(flt, F)));
  story(Story, arg(id(flt, F), subject, id(Type, ID))) :
    story(Story, id(Type, ID))
} :-
  story(Story, id(flt, F)).

1 = {
  error(m("State subject/value mismatch.", Story, id(flt, F), id(Type, ID)));
  story(Story, arg(id(flt, F), value, Value)) : state(Type, Value)
} :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), type, state)),
  story(Story, arg(id(flt, F), subject, id(Type, ID))).

1 = {
  error(m("Property subject/domain mismatch.",Story, id(flt, F), id(Type, ID)));
  story(Story, arg(id(flt, F), domain, Domain)) : property(Type, Domain)
} :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), type, property)),
  story(Story, arg(id(flt, F), subject, id(Type, ID))).

1 = {
  error(m("Invalid property value.", Story, id(flt, F), Domain));
  story(Story, arg(id(flt, F), value, Value)) : value(Domain, Value)
} :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), type, property)),
  story(Story, arg(id(flt, F), domain, Domain)).

1 = {
  error(m("Relation subject/value mismatch.",Story,id(flt, F), id(SType, SID)));
  story(Story, arg(id(flt, F), value, Value)) :
    relation(SType, Value, OType),
    type(OType);
  story(Story, arg(id(flt, F), value, Value)) :
    relation(any, Value, OType),
    type(OType);
  story(Story, arg(id(flt, F), value, Value)) :
    relation(SType, Value, any);
  story(Story, arg(id(flt, F), value, Value)) :
    relation(any, Value, any)
} :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), type, relation)),
  story(Story, arg(id(flt, F), subject, id(SType, SID))).

1 = {
  error(
    m(
      "Relation subject/value/object mismatch.",
      Story,
      id(flt, F),
      id(SType, SID),
      Value
    )
  );
  story(Story, arg(id(flt, F), object, id(OType, OID))) :
    relation(SType, Value, OType),
    story(Story, id(OType, OID));
  story(Story, arg(id(flt, F), object, id(OType, OID))) :
    relation(any, Value, OType),
    story(Story, id(OType, OID));
  story(Story, arg(id(flt, F), object, id(OType, OID))) :
    relation(SType, Value, any),
    story(Story, id(OType, OID));
  story(Story, arg(id(flt, F), object, id(OType, OID))) :
    relation(any, Value, any),
    story(Story, id(OType, OID))
} :-
  story(Story, id(flt, F)),
  story(Story, arg(id(flt, F), type, relation)),
  story(Story, arg(id(flt, F), subject, id(SType, SID))),
  story(Story, arg(id(flt, F), value, Value)).

% Basic constraints:
%-------------------

% Constraints such as incompatible, dependent, and exclusive define how fluents
% interact.

% TODO: Allow constructive compatability?

error(m("Incompatible fluents.", Story, T, F1, F2)) :-
  incompatible(V1, V2),
  story(Story, at(T, F1)),
  story(Story, at(T, F2)),
  story(Story, arg(F1, value, V1)),
  story(Story, arg(F2, value, V2)),
  F1 != F2.

error(m("Unmet dependency.", Story, T, Dep, Depends, Value)) :-
  dependent(Depends, Value),
  story(Story, at(T, Dep)),
  story(Story, arg(Dep, value, Depends)),
  0 = {
    Sat :
      story(Story, at(T, Sat)),
      story(Story, arg(Sat, value, Value)),
      Sat != Dep
  }.

error(m("Failed to maintain exclusivity.", Story, T, R1, R2, V, O1, O2)) :-
  exclusive(V),
  story(Story, at(T, R1)),
  story(Story, at(T, R2)),
  story(Story, arg(R1, value, V)),
  story(Story, arg(R2, value, V)),
  story(Story, arg(R1, object, O1)),
  story(Story, arg(R2, object, O2)),
  R1 != R2.
