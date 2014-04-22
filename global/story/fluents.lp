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
flarg(T, subject) :- fltype(T).
flarg(T, value) :- fltype(T).

flarg(property, domain).
flarg(relation, object).

error(m("Fluent with no type.", Story, Fluent, Arg)) :-
  story(Story, Fluent),
  0 = { story(Story, arg(Fluent, type, Type)) : fltype(Type) }.

error(m("Fluent missing argument.", Story, Fluent, Arg)) :-
  story(Story, Fluent),
  story(Story, arg(Fluent, type, Type)),
  flarg(Type, Arg),
  0 = {
    story(Story, arg(Fluent, Arg, Val)) :
      story(Story, arg(Fluent, Arg, Val))
  }.

error(m("Fluent with unknown argument.", Story, Fluent, Arg)) :-
  story(Story, Fluent),
  story(Story, arg(Fluent, type, Type)),
  story(Story, arg(Fluent, Arg, Value)),
  not flarg(Type, Arg).

error(m("Fluent with multiple arguments.", Story, Fluent, Arg, V1, V2)) :-
  story(Story, Fluent),
  story(Story, arg(Fluent, Arg, V1),
  story(Story, arg(Fluent, Arg, V2),
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

story(Story, at(T, Fluent)) :-
  time(T),
  story(Story, initiated(Ti, Fluent)),
  story(Story, terminated(Tt, Fluent)),
  T > Ti, T <= Tt.

error(m("Non-hypothetical fluent never initiated.", Story, Fluent)) :-
  story(Story, id(flt, ID)),
  not story(Story, hypothetical(id(flt, ID))),
  0 = {
    story(Story, initiated(T, id(flt, ID))) : time(T);
    story(Story, initiated(BoT, id(flt, ID)) : beginning_of_time(BoT)
  }.

error(m("Non-hypothetical fluent never terminated.", Story, Fluent)) :-
  story(Story, id(flt, ID)),
  not story(Story, hypothetical(id(flt, ID))),
  0 = {
    story(Story, terminated(T, id(flt, ID))) : time(T);
    story(Story, terminated(EoT, id(flt, ID)) : end_of_time(EoT)
  }.

error(m("Non-hypothetical fluent never holds.", Story, Fluent)) :-
  story(Story, id(flt, ID)),
  not story(Story, hypothetical(id(flt, ID))),
  0 = { story(Story, at(T, id(flt, ID))) : time(T) }.

% Fluent ontology rules:
%-----------------------

% Rather than allowing arbitrary fluents, for the sake of consistency, an
% ontology of fluents of various types limits the allowed argument values. 

error(m("State subject/value mismatch.", Story, F, id(Type, ID), Value)) :-
  story(Story, F),
  story(Story, arg(F, type, state)),
  story(Story, arg(F, subject, id(Type, ID))),
  story(Story, arg(F, value, Value)),
  not state(Type, Value).

error(m("Property subject/domain mismatch.", Story, F, id(Type, ID), Domain)) :-
  story(Story, F),
  story(Story, arg(F, type, property)),
  story(Story, arg(F, subject, id(Type, ID))),
  story(Story, arg(F, domain, Domain)),
  not property(Type, Domain).

error(m("Invalid property value.", Story, F, Domain, Value)) :-
  story(Story, F),
  story(Story, arg(F, type, property)),
  story(Story, arg(F, domain, Domain)),
  story(Story, arg(F, value, Value)),
  not value(Domain, Value).

error(
  m(
    "Relation subject/value/object mismatch.",
    Story,
    F,
    id(SType, SID),
    Value,
    id(OType, OID)
  )
) :-
  story(Story, F),
  story(Story, arg(F, type, relation)),
  story(Story, arg(F, subject, id(SType, SID))),
  story(Story, arg(F, value, Value)),
  story(Story, arg(F, object, id(OType, OID))),
  0 = {
    relation(SType, Value, OType);
    relation(any, Value, OType);
    relation(SType, Value, any);
    relation(any, Value, any)
  }.

% Basic constraints:
%-------------------

% Constraints such as incompatible, dependent, and exclusive define how fluents
% interact.

error(m("Incompatible fluents.", Story, T, F1, F2)) :-
  incompatible(V1, V2),
  story(Story, at(T, F1)),
  story(Story, at(T, F2)),
  story(Story, arg(F1, value, V1)),
  story(Story, arg(F2, value, V2)),
  F1 != F2.

error(m("Unmet dependency.", Story, T, Dep, Depends, Value) :-
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
