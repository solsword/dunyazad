% fluents.lp
% Fluent coherence.

% basic initiation and termination:

holds(after(evt(X)), fl(Fl, Args)) :-
  holds(before(evt(X)), fl(Fl, Args)),
  not terminates(evt(X), fl(Fl, Args)).

holds(after(evt(X)), fl(Fl, Args)) :-
  initiates(evt(X), fl(Fl, Args)).

holds(before(evt(X)), fl(Fl, Args)) :-
  holds(after(evt(B)), fl(Fl, Args)),
  event_next(evt(B), evt(X)).

% conflicting fluents cannot coexist:

error(m("Conflicting fluents.")) :-
  holds(T, fl(Fl, Args)),
  holds(T, fl(Fcon, Acon)),
  conflicts(fl(Fl, Args), fl(Fcon, Acon)).

% fluent conflict is reciprocal:
conflicts(F1, F2) :- conflicts(F2, F1).

% fluents cannot hold without satisfying dependencies:

error(m("Unmet fluent dependency.")) :-
  holds(T, fl(Fdep, Adep)),
  depends(fl(Fdep, Adep), fl(Fl, Args)),
  not holds(T, fl(Fl, Args)).

% binary statuses:

status_opposite(Status, _not(Status)) :-
  status(Type, Status).

status_opposite(_not(Status), Status) :-
  status(Type, Status).

fluent(a(Type), status(Status)) :-
  status(Type, Status).

fluent(a(Type), status(_not(Status))) :-
  status(Type, Status).

conflicts(
  fl(status(Status), a(id(Type, Id))),
  fl(status(_not(Status)), a(id(Type, Id)))
) :-
  id(Type, Id),
  status(Type, Status).

depends(
  fl(status(Req), a(id(Type, Id))),
  fl(status(Dep), a(id(Type, Id)))
) :-
  id(Type, Id),
  status(Type, Req),
  status_depends_status(Req, Dep).

depends(
  fl(status(Req), a(id(Type, Id))),
  fl(state(Dep, DVal), a(id(Type, Id)))
) :-
  id(Type, Id),
  status(Type, Req),
  status_depends_state(Req, Dep, DVal).

conflicts(
  fl(status(Status), a(id(Type, Id))),
  fl(status(Conflict), a(id(Type, Id)))
) :-
  id(Type, Id),
  status(Type, Status),
  status_conflicts_status(Status, Conflict).

conflicts(
  fl(status(Status), a(id(Type, Id))),
  fl(state(Conflict, CVal), a(id(Type, Id)))
) :-
  id(Type, Id),
  status(Type, Status),
  status_conflicts_state(Status, Conflict, CVal).

% exclusive multi-valued states:

fluent(a(Type), state(State, Value)) :-
  state(Type, State),
  value(State, Value).

conflicts(
  fl(state(State, Value), a(id(Type, Id))),
  fl(state(State, Vother), a(id(Type, Id)))
) :-
  id(Type, Id),
  state(Type, State),
  state_value(State, Value),
  state_value(State, Vother),
  Value != Vother.

depends(
  fl(state(State, Value), a(id(Type, Id))),
  fl(status(Dep), a(id(Type, Id)))
) :-
  id(Type, Id)
  state(Type, State),
  state_depends_status(State, Value, Dep).

depends(
  fl(state(State, Value), a(id(Type, Id))),
  fl(state(Dep, DVal), a(id(Type, Id)))
) :-
  id(Type, Id)
  state(Type, State),
  state_depends_state(State, Value, Dep, DVal).

conflicts(
  fl(state(State, Value), a(id(Type, Id))),
  fl(status(Status), a(id(Type, Id)))
) :-
  id(Type, Id),
  state(Type, State),
  state_conflicts_status(State, Value, Status).

conflicts(
  fl(state(State, Value), a(id(Type, Id))),
  fl(state(Conflict, CVal), a(id(Type, Id)))
) :-
  id(Type, Id),
  state(Type, State),
  state_conflicts_state(State, Value, Conflict, CVal).

% exclusive multi-valued relationships:

fluent(a(T1, T2), rel(Rel, Value)) :-
  rel(T1, T2, Rel),
  rel_value(Rel, Value).

conflicts(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(rel(Rel, Vother), a(id(T1, I1), id(T2, I2)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_value(Rel, Value),
  rel_value(Rel, Vother),
  Value != Vother.

depends(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(status(Status), a(id(T1, I1)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_depends_subject_status(Rel, Value, Status).

depends(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(status(Status), a(id(T2, I2)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_depends_object_status(Rel, Value, Status).

depends(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(state(State, SVal), a(id(T1, I1)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_depends_subject_state(Rel, Value, State, SVal).

depends(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(state(State, SVal), a(id(T2, I2)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_depends_object_state(Rel, Value, State, SVal).

depends(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(rel(DRel, DVal), a(id(T1, I1), id(T2, I2)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_depends_subject_object_rel(Rel, Value, DRel, DVal).

depends(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(rel(DRel, DVal), a(id(T2, I2), id(T1, I1)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_depends_object_subject_rel(Rel, Value, DRel, DVal).

conflicts(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(status(Status), a(id(T1, I1)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_conflicts_subject_status(Rel, Value, Status).

conflicts(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(status(Status), a(id(T2, I2)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_conflicts_object_status(Rel, Value, Status).

conflicts(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(state(State, SVal), a(id(T1, I1)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_conflicts_subject_state(Rel, Value, State, SVal).

conflicts(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(state(State, SVal), a(id(T2, I2)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_conflicts_object_state(Rel, Value, State, SVal).

conflicts(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(rel(CRel, CVal), a(id(T1, I1), id(T2, I2)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_conflicts_subject_object_rel(Rel, Value, CRel, CVal).

conflicts(
  fl(rel(Rel, Value), a(id(T1, I1), id(T2, I2))),
  fl(rel(CRel, CVal), a(id(T2, I2), id(T1, I1)))
) :-
  id(T1, I1),
  id(T2, I2),
  rel(T1, T2, Rel),
  rel_conflicts_object_subject_rel(Rel, Value, CRel, CVal).
