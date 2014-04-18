% bind.lp
% Rules to support dynamic binding of references to ids.

% TODO: Should double-binding be restricted?
%error(m("Double binding.", id(Type, ID))) :-
%  bound(R1, id(Type, ID)),
%  bound(R2, id(Type, ID)),
%  R1 != R2.

error(m("Could not satisfy required property.", Property)) :-
  require(Property),
  not satisfied(require(Property)).

error(m("Could not ensure property.", Property)) :-
  ensure(Property),
  not satisfied(ensure(Property)).

% Requiring args, traits, states, properties, and relations (requiring their
% presence in the input):

satisfied(require(arg(id(evt, EID), Arg, Ref))) :-
  bound(Ref, id(Type, ID)),
  require(arg(id(evt, EID), Arg, Ref)),
  story(current, arg(id(evt, EID), arg, id(Type, ID))).

satisfied(require(arg(ERef, Arg, Ref))) :-
  bound(ERef, id(EType, EID)),
  bound(Ref, id(Type, ID)),
  require(arg(ERef, Arg, Ref)),
  story(current, arg(id(EType, EID), arg, id(Type, ID))).

satisfied(require(trait(Ref, Trait))) :-
  bound(Ref, id(Type, ID)),
  require(trait(Ref, Trait)),
  story(current, trait(id(Type, ID), Trait)).

satisfied(require(at(T, state(Ref, State)))) :-
  bound(Ref, id(Type, ID)),
  require(at(T, state(Ref, State))),
  story(current, at(T, state(id(Type, ID), State))).

satisfied(require(at(T, property(Ref, Prop, Val)))) :-
  bound(Ref, id(Type, ID)),
  require(at(T, property(Ref, Prop, Val))),
  story(current, at(T, property(id(Type, ID), Prop, Val))).

satisfied(require(at(T, rel(Ref, Rel, id(OType, OID))))) :-
  bound(Ref, id(Type, ID)),
  require(at(T, rel(Ref, Rel, id(OType, OID)))),
  story(current, at(T, rel(id(Type, ID), Rel, id(OType, OID)))).

satisfied(require(at(T, rel(id(OType, OID), Rel, Ref)))) :-
  bound(Ref, id(Type, ID)),
  require(at(T, rel(id(OType, OID), Rel, Ref))),
  story(current, at(T, rel(id(OType, OID), Rel, id(Type, ID)))).

satisfied(require(at(T, rel(R1, Rel, R2)))) :-
  bound(R1, id(T1, ID1)),
  bound(R2, id(T2, ID2)),
  require(at(T, rel(R1, Rel, R2))),
  story(current, at(T, rel(id(T1, ID1), Rel, id(T2, ID2)))).

% Ensuring args, traits, states, properties, and relations (requiring their
% presence in the output):

satisfied(ensure(arg(id(evt, EID), Arg, Ref))) :-
  bound(Ref, id(Type, ID)),
  ensure(arg(id(evt, EID), Arg, Ref)),
  story(proposed, arg(id(evt, EID), arg, id(Type, ID))).

satisfied(ensure(arg(ERef, Arg, Ref))) :-
  bound(ERef, id(EType, EID)),
  bound(Ref, id(Type, ID)),
  ensure(arg(ERef, Arg, Ref)),
  story(proposed, arg(id(EType, EID), arg, id(Type, ID))).

satisfied(ensure(trait(Ref, Trait))) :-
  bound(Ref, id(Type, ID)),
  ensure(trait(Ref, Trait)),
  story(proposed, trait(id(Type, ID), Trait)).

satisfied(ensure(at(T, state(Ref, State)))) :-
  bound(Ref, id(Type, ID)),
  ensure(at(T, state(Ref, State))),
  story(proposed, at(T, state(id(Type, ID), State))).

satisfied(ensure(at(T, property(Ref, Prop, Val)))) :-
  bound(Ref, id(Type, ID)),
  ensure(at(T, property(Ref, Prop, Val))),
  story(proposed, at(T, property(id(Type, ID), Prop, Val))).

satisfied(ensure(at(T, rel(Ref, Rel, id(OType, OID))))) :-
  bound(Ref, id(Type, ID)),
  ensure(at(T, rel(Ref, Rel, id(OType, OID)))),
  story(proposed, at(T, rel(id(Type, ID), Rel, id(OType, OID)))).

satisfied(ensure(at(T, rel(id(OType, OID), Rel, Ref)))) :-
  bound(Ref, id(Type, ID)),
  ensure(at(T, rel(id(OType, OID), Rel, Ref))),
  story(proposed, at(T, rel(id(OType, OID), Rel, id(Type, ID)))).

satisfied(ensure(at(T, rel(R1, Rel, R2)))) :-
  bound(R1, id(T1, ID1)),
  bound(R2, id(T2, ID2)),
  ensure(at(T, rel(R1, Rel, R2))),
  story(proposed, at(T, rel(id(T1, ID1), Rel, id(T2, ID2)))).

% Asserting args, traits, states, properties, and relations (adding them to the
% output):

story(proposed, arg(id(evt, EID), Arg, id(Type, ID))) :-
  bound(Ref, id(Type, ID)),
  assert(arg(id(evt, EID), Arg, Ref)).

story(proposed, arg(id(evt, EID), Arg, id(Type, ID))) :-
  bound(Ref, id(Type, ID)),
  bound(ERef, id(evt, EID)),
  assert(arg(ERef, Arg, Ref)).

story(proposed, trait(id(Type, ID), Trait)) :-
  bound(Ref, id(Type, ID)),
  assert(trait(Ref, Trait)).

story(proposed, at(T, state(id(Type, ID), State))) :-
  bound(Ref, id(Type, ID)),
  assert(at(T, state(Ref, State))).

story(proposed, at(T, property(id(Type, ID), Prop, Val))) :-
  bound(Ref, id(Type, ID)),
  assert(at(T, property(Ref, Prop, Val))).

story(proposed, at(T, rel(id(Type, ID), Rel, id(OType, OID)))) :-
  bound(Ref, id(Type, ID)),
  assert(at(T, rel(Ref, Rel, id(OType, OID)))).

story(proposed, at(T, rel(id(OType, OID), Rel, id(Type, ID)))) :-
  bound(Ref, id(Type, ID)),
  assert(at(T, rel(id(OType, OID), Rel, Ref))).

story(proposed, at(T, rel(id(T1, ID1), Rel, id(T2, ID2)))) :-
  bound(R1, id(T1, ID1)),
  bound(R2, id(T2, ID2)),
  assert(at(T, rel(R1, Rel, R2))).
