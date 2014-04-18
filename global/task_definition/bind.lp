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

% Requiring traits, states, and relations (requiring their presence in the
% input):

satisfied(require(trait(Ref, Trait))) :-
  bound(Ref, id(Type, ID)),
  require(trait(Ref, Trait)),
  story(current, trait(id(Type, ID), Trait)).

satisfied(require(at(T, state(Ref, State)))) :-
  bound(Ref, id(Type, ID)),
  require(at(T, state(Ref, State))),
  story(current, at(T, state(id(Type, ID), State))).

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

% Ensuring traits, states, and relations (reuquiring their presence in the
% output):

satisfied(ensure(trait(Ref, Trait))) :-
  bound(Ref, id(Type, ID)),
  ensure(trait(Ref, Trait)),
  story(proposed, trait(id(Type, ID), Trait)).

satisfied(ensure(at(T, state(Ref, State)))) :-
  bound(Ref, id(Type, ID)),
  ensure(at(T, state(Ref, State))),
  story(proposed, at(T, state(id(Type, ID), State))).

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
