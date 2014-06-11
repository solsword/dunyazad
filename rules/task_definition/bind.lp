% bind.lp
% Rules to support dynamic binding of references to ids.

% TODO: Should double-binding be restricted?
%error(m("Double binding.", id(Type, ID))) :-
%  bound(R1, id(Type, ID)),
%  bound(R2, id(Type, ID)),
%  R1 != R2.

% Require means that the stated property should be true in the given story.

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, arg(Subj, Arg, Ref)),
  bound(Ref, Bound),
  not story(Story, arg(Subj, Arg, Bound)).

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, arg(Ref, Arg, Obj)),
  bound(Ref, Bound),
  not story(Story, arg(Bound, Arg, Obj)).

error(m("Failed to satisfy requirements on", SRef, ORef)) :-
  require(Story, arg(SRef, Arg, ORef)),
  bound(SRef, SBound),
  bound(ORef, OBound),
  not story(Story, arg(SBound, Arg, OBound)).

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, at(T, Ref)),
  bound(Ref, Bound),
  not story(Story, at(T, Bound)).

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, initiated(T, Ref)),
  bound(Ref, Bound),
  not story(Story, initiated(T, Bound)).

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, initiated(before(T), Ref)),
  bound(Ref, Bound),
  0 = { story(Story, initiated(Ti, Bound)) : Ti < T }.

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, initiated(after(T), Ref)),
  bound(Ref, Bound),
  0 = { story(Story, initiated(Ti, Bound)): Ti > T }.

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, terminated(T, Ref)),
  bound(Ref, Bound),
  not story(Story, terminated(T, Bound)).

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, terminated(before(T), Ref)),
  bound(Ref, Bound),
  0 = { story(Story, terminated(Tt, Bound)): Tt < T }.

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, terminated(after(T), Ref)),
  bound(Ref, Bound),
  0 = { story(Story, terminated(Tt, Bound)): Tt > T }.

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, happens(T, Ref)),
  bound(Ref, Bound),
  not story(Story, happens(T, Bound)).

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, happens(after(T), Ref)),
  bound(Ref, Bound),
  0 = { story(Story, happens(Th, Bound)): Th < T }.

error(m("Failed to satisfy requirement on", Ref)) :-
  require(Story, happens(after(T), Ref)),
  bound(Ref, Bound),
  0 = { story(Story, happens(Th, Bound)): Th > T }.

% Asserting args, traits, states, properties, and relations (adding them to the
% output):

story(proposed, arg(Thing, Arg, Bound)) :-
  bound(Ref, Bound),
  assert(arg(Thing, Arg, Ref)).

story(proposed, arg(Bound, Arg, Thing)) :-
  bound(Ref, Bound),
  assert(arg(Ref, Arg, Thing)).

story(proposed, arg(B1, Arg, B2)) :-
  bound(R1, B1),
  bound(R2, B2),
  assert(arg(R1, Arg, R2)).

story(proposed, at(T, Bound)) :-
  bound(Ref, Bound),
  assert(at(T, Ref)).

story(proposed, happens(T, Bound)) :-
  bound(Ref, Bound),
  assert(happens(T, Ref)).
