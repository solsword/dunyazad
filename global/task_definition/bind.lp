% bind.lp
% Rules to support dynamic binding of references to ids.

% TODO: Should double-binding be restricted?
%error(m("Double binding.", id(Type, ID))) :-
%  bound(R1, id(Type, ID)),
%  bound(R2, id(Type, ID)),
%  R1 != R2.

error(m("Could not satisfy required property.", Story, Property)) :-
  require(Story, Property),
  not satisfied(require(Story, Property)).

% Require means that the stated property should be true in the given story.

satisfied(require(Story, Ref)) :-
  require(Story, Ref),
  bound(Ref, Bound),
  story(Story, Bound).

satisfied(require(Story, arg(Subj, Arg, Ref))) :-
  require(Story, arg(Subj, Arg, Ref)),
  bound(Ref, Bound),
  story(Story, arg(Subj, Arg, Bound)).

satisfied(require(Story, arg(Ref, Arg, Obj))) :-
  require(Story, arg(Ref, Arg, Obj)),
  bound(Ref, Bound),
  story(Story, arg(Bound, Arg, Obj)).

satisfied(require(Story, arg(SRef, Arg, ORef))) :-
  require(Story, arg(RefO, Arg, RefA)),
  bound(SRef, SBound),
  bound(ORef, OBound),
  story(Story, arg(SBound, Arg, OBound)).

satisfied(require(Story, at(T, Ref))) :-
  require(Story, at(T, Ref)),
  bound(Ref, Bound),
  story(Story, at(T, Bound)).

satisfied(require(Story, initiated(T, Ref))) :-
  require(Story, initiated(T, Ref)),
  bound(Ref, Bound),
  story(Story, initiated(T, Bound)).

satisfied(require(Story, initiated(before(T), Ref))) :-
  require(Story, initiated(before(T), Ref)),
  bound(Ref, Bound),
  story(Story, initiated(Ti, Bound)),
  Ti < T.

satisfied(require(Story, initiated(after(T), Ref))) :-
  require(Story, initiated(after(T), Ref)),
  bound(Ref, Bound),
  story(Story, initiated(Ti, Bound)),
  Ti > T.

satisfied(require(Story, terminated(T, Ref))) :-
  require(Story, terminated(T, Ref)),
  bound(Ref, Bound),
  story(Story, terminated(T, Bound)).

satisfied(require(Story, terminated(before(T), Ref))) :-
  require(Story, terminated(before(T), Ref)),
  bound(Ref, Bound),
  story(Story, terminated(Tt, Bound)),
  Tt < T.

satisfied(require(Story, terminated(after(T), Ref))) :-
  require(Story, terminated(after(T), Ref)),
  bound(Ref, Bound),
  story(Story, terminated(Tt, Bound)),
  Tt > T.

satisfied(require(Story, happens(T, Ref))) :-
  require(Story, happens(T, Ref)),
  bound(Ref, Bound),
  story(Story, happens(T, Bound)).

satisfied(require(Story, happens(before(T), Ref))) :-
  require(Story, happens(after(T), Ref)),
  bound(Ref, Bound),
  story(Story, happens(Th, Bound)),
  Th < T.

satisfied(require(Story, happens(after(T), Ref))) :-
  require(Story, happens(after(T), Ref)),
  bound(Ref, Bound),
  story(Story, happens(Th, Bound)),
  Th > T.

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
