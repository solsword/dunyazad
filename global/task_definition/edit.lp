% edit.lp
% Rules about how to construct a modified story state given the existing story
% state and some rules about what the modification is. Allows for writing
% simple/generic modifications and solving for modified stories consistent with
% those (perhaps partially specified) changes.

% In the absence of explicit modifications, story elements are preserved:
proposed(X) :- story(X), not drop(X).

% Source-tracked proposals:
proposed(X) :- proposed_by(Proposer, X).

% Source-tracked proposals are exclusive:
:- proposed_by(Proposer, X), proposed_by(Other, X), Proposer != Other.

% Limit the number of possible proposed IDs to choke off grounding issues (this
% also makes sense from a storytelling perspecitve):
available_id(Type, X) :-
  X >= 1, X <= 7,
  not story(id, Type, X).

%%%%%%%%%%%%%%%
% Edit rules: %
%%%%%%%%%%%%%%%

% Low-level editing:
%-------------------

% drop:
drop(Fact) :-
  edit(drop, Fact).

% add:
proposed(Fact) :-
  edit(add, Fact).

% replace:
proposed(Replace) :-
  story(Fact),
  edit(replace, Fact, Replace).

% Reference creation:
%--------------------

% new:
% Each new proposes an available id...
1 = {
  error(m("Out of avaialable IDs for type.", edit(new, Type, Ref)));
  proposed_by(
    edit(new, Type, Ref),
    id(Type, ID)
  ) : available_id(Type, ID)
} :-
  edit(new, Type, Ref).

% ...and also binds a proposed non-story id
1 = {
  bound(Ref, id(Type, ID)) :
    proposed(id(Type, ID)),
    not story(id(Type, ID))
} :-
  edit(new, Type, Ref).

% some:
% Each some statement may propose an available id but doesn't have to...
0 <= {
  proposed_by(edit(some, Type, Ref), id(Type, ID)) : available_id(Type, ID)
} <= 1 :-
  edit(some, Type, Ref).

% ...and it will bind any proposed id, including one forwarded from the story
% or proposed by a different some or new.
1 = {
  error(m("Nothing for some to bind to!", edit(some, Type, Ref)));
  bound(Ref, id(Type, ID))
} :-
  edit(some, Type, Ref),
  proposed(id(Type, ID)).

% Event manipulation:
%--------------------

% TODO: HERE
