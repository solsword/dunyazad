% some.lp
% Supports defining task-local variables that will be created if they don't
% match an existing story element (there's also a mode for variables that must
% match an existing element).

% To define a task-local variable, use a predicate:
%
%   some( id(Type, VarID) ).
%
% To define a required variable:
%
%   definitely( id(Type, VarID) ).
%
% In both cases required properties are specified using the same syntax with a
% second argument, e.g.:
%
%   some( id(chr, protagonist), intrinsic, rel(child_of, id(chr, father)) ).
%
% Note in this example the use of a task-local variable in the required
% property.

% some() might create a binding:
0 <= { bound( id(Type, VarID), id(Type, BoundID) ) } <= 1 :-
  some( id(Type, VarID) ),
  id(Type, BoundID).

% definitely() must create a binding:
1 = { bound( id(Type, VarID), id(Type, BoundID) ) } :-
  definitely( id(Type, VarID) ),
  id(Type, BoundID).

% if there isn't a binding in the some() case, call for the creation of an
% appropriate entity:
cmd_fabricate_story_element( id(Type, VarID) ) :-
  some( id(Type, VarID) ),
  0 = {
    bound( id(Type, VarID), id(Type, BoundID) ) :
      some( id(Type, VarID) ),
      bound( id(Type, VarID), id(Type, BoundID) )
  }.

% definite properties
constraint(
  ConstraintType,
  ConstraintArgument,
  property( id(Type, BoundID), prop(Property) )
) :-
  bound( id(Type, VarID), id(Type, BoundID) ),
  definitely(
    ConstraintType,
    ConstraintArgument,
    property( id(Type, VarID), prop(Property) )
  ).


% indefinite properties
constraint(
  may_hold_at,
  at(StoryNode),
  property( id(Type, BoundID), prop(Property) )
) :-
  bound( id(Type, VarID), id(Type, BoundID) ),
  some(
    holds_at,
    at(StoryNode),
    property( id(Type, VarID), prop(Property) )
  ).


