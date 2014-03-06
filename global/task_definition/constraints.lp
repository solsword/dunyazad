% constraints.lp
% The ability to create some abstract constraints simplifies the writing of
% tasks and also some of the meta-task language rules. This module defines how
% constraints are violated, and specifies that they never should be.

% No constraints should ever be violated:
:- violated_constraint( Type, Specific, Value ).

% holds_at for properties:
violated_constraint(
  holds_at,
  at(StoryNode),
  property( id(Type, ID), prop(Property) )
) :-
  constraint(
    holds_at,
    at(StoryNode),
    property( id(Type, ID), prop(Property) )
  ),
  not holds_at( at(StoryNode), property( id(Type, ID), prop(Property) ) ).

% holds_at for relations:
violated_constraint(
  holds_at,
  at(StoryNode),
  relation(
    id(SubjectType, SubjectID),
    rel(
      Relation,
      id(ObjectType, ObjectID)
    )
  )
) :-
  constraint(
    holds_at,
    at(StoryNode),
    relation(
      id(SubjectType, SubjectID),
      rel(
        Relation,
        id(ObjectType, ObjectID)
      )
    )
  ),
  not holds_at(
    at(StoryNode),
    relation(
      id(SubjectType, SubjectID),
      rel(
        Relation,
        id(ObjectType, ObjectID)
      )
    )
  ).

% may_hold_at for properties:
% mutually_exclusive rule:
violated_constraint(
  may_hold_at,
  at(StoryNode),
  property( id(Type, ID), prop(Property) )
) :-
  constraint(
    may_hold_at,
    at(StoryNode),
    property( id(Type, ID), prop(Property) )
  ),
  holds_at( at(StoryNode), property( id(Type, ID), prop(Excluded),
  mutually_exclusive( prop(Property), prop(Excluded) ).

% TODO: More conditions for may_hold_at?

% may_hold_at for relations:
% same_object_exclusive rule:
violated_constraint(
  may_hold_at,
  at(StoryNode),
  relation(
    id(SubjectType, SubjectID),
    rel(
      Relation,
      id(ObjectType, ObjectID)
    )
  ),
  excluded_by,
  Excluded
) :-
  constraint(
    may_hold_at,
    at(StoryNode),
    relation(
      id(SubjectType, SubjectID),
      rel(
        Relation,
        id(ObjectType, ObjectID)
      )
    )
  ),
  same_object_exclusive( rel(Relation), rel(Excluded) ),
  holds_at(
    at(StoryNode),
    relation(
      id(SubjectType, SubjectID),
      rel(
        Excluded,
        id(ObjectType, ObjectID)
      )
    )
  ).

% unique_per_subject rule:
violated_constraint(
  may_hold_at,
  at(StoryNode),
  relation(
    id(SubjectType, SubjectID),
    rel(
      Relation,
      id(ObjectType, ObjectID)
    )
  ),
  unique_rel_already_specified,
  id(OtherType, OtherID)
) :-
  constraint(
    may_hold_at,
    at(StoryNode),
    relation(
      id(SubjectType, SubjectID),
      rel(
        Relation,
        id(ObjectType, ObjectID)
      )
    )
  ),
  unique_per_subject( rel(Relation) ),
  holds_at(
    at(StoryNode),
    relation(
      id(SubjectType, SubjectID),
      rel(
        Relation,
        id(OtherType, OtherID)
      )
    ),
    id(OtherType, OtherID) != id(ObjectType, ObjectID).
  ).

% TODO: More conditions for may_hold_at?
