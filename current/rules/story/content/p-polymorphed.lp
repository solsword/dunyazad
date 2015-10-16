% polymorphed
potential(problem, property, polymorphed, Any) :- any_class(Any).

pcategory(potential(problem, property, polymorphed, Any), urgent) :-
  any_class(Any).

pcategory(potential(problem, property, polymorphed, Any), persistent) :-
  any_class(Any).

problematic_for(problem, property, polymorphed, Any, inst) :- any_class(Any).

% When you become polymorphed, you become a chicken:
at(N, consequence_of(X, O, property(type, Target, chicken))) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  story_op(N, build_options).

% You also gain the "unintelligent" skill:
at(N, consequence_of(X, O, property(has_skill, Target, unintelligent))) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  story_op(N, build_options).

% You also drop all of your items:
% TODO: Some way to pick them back up!
% TODO: Some concept of item ownership!
at(N, consequence_of(X, O, _not, relation(has_item, Target, Item))) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  st(N, relation(has_item, Target, Item)),
  story_op(N, build_options).


% When you're polymporhed back, you regain your original type and lose the
% unintelligent skill unless it's a class skill for your original type:

at(N, consequence_of(X, O, property(type, Target, Original))) :-
  at(N, consequence_of(X, O, _not, property(polymorphed, Target, Original))),
  story_op(N, build_options).

at(N, consequence_of(X, O, _not, property(has_skill, Target, unintelligent))) :-
  at(N, consequence_of(X, O, _not, property(polymorphed, Target, Original))),
  0 = {
    class_skill(SomeClass, unintelligent, always) :
      subclass(Original, SomeClass),
      class_skill(SomeClass, unintelligent, always);
    class_skill(Original, unintelligent, always) :
      class_skill(Original, unintelligent, always)
  },
  story_op(N, build_options).

% Becomming polymorphed cancels several other potentials:

at(
  N,
  consequence_of(
    X,
    O,
    nullifies,
    potential(opportunity, state(knows_gossip, Target))
  )
) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  at(N, potential(opportunity, state(knows_gossip, Target))),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    O,
    nullifies,
    potential(opportunity, state(bored, Target))
  )
) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  at(N, potential(opportunity, state(bored, Target))),
  story_op(N, build_options).

% yes, polymorphing someone is an extreme way of healing them...
at(
  N,
  consequence_of(
    X,
    O,
    nullifies,
    potential(problem, state(injured, Target))
  )
) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  at(N, potential(problem, state(injured, Target))),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    O,
    nullifies,
    potential(opportunity, property(offering_service, Target, Anything))
  )
) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  at(N, potential(opportunity, property(offering_service, Target, Anything))),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    O,
    nullifies,
    potential(opportunity, relation(selling, Target, Anything))
  )
) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  at(N, potential(opportunity, relation(selling, Target, Anything))),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    O,
    nullifies,
    potential(problem, relation(accusing, Target, Anyone))
  )
) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  at(N, potential(problem, relation(accusing, Target, Anyone))),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    O,
    nullifies,
    potential(problem, relation(threatening, Target, Anyone))
  )
) :-
  at(N, consequence_of(X, O, property(polymorphed, Target, Any))),
  at(N, potential(problem, relation(threatening, Target, Anyone))),
  story_op(N, build_options).
