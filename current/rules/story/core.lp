% core rules:
% -----------

%#minimize { 1@0, error(Message) : error(Message) }.
:- error(Message).
%:- error(m("Story too short", N)).
%:- error(m("Skipped transition", N)).
%:- error(m("Transition is too early", N)).
%:- error(m("Reached destination before end of story", N, X)).
%:- error(m("Post-travel node without setup.", N, X, Next)).
%:- error(m("Unmotivated action", N, X)).
%:- error(m("Redundant option", N, X)).
%:- error(m("Left a non-hidden problem behind.", N, P)).
%:- error(m("Party-initiated event", N, X)).
%:- error(m("Vignette starts with an event!")).
%:- error(m("Irrelevant option", Node, Option)).
%:- error(m("Unbound action argument.", Node, Action, Arg, ArgType)).
%:- error(m("Price is not a treasure.", Node, Option)).

#const max_options=5.
#const min_story_length=5.
#const min_story_choices=3.
#const max_setup_argument_arity=4.

opt_num(1..max_options).
setup_arg_id(1..max_setup_argument_arity).


% Core node construction:
% -----------------------

% Choice nodes have at least two options:
2 <= {
  at(N, option(X)) : opt_num(X);
  error(m("Choice node missing option 1.", N));
  error(m("Choice node missing option 2.", N))
} <= max_options :-
  node_type(N, choice),
  story_op(N, build_options).

% Ending and event nodes have exactly one "option:"
at(N, option(1)) :-
  node_type(N, ending),
  story_op(N, build_options).

at(N, option(1)) :-
  node_type(N, event),
  story_op(N, build_options).


% Misc rules:
% -----------

% The concept of an "initiator:"
at(N, initiator(X, Initiator)) :-
  at(N, action(X, Action)),
  at(N, arg(X, InitArg, Initiator)),
  initiator(Action, InitArg),
  story_op(N, build_options).

% An action without an initiator is an error:
error(m("Action without initiator.", N, X)) :-
  at(N, option(X)),
  0 = {
    at(N, initiator(option(X), Anyone))
  }.


% State and action rules:
% -----------------------

% Exclusive relationships:
exclusive(has_item).

at(N, consequence_of(X, O, _not, relation(Rel, Old, Obj))) :-
  at(N, consequence_of(X, O, relation(Rel, New, Obj))),
  exclusive(Rel),
  st(N, relation(Rel, Old, Obj)),
  Old != New,
  story_op(N, build_options).

% Most actions can't be performed reflexively:
error(m("Improperly reflexive action.", N, Opt)) :-
  reflexive(N, Opt),
  at(N, action(Opt, Action)),
  not reflexive(Action).

reflexive(N, Opt) :-
  at(N, arg(Opt, Arg1, inst(Type, ID))),
  at(N, arg(Opt, Arg2, inst(Type, ID))),
  Arg1 < Arg2.


% Typing rules:
% -------------

general_category(actor).
general_category(item).
% TODO: locations?

category(N, Thing, Category) :-
  is_instance(N, Thing, Category),
  general_category(Category).

category_for(Category, Category) :-
  general_category(Category).

category_for(Class, Category) :-
  subclass(Category, Class),
  general_category(Category).

error(m("Instance doesn't belong to a genereal category.", N, I)) :-
  st(N, inst(Type, I)),
  0 = {
    category(N, inst(Type, I), Category)
  }.

is_instance(N, Thing, Class) :-
  st(N, property(type, Thing, Class)).

is_instance(N, Thing, Class) :-
  is_instance(N, Thing, Subclass),
  subclass(Class, Subclass).

subclass(General, Specific) :-
  subclass(General, Intermediate),
  subclass(Intermediate, Specific).

concrete_class_of(Abstract, Concrete) :-
  subclass(Abstract, Concrete),
  concrete(Concrete).

concrete_class_of(Concrete, Concrete) :-
  concrete(Concrete).
