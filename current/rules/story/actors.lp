% actors.lp
% Special rules for actors. See content/actors/*.lp for actor
% definitions.

% Actor definition unpacking:
subclass(Imm, Spec) :-
  actor_def(Spec, Imm, Name, Number, Gender).

concrete(Spec) :-
  actor_def(Spec, Imm, Name, Number, Gender).

default_name_for(Class, Name) :-
  actor_def(Class, Superclass, Name, Number, Gender).

default_number_for(Class, Number) :-
  actor_def(Class, Superclass, Name, Number, Gender).

default_gender_for(Class, Gender) :-
  gender(Gender),
  actor_def(Class, Superclass, Name, Number, Gender).

default_gender_for(Class, masculine) :-
  actor_def(Class, Superclass, Name, Number, either).

default_gender_for(Class, feminine) :-
  actor_def(Class, Superclass, Name, Number, either).

% Some  high-level actor ontology structure:
subclass(actor, person).
concrete(person).
default_name_for(person, "person").
default_number_for(person, singular).
default_gender_for(person, masculine).
default_gender_for(person, feminine).
subclass(actor, animal).
concrete(animal).
default_name_for(animal, "animal").
default_number_for(animal, singular).
default_gender_for(animal, masculine).
default_gender_for(animal, feminine).
class_skill(animal, wilderness_lore, always).
class_skill(animal, unintelligent, always).

% Power relations:

at(N, is_powerful(Inst)) :-
  is_instance(N, Inst, Category),
  powerful(Category).

at(N, is_powerless(Inst)) :-
  is_instance(N, Inst, Category),
  powerless(Category).
