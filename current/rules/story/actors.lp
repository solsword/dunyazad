% actors.lp
% Special rules for actors. See content/actors/*.lp for actor definitions.

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
subclass(actor, animal).
