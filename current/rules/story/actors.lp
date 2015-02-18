% actors.lp
% Special rules for actors. See content/actors/*.lp for actor definitions.

% Actor definition unpacking:
subclass(Imm, Spec) :-
  actor_def(Spec, Imm, Name, Number).

default_name_for(Class, Name) :-
  actor_def(Class, Superclass, Name, Number).

default_number_for(Class, Number) :-
  actor_def(Class, Superclass, Name, Number).

% All plural actors are by default neuter:
default_gender_for(Class, neuter) :-
  default_number_for(Class, plural).

% All other actors can by default be either masculine or feminine:
default_gender_for(Class, feminine) :-
  actor_def(Class, Superclass, Name, Number),
  Number != plural.
default_gender_for(Class, masculine) :-
  actor_def(Class, Superclass, Name, Number),
  Number != plural.
