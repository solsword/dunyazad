% symbols.lp
% Special categories for arguments.

symbol(person_gender, masculine).
symbol(person_gender, feminine).
% TODO: other genders?

symbol(type_of(BaseType), Subtype) :-
  subclass(BaseType, Subtype).
