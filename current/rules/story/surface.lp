% surface.lp
% Rules about surface text realization

% Gender values:
gender(masculine).
gender(feminine).
gender(neuter).

number(singular).
number(plural).

% Default values for name, person, number, gender, and determination:

1 = {
  surface_property(name, inst(Type, ID), Name)
    : surface_property(name, inst(Type, ID), Name);
  surface_property(name, inst(Type, ID), Name)
    : default_name_for(Class, Name),
      st(N, property(type, inst(Type, ID), Class));
  error(m("Error with instance name.", inst(Type, ID)))
} :-
  st(N, inst(Type, ID)).

1 = {
  surface_property(person, inst(Type, ID), Name)
    : surface_property(person, inst(Type, ID), Name);
  surface_property(person, inst(Type, ID), third);
  error(m("Error with instance person.", inst(Type, ID)))
} :-
  st(N, inst(Type, ID)).

1 = {
  surface_property(number, inst(Type, ID), Number)
    : surface_property(number, inst(Type, ID), Number);
  surface_property(number, inst(Type, ID), Number)
    : default_number_for(Class, Number),
      st(N, property(type, inst(Type, ID), Class));
  error(m("Error with instance number.", inst(Type, ID)))
} :-
  st(N, inst(Type, ID)).

1 = {
  surface_property(gender, inst(Type, ID), Gender)
    : surface_property(gender, inst(Type, ID), Gender);
  surface_property(gender, inst(Type, ID), Gender)
    : default_gender_for(Class, Gender),
      st(N, property(type, inst(Type, ID), Class));
  error(m("Error with spontaneous instance gender.", inst(Type, ID)))
} :-
  st(N, inst(Type, ID)).

1 = {
  surface_property(determined, inst(Type, ID), Det) :
    surface_property(determined, inst(Type, ID), Det);
  surface_property(determined, inst(Type, ID), true);
  error(m("Error with instance determination.", inst(Type, ID)))
} :-
  st(N, inst(Type, ID)).
