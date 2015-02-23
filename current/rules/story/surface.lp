% surface.lp
% Rules about surface text realization

error(m("Missing intro text.", N, Setup)) :-
  setup(N, Setup),
  story_op(N, add_surface),
  0 = {
    intro_text(N, Setup, Text)
  }.

error(m("Missing potential text.", N, Type, Fact)) :-
  at(N, potential(Type, Fact)),
  story_op(N, add_surface),
  0 = {
    potential_text(N, Text)
  }.

% Gender values:
gender(masculine).
gender(feminine).
gender(neuter).

number(singular).
number(plural).

% Default values for name, person, number, gender, and determination:

1 = {
  sp_st(N, property(name, inst(Type, ID), Name))
    : sp_st(N, property(name, inst(Type, ID), Name));
  sp_st(N, property(name, inst(Type, ID), Name))
    : default_name_for(Class, Name),
      sp_st(N, property(type, inst(Type, ID), Class));
  error(m("Error with spontaneous instance name.", inst(Type, ID)))
} :-
  sp_st(N, inst(Type, ID)).

1 = {
  sp_st(N, property(person, inst(Type, ID), Name))
    : sp_st(N, property(person, inst(Type, ID), Name));
  sp_st(N, property(person, inst(Type, ID), third));
  error(m("Error with spontaneous instance person.", inst(Type, ID)))
} :-
  sp_st(N, inst(Type, ID)).

1 = {
  sp_st(N, property(number, inst(Type, ID), Name))
    : sp_st(N, property(number, inst(Type, ID), Name));
  sp_st(N, property(number, inst(Type, ID), Number))
    : default_number_for(Class, Number),
      sp_st(N, property(type, inst(Type, ID), Class));
  error(m("Error with spontaneous instance number.", inst(Type, ID)))
} :-
  sp_st(N, inst(Type, ID)).

1 = {
  sp_st(N, property(gender, inst(Type, ID), Name))
    : sp_st(N, property(gender, inst(Type, ID), Name));
  sp_st(N, property(number, inst(Type, ID), Gender))
    : default_gender_for(Class, Gender),
      sp_st(N, property(type, inst(Type, ID), Class));
  error(m("Error with spontaneous instance gender.", inst(Type, ID)))
} :-
  sp_st(N, inst(Type, ID)).

1 = {
  sp_st(N, property(determined, inst(Type, ID), Det)) :
    sp_st(N, property(determined, inst(Type, ID), Det));
  sp_st(N, property(determined, inst(Type, ID), true));
  error(m("Error with spontaneous instance determination.", inst(Type, ID)))
} :-
  sp_st(N, inst(Type, ID)).
