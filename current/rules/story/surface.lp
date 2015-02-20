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
  sp_st(N, property(name, inst(ID), Name))
    : sp_st(N, property(name, inst(ID), Name));
  sp_st(N, property(name, inst(ID), Name))
    : default_name_for(Class, Name),
      sp_st(N, property(type, inst(ID), Class));
  error(m("Error with spontaneous instance name.", inst(ID)))
} :-
  sp_st(N, inst(ID)).

1 = {
  sp_st(N, property(person, inst(ID), Name))
    : sp_st(N, property(person, inst(ID), Name));
  sp_st(N, property(person, inst(ID), third));
  error(m("Error with spontaneous instance person.", inst(ID)))
} :-
  sp_st(N, inst(ID)).

1 = {
  sp_st(N, property(number, inst(ID), Name))
    : sp_st(N, property(number, inst(ID), Name));
  sp_st(N, property(number, inst(ID), Number))
    : default_number_for(Class, Number),
      sp_st(N, property(type, inst(ID), Class));
  error(m("Error with spontaneous instance number.", inst(ID)))
} :-
  sp_st(N, inst(ID)).

1 = {
  sp_st(N, property(gender, inst(ID), Name))
    : sp_st(N, property(gender, inst(ID), Name));
  sp_st(N, property(number, inst(ID), Gender))
    : default_gender_for(Class, Gender),
      sp_st(N, property(type, inst(ID), Class));
  error(m("Error with spontaneous instance gender.", inst(ID)))
} :-
  sp_st(N, inst(ID)).

1 = {
  sp_st(N, property(determined, inst(ID), Det)) :
    sp_st(N, property(determined, inst(ID), Det));
  sp_st(N, property(determined, inst(ID), true));
  error(m("Error with spontaneous instance determination.", inst(ID)))
} :-
  sp_st(N, inst(ID)).
