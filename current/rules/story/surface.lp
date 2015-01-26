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

% Default values for name, person, number, gender, and determination:

1 = {
  sp_st(Setup, property(name, inst(Type, ID), Name))
    : sp_st(Setup, property(name, inst(Type, ID), Name));
  sp_st(Setup, property(name, inst(Type, ID), "nameless"))
} :-
  sp_st(Setup, inst(Type, ID)).

1 = {
  sp_st(Setup, property(person, inst(Type, ID), Name))
    : sp_st(Setup, property(person, inst(Type, ID), Name));
  sp_st(Setup, property(person, inst(Type, ID), third))
} :-
  sp_st(Setup, inst(Type, ID)).

1 = {
  sp_st(Setup, property(number, inst(Type, ID), Name))
    : sp_st(Setup, property(number, inst(Type, ID), Name));
  sp_st(Setup, property(number, inst(Type, ID), singular))
} :-
  sp_st(Setup, inst(Type, ID)).

1 = {
  sp_st(Setup, property(gender, inst(Type, ID), Name))
    : sp_st(Setup, property(gender, inst(Type, ID), Name));
  sp_st(Setup, property(gender, inst(Type, ID), neuter))
} :-
  sp_st(Setup, inst(Type, ID)).

1 = {
  sp_st(Setup, property(proper, inst(Type, ID), Det)) :
    sp_st(Setup, property(proper, inst(Type, ID), Det));
  sp_st(Setup, property(proper, inst(Type, ID), false))
} :-
  sp_st(Setup, inst(Type, ID)).
