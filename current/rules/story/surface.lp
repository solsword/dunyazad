% surface.lp
% Rules about surface text realization

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
