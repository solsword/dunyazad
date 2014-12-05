% surface.lp
% Rules about surface text realization

% Default values for name, person, number, gender, and determination:

1 = {
  s_st(Setup, property(name, linst(Type, ID), Name))
    : s_st(Setup, property(name, linst(Type, ID), Name));
  s_st(Setup, property(name, linst(Type, ID), "nameless"))
} :-
  s_st(Setup, linst(Type, ID)).

1 = {
  s_st(Setup, property(person, linst(Type, ID), Name))
    : s_st(Setup, property(person, linst(Type, ID), Name));
  s_st(Setup, property(person, linst(Type, ID), third))
} :-
  s_st(Setup, linst(Type, ID)).

1 = {
  s_st(Setup, property(number, linst(Type, ID), Name))
    : s_st(Setup, property(number, linst(Type, ID), Name));
  s_st(Setup, property(number, linst(Type, ID), singular))
} :-
  s_st(Setup, linst(Type, ID)).

1 = {
  s_st(Setup, property(gender, linst(Type, ID), Name))
    : s_st(Setup, property(gender, linst(Type, ID), Name));
  s_st(Setup, property(gender, linst(Type, ID), neuter))
} :-
  s_st(Setup, linst(Type, ID)).

1 = {
  s_st(Setup, property(proper, linst(Type, ID), Det)) :
    s_st(Setup, property(proper, linst(Type, ID), Det));
  s_st(Setup, property(proper, linst(Type, ID), true))
} :-
  s_st(Setup, linst(Type, ID)).
