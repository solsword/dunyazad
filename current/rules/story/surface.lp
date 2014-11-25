% surface.lp
% Rules about surface text realization

% Default values for name, person, number, gender, and determination:

1 = {
  s_st(Setup, property(name, inst(Type, ID), Name))
    : s_st(Setup, property(name, inst(Type, ID), Name));
  s_st(Setup, property(name, inst(Type, ID), "nameless"))
} :-
  s_st(Setup, inst(Type, ID)).

1 = {
  s_st(Setup, property(person, inst(Type, ID), Name))
    : s_st(Setup, property(person, inst(Type, ID), Name));
  s_st(Setup, property(person, inst(Type, ID), third))
} :-
  s_st(Setup, inst(Type, ID)).

1 = {
  s_st(Setup, property(number, inst(Type, ID), Name))
    : s_st(Setup, property(number, inst(Type, ID), Name));
  s_st(Setup, property(number, inst(Type, ID), singular))
} :-
  s_st(Setup, inst(Type, ID)).

1 = {
  s_st(Setup, property(gender, inst(Type, ID), Name))
    : s_st(Setup, property(gender, inst(Type, ID), Name));
  s_st(Setup, property(gender, inst(Type, ID), neuter))
} :-
  s_st(Setup, inst(Type, ID)).

1 = {
  s_st(Setup, property(proper, inst(Type, ID), Det)) :
    s_st(Setup, property(proper, inst(Type, ID), Det));
  s_st(Setup, property(proper, inst(Type, ID), true))
} :-
  s_st(Setup, inst(Type, ID)).

% Problem/opportunity description:
% TODO: content files for problems/opportunities?
potential_text(
  N,
  @fmt(
    "N#{}/they V#be/prs/{} V#threaten/prp/{} N#{}/them",
    Threatening, Threatening, Threatening, Threatened
  )
) :-
  st(
    N,
    potential(
      problem,
      threatening(inst(T1, Threatening), inst(T2, Threatened))
    )
  ),
  story_op(N, add_surface).
