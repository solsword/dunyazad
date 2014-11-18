% surface.lp
% Rules about surface text realization

% Default values for name, person, number, gender, and determination:

1 = {
  st(Node, property(name, inst(Type, ID), Name))
    : st(Node, property(name, inst(Type, ID), Name));
  st(Node, property(name, inst(Type, ID), "nameless"))
} :-
  st(Node, inst(Type, ID)).

1 = {
  st(Node, property(person, inst(Type, ID), Name))
    : st(Node, property(person, inst(Type, ID), Name));
  st(Node, property(person, inst(Type, ID), third))
} :-
  st(Node, inst(Type, ID)).

1 = {
  st(Node, property(number, inst(Type, ID), Name))
    : st(Node, property(number, inst(Type, ID), Name));
  st(Node, property(number, inst(Type, ID), singular))
} :-
  st(Node, inst(Type, ID)).

1 = {
  st(Node, property(gender, inst(Type, ID), Name))
    : st(Node, property(gender, inst(Type, ID), Name));
  st(Node, property(gender, inst(Type, ID), neuter))
} :-
  st(Node, inst(Type, ID)).

1 = {
  st(Node, property(proper, inst(Type, ID), Det)) :
    st(Node, property(proper, inst(Type, ID), Det));
  st(Node, property(proper, inst(Type, ID), true))
} :-
  st(Node, inst(Type, ID)).

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
  ).
