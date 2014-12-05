% setup.lp
% Rules concerning vignette setup.

% Local instance counting:
licount(linst(Typ, I), N, X) :-
  setup(N, Setup),
  s_st(Setup, linst(Typ, I)),
  X = {
    setup(Prev, OSet) : s_st(OSet, linst(Typ, I)), before(Prev, N)
  }.

% Rules for unpacking setups:
% ---------------------------

st(N, State) :-
  ls_st(N, State),
  story_op(N, initialize_node).

spontaneous(st(N, State)) :-
  ls_st(N, State),
  story_op(N, initialize_node).

% Instances
ls_st(N, inst(Typ, @inst(I, LIC+1))) :-
  licount(linst(Typ, I), N, LIC),
  s_st(Setup, linst(Typ, I)),
  setup(N, Setup),
  story_op(N, initialize_node).

ls_st(N, state(local_instance, inst(Typ, @inst(I, LIC+1)))) :-
  licount(linst(Typ, I), N, LIC),
  s_st(Setup, linst(Typ, I)),
  setup(N, Setup),
  story_op(N, initialize_node).

% States
ls_st(N, state(State, inst(Typ, I))) :-
  s_st(Setup, state(State, inst(Typ, I))),
  setup(N, Setup),
  story_op(N, initialize_node).

ls_st(N, state(State, inst(Typ, @inst(I, LIC+1)))) :-
  licount(linst(Typ, I), N, LIC),
  s_st(Setup, state(State, linst(Typ, I))),
  setup(N, Setup),
  story_op(N, initialize_node).

% Properties
ls_st(N, property(Prop, inst(Typ, I), Value)) :-
  s_st(Setup, property(Prop, inst(Typ, I), Value)),
  setup(N, Setup),
  story_op(N, initialize_node).

ls_st(N, property(Prop, inst(Typ, @inst(I, LIC+1)), Value)) :-
  licount(linst(Typ, I), N, LIC),
  s_st(Setup, property(Prop, linst(Typ, I), Value)),
  setup(N, Setup),
  story_op(N, initialize_node).

% Relations
ls_st(N, relation(Rel, inst(T1, I1), inst(T2, I2))) :-
  s_st(Setup, relation(Rel, inst(T1, I1), inst(T2, I2))),
  setup(N, Setup),
  story_op(N, initialize_node).

ls_st(N, relation(Rel, inst(LT, @inst(LI, LIC+1)), inst(T, I))) :-
  licount(linst(LT, LI), N, LIC),
  s_st(Setup, relation(Rel, linst(LT, LI), inst(T, I))),
  setup(N, Setup),
  story_op(N, initialize_node).

ls_st(N, relation(Rel, inst(T, I), inst(LT, @inst(LI, LIC+1)))) :-
  licount(linst(LT, LI), N, LIC),
  s_st(Setup, relation(Rel, inst(T, I), linst(LT, LI))),
  setup(N, Setup),
  story_op(N, initialize_node).

ls_st(
  N,
  relation(
    Rel,
    inst(LT1, @inst(LI1, LIC1+1)),
    inst(LT2, @inst(LI2, LIC2+1))
  )
) :-
  licount(linst(LT1, LI1), N, LIC1),
  licount(linst(LT2, LI2), N, LIC2),
  s_st(Setup, relation(Rel, linst(LT1, LI1), linst(LT2, LI2))),
  setup(N, Setup),
  story_op(N, initialize_node).
