% setup.lp
% Rules concerning vignette setup.

% Local instance counting:
licount(inst(Typ, I), N, X) :-
  setup(N, Setup),
  setup_argument_c(Setup, AnyArg, inst(Typ, I)),
  X = {
    setup(Prev, OSet)
      : setup_argument_c(OSet, SomeArg, inst(Typ, I)),
      before(Prev, N)
  }.

% Rules for unpacking and labelling spontaneous states:
% -----------------------------------------------------

st(N, State) :-
  sp_st(N, State),
  story_op(N, initialize_node).

spontaneous(st(N, State)) :-
  sp_st(N, State),
  story_op(N, initialize_node).

% Rules for setup arguments:
% --------------------------

1 = {
  at(N, setup_arg(Arg, inst(Type, Inst))) : st(N, inst(Type, Inst))
} :-
  setup(N, Setup),
  setup_argument(Setup, Arg, Type),
  story_op(N, initialize_node).

1 = {
  at(N, setup_arg(Arg, Symbol)) : symbol(SymType, Symbol)
} :-
  setup(N, Setup),
  setup_argument_s(Setup, Arg, SymType),
  story_op(N, initialize_node).

at(N, setup_arg(Arg, inst(Typ, @inst(I, LIC+1)))) :-
  setup(N, Setup),
  licount(inst(Typ, I), N, LIC),
  setup_argument_c(Setup, Arg, inst(Type, I)),
  story_op(N, initialize_node).

sp_st(N, inst(Typ, @inst(I, LIC+1))) :-
  setup(N, Setup),
  licount(inst(Typ, I), N, LIC),
  setup_argument_c(Setup, Arg, inst(Type, I)),
  story_op(N, initialize_node).

% TODO: Do we need this?
%sp_st(N, state(local_instance, inst(Typ, @inst(I, LIC+1)))) :-
%  licount(inst(Typ, I), N, LIC),
%  setup_argument_c(Setup, Arg, inst(Type, I)),
%  setup(N, Setup),
%  story_op(N, initialize_node).


% Rules for argument substitution:
% --------------------------------

% States
sp_st(N, state(State, Const)) :-
  s_st(Setup, state(c(State), c(Const))),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, state(State, Val)) :-
  s_st(Setup, state(c(State), v(Arg))),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, state(SVal, Const)) :-
  s_st(Setup, state(v(SArg), c(Const))),
  at(N, setup_arg(SArg, SVal)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, state(SVal, Val)) :-
  s_st(Setup, state(v(SArg), v(Arg))),
  at(N, setup_arg(Arg, Val)),
  at(N, setup_arg(SArg, SVal)),
  setup(N, Setup),
  story_op(N, initialize_node).

% Properties
sp_st(N, property(Prop, C1, C2)) :-
  s_st(Setup, property(c(Prop), c(C1), c(C2))),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, property(Prop, Val, Const)) :-
  s_st(Setup, property(c(Prop), v(Arg), c(Const))),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, property(Prop, Const, Val)) :-
  s_st(Setup, property(c(Prop), c(Const), v(Arg))),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, property(Prop, V1, V2)) :-
  s_st(Setup, property(c(Prop), v(A1), v(A2))),
  at(N, setup_arg(A1, V1)),
  at(N, setup_arg(A2, V2)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, property(PVal, C1, C2)) :-
  s_st(Setup, property(v(PArg), c(C1), c(C2))),
  at(N, setup_arg(PArg, PVal)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, property(PVal, Val, Const)) :-
  s_st(Setup, property(v(PArg), v(Arg), c(Const))),
  at(N, setup_arg(PArg, PVal)),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, property(PVal, Const, Val)) :-
  s_st(Setup, property(v(PArg), c(Const), v(Arg))),
  at(N, setup_arg(PArg, PVal)),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, property(PVal, V1, V2)) :-
  s_st(Setup, property(v(PArg), v(A1), v(A2))),
  at(N, setup_arg(PArg, PVal)),
  at(N, setup_arg(A1, V1)),
  at(N, setup_arg(A2, V2)),
  setup(N, Setup),
  story_op(N, initialize_node).

% Relations
sp_st(N, relation(Rel, C1, C2)) :-
  s_st(Setup, relation(c(Rel), c(C1), c(C2))),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, relation(Rel, Val, Const)) :-
  s_st(Setup, relation(c(Rel), v(Arg), c(Const))),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, relation(Rel, Const, Val)) :-
  s_st(Setup, relation(c(Rel), c(Const), v(Arg))),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, relation(Rel, V1, V2)) :-
  s_st(Setup, relation(c(Rel), v(A1), v(A2))),
  at(N, setup_arg(A1, V1)),
  at(N, setup_arg(A2, V2)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, relation(RVal, C1, C2)) :-
  s_st(Setup, relation(v(RArg), c(C1), c(C2))),
  at(N, setup_arg(RArg, RVal)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, relation(RVal, Val, Const)) :-
  s_st(Setup, relation(v(RArg), v(Arg), c(Const))),
  at(N, setup_arg(RArg, RVal)),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, relation(RVal, Const, Val)) :-
  s_st(Setup, relation(v(RArg), c(Const), v(Arg))),
  at(N, setup_arg(RArg, RVal)),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, relation(RVal, V1, V2)) :-
  s_st(Setup, relation(v(RArg), v(A1), v(A2))),
  at(N, setup_arg(RArg, RVal)),
  at(N, setup_arg(A1, V1)),
  at(N, setup_arg(A2, V2)),
  setup(N, Setup),
  story_op(N, initialize_node).
