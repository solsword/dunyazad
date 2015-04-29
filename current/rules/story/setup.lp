% setup.lp
% Rules concerning vignette setup.

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

% normal arguments
setup_argument_n(Setup, Arg, Class, 1, 1) :-
  setup_argument(Setup, Arg, Class).

1 >= {
  error(m("Argument binding failed.", N, Setup, Arg))
} :-
  setup(N, Setup),
  setup_argument_n(Setup, Arg, Class, Min, Max).

Min <= {
  at(N, setup_arg(Arg, Inst)) : is_instance(N, Inst, Class)
} <= Max :-
  setup(N, Setup),
  setup_argument_n(Setup, Arg, Class, Min, Max),
  not error(m("Argument binding failed.", N, Setup, Arg)),
  story_op(N, initialize_node).

% create arguments

setup_argument_create_n(Setup, Arg, Class, 1, 1) :-
  setup_argument_create(Setup, Arg, Class).

1 >= {
  error(m("Creation argument binding failed.", N, Setup, Arg))
} :-
  setup(N, Setup),
  setup_argument_create_n(Setup, Arg, Class, Min, Max).

Min <= {
  get_unique_key(s(N, sargcreate(Setup, Arg, Class), I))
    : setup_arg_id(I),
      I <= Max
} <= Max :-
  setup(N, Setup),
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  not error(m("Creation argument binding failed.", N, Setup, Arg)),
  story_op(N, initialize_node).

at(N, setup_arg(Arg, inst(Type, @inst(Class, K)))) :-
  unique_key(s(N, sargcreate(Setup, Arg, Class), I), K),
  category_for(Class, Type),
  setup(N, Setup),
  story_op(N, initialize_node).

sp_st(N, Inst) :-
  setup(N, Setup),
  at(N, setup_arg(Arg, Inst)),
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  story_op(N, initialize_node).

1 = {
  sp_inst_class(N, Inst, Concrete)
    : concrete_class_of(Class, Concrete);
  error(m("Error with spontaneous instance concrete class.", N, Inst))
} :-
  setup(N, Setup),
  at(N, setup_arg(Arg, Inst)),
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  story_op(N, initialize_node).

sp_st(N, property(type, Inst, Class)) :-
  sp_inst_class(N, Inst, Class).

% Add class skills:
0 <= {
  sp_st(N, property(has_skill, Inst, Skill))
} <= 1 :-
  sp_inst_class(N, Inst, Class),
  is_instance(N, Inst, Superclass),
  class_skill(Superclass, Skill, sometimes).

sp_st(N, property(has_skill, Inst, Skill)) :-
  sp_inst_class(N, Inst, Class),
  is_instance(N, Inst, Superclass),
  class_skill(Superclass, Skill, always).

% Create class items

1 >= {
  error(m("Creation side-effect binding failed.", N, Setup, Arg))
} :-
  setup(N, Setup),
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  at(N, setup_arg(Arg, Inst)),
  is_instance(N, Inst, Someclass),
  class_item(Someclass, IClass, Min, Max).

Min <= {
  get_unique_key(s(N, createclassitem(Owner, CIClass), I))
    : setup_arg_id(I),
      concrete_class_of(IClass, CIClass),
      I <= Max
} <= Max :-
  setup(N, Setup),
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  not error(m("Creation side-effect binding failed.", N, Setup, Arg)),
  at(N, setup_arg(Arg, Owner)),
  is_instance(N, Owner, Someclass),
  class_item(Someclass, IClass, Min, Max),
  story_op(N, initialize_node).

sp_st(N, inst(Type, @inst(CIClass, K))) :-
  unique_key(s(N, createclassitem(Owner, CIClass), I), K),
  category_for(CIClass, Type),
  story_op(N, initialize_node).

sp_st(N, relation(has_item, Owner, inst(Type, @inst(CIClass, K)))) :-
  unique_key(s(N, createclassitem(Owner, CIClass), I), K),
  category_for(CIClass, Type),
  story_op(N, initialize_node).

sp_st(N, property(type, inst(Type, @inst(CIClass, K)), CIClass)) :-
  unique_key(s(N, createclassitem(Owner, CIClass), I), K),
  category_for(CIClass, Type),
  story_op(N, initialize_node).

% Rules for argument substitution:
% --------------------------------

s_st_n(Setup, State, 1) :-
  s_st(Setup, State).

s_st_n(Setup, State, 0) :-
  s_o_st(Setup, State).

% States
L <= { sp_st(N, state(State, Const)) } <= 1 :-
  s_st_n(Setup, state(c(State), c(Const)), L),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, state(State, Val)) } <= 1 :-
  s_st_n(Setup, state(c(State), v(Arg)), L),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, state(SVal, Const)) } <= 1 :-
  s_st_n(Setup, state(v(SArg), c(Const)), L),
  at(N, setup_arg(SArg, SVal)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, state(SVal, Val)) } <= 1 :-
  s_st_n(Setup, state(v(SArg), v(Arg)), L),
  at(N, setup_arg(Arg, Val)),
  at(N, setup_arg(SArg, SVal)),
  setup(N, Setup),
  story_op(N, initialize_node).

% Properties
L <= { sp_st(N, property(Prop, C1, C2)) } <= 1 :-
  s_st_n(Setup, property(c(Prop), c(C1), c(C2)), L),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, property(Prop, Val, Const)) } <= 1 :-
  s_st_n(Setup, property(c(Prop), v(Arg), c(Const)), L),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, property(Prop, Const, Val)) } <= 1 :-
  s_st_n(Setup, property(c(Prop), c(Const), v(Arg)), L),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, property(Prop, V1, V2)) } <= 1 :-
  s_st_n(Setup, property(c(Prop), v(A1), v(A2)), L),
  at(N, setup_arg(A1, V1)),
  at(N, setup_arg(A2, V2)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, property(PVal, C1, C2)) } <= 1 :-
  s_st_n(Setup, property(v(PArg), c(C1), c(C2)), L),
  at(N, setup_arg(PArg, PVal)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, property(PVal, Val, Const)) } <= 1 :-
  s_st_n(Setup, property(v(PArg), v(Arg), c(Const)), L),
  at(N, setup_arg(PArg, PVal)),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, property(PVal, Const, Val)) } <= 1 :-
  s_st_n(Setup, property(v(PArg), c(Const), v(Arg)), L),
  at(N, setup_arg(PArg, PVal)),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, property(PVal, V1, V2)) } <= 1 :-
  s_st_n(Setup, property(v(PArg), v(A1), v(A2)), L),
  at(N, setup_arg(PArg, PVal)),
  at(N, setup_arg(A1, V1)),
  at(N, setup_arg(A2, V2)),
  setup(N, Setup),
  story_op(N, initialize_node).

% Relations
L <= { sp_st(N, relation(Rel, C1, C2)) } <= 1 :-
  s_st_n(Setup, relation(c(Rel), c(C1), c(C2)), L),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, relation(Rel, Val, Const)) } <= 1 :-
  s_st_n(Setup, relation(c(Rel), v(Arg), c(Const)), L),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, relation(Rel, Const, Val)) } <= 1 :-
  s_st_n(Setup, relation(c(Rel), c(Const), v(Arg)), L),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, relation(Rel, V1, V2)) } <= 1 :-
  s_st_n(Setup, relation(c(Rel), v(A1), v(A2)), L),
  at(N, setup_arg(A1, V1)),
  at(N, setup_arg(A2, V2)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, relation(RVal, C1, C2)) } <= 1 :-
  s_st_n(Setup, relation(v(RArg), c(C1), c(C2)), L),
  at(N, setup_arg(RArg, RVal)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, relation(RVal, Val, Const)) } <= 1 :-
  s_st_n(Setup, relation(v(RArg), v(Arg), c(Const)), L),
  at(N, setup_arg(RArg, RVal)),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, relation(RVal, Const, Val)) } <= 1 :-
  s_st_n(Setup, relation(v(RArg), c(Const), v(Arg)), L),
  at(N, setup_arg(RArg, RVal)),
  at(N, setup_arg(Arg, Val)),
  setup(N, Setup),
  story_op(N, initialize_node).

L <= { sp_st(N, relation(RVal, V1, V2)) } <= 1 :-
  s_st_n(Setup, relation(v(RArg), v(A1), v(A2)), L),
  at(N, setup_arg(RArg, RVal)),
  at(N, setup_arg(A1, V1)),
  at(N, setup_arg(A2, V2)),
  setup(N, Setup),
  story_op(N, initialize_node).

% Error checking:
error(m("Negative argument arity.", Setup, Arg)) :-
  setup_argument_n(Setup, Arg, Class, Min, Max),
  Min < 0.

error(m("Negative argument arity.", Setup, Arg)) :-
  setup_argument_n(Setup, Arg, Class, Min, Max),
  Max < 0.

error(m("Invalid argument arity.", Setup, Arg)) :-
  setup_argument_n(Setup, Arg, Class, Min, Max),
  Min > Max.

error(m("Excessive argument arity.", Setup, Arg)) :-
  setup_argument_n(Setup, Arg, Class, Min, Max),
  Min > max_setup_argument_arity.

error(m("Excessive argument arity.", Setup, Arg)) :-
  setup_argument_n(Setup, Arg, Class, Min, Max),
  Max > max_setup_argument_arity.

error(m("Negative argument arity.", Setup, Arg)) :-
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  Min < 0.

error(m("Negative argument arity.", Setup, Arg)) :-
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  Max < 0.

error(m("Invalid argument arity.", Setup, Arg)) :-
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  Min > Max.

error(m("Excessive argument arity.", Setup, Arg)) :-
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  Min > max_setup_argument_arity.

error(m("Excessive argument arity.", Setup, Arg)) :-
  setup_argument_create_n(Setup, Arg, Class, Min, Max),
  Max > max_setup_argument_arity.
