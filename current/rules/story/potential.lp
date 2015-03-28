% potential.lp
% Rules about potential problems and opportunities.

% TODO: How to indicate resolution success?!

% Problematic/opportune states:
% -----------------------------

at(N, potential(PTyp, state(PState, Inst))) :-
  potential(PTyp, state, PState),
  st(N, state(PState, Inst)),
  story_op(N, build_options).

at(N, potential(PTyp, property(PProp, Inst, PVal))) :-
  potential(PTyp, property, PProp, PVal),
  st(N, property(PProp, Inst, PVal)),
  story_op(N, build_options).

at(N, potential(PTyp, relation(PRel, I1, I2))) :-
  potential(PTyp, relation, PRel),
  st(N, relation(PRel, I1, I2)),
  story_op(N, build_options).

% certain parties are the initiators of and/or are troubled by different
% potentials.

at(N, initiated_by(potential(PTyp, state(PState, Inst)), Inst)) :-
  at(N, potential(PTyp, state(PState, Inst))),
  initiated_by(PTyp, state, PState, inst).

at(N, initiated_by(potential(PTyp, property(PProp, Inst, PVal)), Inst)) :-
  at(N, potential(PTyp, property(PProp, Inst, PVal))),
  initiated_by(PTyp, property, PProp, PVal, inst).

at(N, initiated_by(potential(PTyp, relation(PRel, From, To)), From)) :-
  at(N, potential(PTyp, relation(PRel, From, To))),
  initiated_by(PTyp, relation, PRel, from).

at(N, initiated_by(potential(PTyp, relation(PRel, From, To)), To)) :-
  at(N, potential(PTyp, relation(PRel, From, To))),
  initiated_by(PTyp, relation, PRel, to).

at(N, problematic_for(potential(PTyp, state(PState, Inst)), Inst)) :-
  at(N, potential(PTyp, state(PState, Inst))),
  problematic_for(PTyp, state, PState, inst).

at(N, problematic_for(potential(PTyp, property(PProp, Inst, PVal)), Inst)) :-
  at(N, potential(PTyp, property(PProp, Inst, PVal))),
  problematic_for(PTyp, property, PProp, PVal, inst).

at(N, problematic_for(potential(PTyp, relation(PRel, From, To)), From)) :-
  at(N, potential(PTyp, relation(PRel, From, To))),
  problematic_for(PTyp, relation, PRel, from).

at(N, problematic_for(potential(PTyp, relation(PRel, From, To)), To)) :-
  at(N, potential(PTyp, relation(PRel, From, To))),
  problematic_for(PTyp, relation, PRel, to).

% Potentials have categories, including "urgent" and "immediate":
at(N, category(potential(PTyp, state(PState, Inst)), Category)) :-
  at(N, potential(PTyp, state(PState, Inst))),
  pcategory(potential(PTyp, state, PState), Category),
  story_op(N, build_options).

at(N, category(potential(PTyp, property(PProp, Inst, PVal)), Category)) :-
  at(N, potential(PTyp, property(PProp, Inst, PVal))),
  pcategory(potential(PTyp, property, PProp, PVal), Category),
  story_op(N, build_options).

at(N, category(potential(PTyp, relation(PRel, I1, I2)), Category)) :-
  at(N, potential(PTyp, relation(PRel, I1, I2))),
  pcategory(potential(PTyp, relation, PRel), Category),
  story_op(N, build_options).

% From immediate problems to mere opportunities some potentials are more
% important than others.
at(N, most_important(Pt)) :-
  at(N, importance(I, Pt)),
  0 = {
    at(N, some_importance(O)) : O < I
  }.

at(N, some_importance(I)) :-
  at(N, importance(I, Something)).

at(N, importance(1, potential(problem, Pt))) :-
  at(N, category(potential(problem, Pt), urgent)),
  at(N, category(potential(problem, Pt), immediate)),
  story_op(N, build_options).

at(N, importance(2, potential(problem, Pt))) :-
  at(N, category(potential(problem, Pt), immediate)),
  story_op(N, build_options).

at(N, importance(3, potential(problem, Pt))) :-
  at(N, category(potential(problem, Pt), urgent)),
  story_op(N, build_options).

at(N, importance(4, potential(problem, Pt))) :-
  at(N, potential(problem, Pt)),
  story_op(N, build_options).

at(N, importance(5, potential(opportunity, Pt))) :-
  at(N, category(potential(opportunity, Pt), urgent)),
  at(N, category(potential(opportunity, Pt), immediate)),
  story_op(N, build_options).

at(N, importance(6, potential(opportunity, Pt))) :-
  at(N, category(potential(opportunity, Pt), immediate)),
  story_op(N, build_options).

at(N, importance(7, potential(opportunity, Pt))) :-
  at(N, category(potential(opportunity, Pt), urgent)),
  story_op(N, build_options).

at(N, importance(8, potential(opportunity, Pt))) :-
  at(N, potential(opportunity, Pt)),
  story_op(N, build_options).

% The idea of "unresolved" potential:
unresolved_potential(N, option(O), potential(PTyp, Pt)) :-
  at(N, potential(PTyp, Pt)),
  at(N, option(O)),
  0 = { at(N, consequence(option(O), _not, Pt)) },
  story_op(N, build_options).

unresolved_potential(N, Opt, potential(PTyp, state(PState, Inst))) :-
  at(N, consequence(Opt, state(PState, Inst))),
  potential(PTyp, state, PState),
  story_op(N, build_options).

unresolved_potential(N, Opt, potential(PTyp, property(PProp, Inst, PVal))) :-
  at(N, consequence(Opt, property(PProp, Inst, PVal))),
  potential(PTyp, property, PProp, PVal),
  story_op(N, build_options).

unresolved_potential(N, Opt, potential(PTyp, relation(PRel, I1, I2))) :-
  at(N, consequence(Opt, relation(PRel, I1, I2))),
  potential(PTyp, relation, PRel),
  story_op(N, build_options).

% Potential can be hidden if something involved is off-stage:
% TODO: Hide base states/properties/relations instead/as well?
at(N, hidden(potential(PTyp, state, PState))) :-
  potential(PTyp, state, PState),
  st(N, state(PState, Inst)),
  st(N, state(off_stage, Inst)),
  story_op(N, build_options).

at(N, hidden(potential(PTyp, property(PProp, Inst, PVal)))) :-
  potential(PTyp, property, PProp, PVal),
  st(N, property(PProp, Inst, PVal)),
  st(N, state(off_stage, Inst)),
  story_op(N, build_options).

at(N, hidden(potential(PTyp, relation(PRel, I1, I2)))) :-
  potential(PTyp, relation, PRel),
  st(N, relation(PRel, I1, I2)),
  st(N, state(off_stage, I1)),
  story_op(N, build_options).

at(N, hidden(potential(PTyp, relation(PRel, I1, I2)))) :-
  potential(PTyp, relation, PRel),
  st(N, relation(PRel, I1, I2)),
  st(N, state(off_stage, I2)),
  story_op(N, build_options).
