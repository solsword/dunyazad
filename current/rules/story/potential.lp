% potential.lp
% Rules about potential problems and opportunities.

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
