% shift_blame
action(shift_blame).

% arguments

argument(shift_blame, shifter, actor).
argument(shift_blame, accuser, actor).
argument(shift_blame, old_victim, actor).
argument(shift_blame, new_victim, actor).
initiator(shift_blame, shifter).
default_intent(shift_blame, o(success, shifted)).

% outcomes

outcome_val(shift_blame, success, shifted).
outcome_val(shift_blame, success, suspected).
outcome_val(shift_blame, success, ignored).

% skills:

skill_link(
  elocution, contest, no_tool,
  shift_blame,
  between(shifter, new_victim),
  either(o(success, shifted), o(success, suspected))
).

% Shifting the blame to yourself is always easy:

at(N, likely_outcome(X, o(success, shifted))) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, accuser, Accuser)),
  at(N, arg(X, shifter, SamePerson)),
  at(N, arg(X, new_victim, SamePerson)).

at(N, relevant_factor(X, SamePerson, shift_blame_via_confession, SamePerson)) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, accuser, Accuser)),
  at(N, arg(X, shifter, SamePerson)),
  at(N, arg(X, new_victim, SamePerson)).

% constraints:

error(m("Unintelligent blame.", N, X)) :-
  at(N, action(X, shfit_blame)),
  at(N, arg(X, Any, ShouldBeSmart)),
  st(N, property(has_skill, ShouldBeSmart, unintelligent)),
  story_op(N, build_options).

reflexive(shift_blame). % see custom role overlap constraints below

error(m("Shifter is also accuser.", N, X)) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, shifter, SamePerson)),
  at(N, arg(X, accuser, SamePerson)).

error(m("Accuser is the old accused.", N, X)) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, accuser, SamePerson)),
  at(N, arg(X, old_victim, SamePerson)).

error(m("Accuser is the new accused.", N, X)) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, accuser, SamePerson)),
  at(N, arg(X, new_victim, SamePerson)).

error(m("Old and new victims are the same.", N, X)) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, old_victim, SamePerson)),
  at(N, arg(X, new_victim, SamePerson)).

error(m("Tried to shift non-existent blame.", N, X)) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, accuser, Accuser)),
  at(N, arg(X, old_victim, OldVictim)),
  not st(N, relation(accusing, Accuser, OldVictim)).

% shifting blame if successful changes an "accusing" relation

at(
  N,
  consequence_of(
    X,
    o(success, shifted),
    resolves,
    potential(problem, relation(accusing, Accuser, OldVictim))
  )
) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, accuser, Accuser)),
  at(N, arg(X, old_victim, OldVictim)),
  at(N, potential(problem, relation(accusing, Accuser, OldVictim))),
  story_op(N, build_options).

% The accusation just spreads rather than shifting if you fail...
%at(
%  N,
%  consequence_of(
%    X,
%    o(success, suspected),
%    resolves,
%    potential(problem, relation(accusing, Accuser, OldVictim))
%  )
%) :-
%  at(N, action(X, shift_blame)),
%  at(N, arg(X, shifter, Shifter)),
%  at(N, arg(X, accuser, Accuser)),
%  at(N, arg(X, old_victim, OldVictim)),
%  at(N, potential(problem, relation(accusing, Accuser, OldVictim))),
%  Shifter != OldVictim,
%  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(success, shifted),
    relation(accusing, Accuser, NewVictim)
  )
) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, accuser, Accuser)),
  at(N, arg(X, new_victim, NewVictim)),
  story_op(N, build_options).

at(
  N,
  consequence_of(
    X,
    o(success, suspected),
    relation(accusing, Accuser, Shifter)
  )
) :-
  at(N, action(X, shift_blame)),
  at(N, arg(X, shifter, Shifter)),
  at(N, arg(X, accuser, Accuser)),
  story_op(N, build_options).
