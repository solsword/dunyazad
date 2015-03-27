% travel_onwards: a special party-only action that moves to the next vignette
action(travel_onwards).

% arguments

argument(travel_onwards, subject, actor).
initiator(travel_onwards, subject).
injured_can_initiate(travel_onwards).

% outcomes

outcome_val(travel_onwards, onwards, onwards).

% no skills

% The argument of travel_onwards is fixed:

at(N, arg(Opt, subject, inst(actor, you))) :-
  at(N, action(Opt, travel_onwards)).

% travel_onwards as an option removes all problems and opportunities related to
% things left behind, but it can't be an option if doing so would remove a
% non-hidden problem.

removed_by(N, X, potential(PType, state(PState, Subject))) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(PType, state(PState, Subject))),
  travel_away_from(N, X, Subject),
  story_op(N, build_options).

removed_by(N, X, potential(PType, property(PProp, Subject, PVal))) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(PType, property(PProp, Subject, PVal))),
  travel_away_from(N, X, Subject),
  story_op(N, build_options).

removed_by(N, X, potential(PType, relation(PRel, Subject, Object))) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(PType, relation(PRel, Subject, Object))),
  travel_away_from(N, X, Subject),
  story_op(N, build_options).

removed_by(N, X, potential(PType, relation(PRel, Subject, Object))) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(PType, relation(PRel, Subject, Object))),
  travel_away_from(N, X, Object),
  story_op(N, build_options).

error(m("Left a non-hidden problem behind.", N, Prb)) :-
  at(N, action(X, travel_onwards)),
  removed_by(N, X, potential(problem, Prb)),
  not at(N, hidden(potential(problem, Prb))),
  1 <= {
    st(N, state(party_member, Member))
      : at(N, problematic_for(potential(problem, Prb), Member))
  },
  story_op(N, build_options).

at(N, consequence_of(X, o(onwards, onwards), _not, Pot)) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(PType, Pot)),
  removed_by(N, X, potential(PType, Pot)),
  story_op(N, build_options).

% travelling onwards gets rid of all spontaneous instances and properties
% thereof unless the party has picked up the instance to take with them:

travel_away_from(N, X, inst(Type, Inst)) :-
  at(N, action(X, travel_onwards)),
  vignette(N, V),
  spontaneous(st(V, inst(Type, Inst))),
  st(N, inst(Type, Inst)),
  not st(N, state(party_member, inst(Type, Inst))),
  0 = {
    st(N, relation(has_item, PartyMember, Inst)) :
      st(N, state(party_member, PartyMember))
  },
  story_op(N, build_options).

at(N, consequence_of(X, o(onwards, onwards), _not, Inst)) :-
  travel_away_from(N, X, Inst).

at(N, consequence_of(X, o(onwards, onwards), _not, state(State, Inst))) :-
  st(N, state(State, Inst)),
  travel_away_from(N, X, Inst).

at(
  N,
  consequence_of(X, o(onwards, onwards), _not, property(Prop, Inst, Value))
) :-
  st(N, property(Prop, Inst, Value)),
  travel_away_from(N, X, Inst).

at(
  N,
  consequence_of(X, o(onwards, onwards), _not, relation(Rel, Inst, Other))
) :-
  st(N, relation(Rel, Inst, Other)),
  travel_away_from(N, X, Inst).

at(
  N,
  consequence_of(X, o(onwards, onwards), _not, relation(Rel, Other, Inst))
) :-
  st(N, relation(Rel, Other, Inst)),
  travel_away_from(N, X, Inst).
