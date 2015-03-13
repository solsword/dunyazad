% travel_onwards: a special party-only action that moves to the next vignette
action(travel_onwards).

% arguments

argument(travel_onwards, subject, actor).
initiator(travel_onwards, subject).

% outcomes

outcome_val(travel_onwards, onwards, onwards).

% no skills

% The argument of travel_onwards is fixed:

at(N, arg(Opt, subject, inst(actor, you))) :-
  at(N, action(Opt, travel_onwards)).

% travel_onwards as an option removes all existing opportunities (but it
% can't be an option if there are problems):

error(m("Left a problem behind.", N, Prb)) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(problem, Prb)),
  story_op(N, build_options).

at(N, consequence_of(X, o(onwards, onwards), _not, Opp)) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(opportunity, Opp)),
  node_type(N, choice),
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
