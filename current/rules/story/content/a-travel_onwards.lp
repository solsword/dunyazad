% travel_onwards: a special party-only action that moves to the next vignette
action(travel_onwards).

argument(travel_onwards, subject, actor).

outcome(travel_onwards, onwards).

% The argument of travel_onwards is fixed:

at(N, arg(Opt, subject, inst(actor, the_party))) :-
  at(N, action(Opt, travel_onwards)).

% travel_onwards as an option removes all existing opportunities (but it
% can't be an option if there are problems):

error(m("Left a problem behind.", N, Prb)) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(problem, Prb)),
  story_op(N, build_options).

at(N, nullifies(X, Opp)) :-
  at(N, action(X, travel_onwards)),
  at(N, potential(opportunity, Opp)),
  node_type(N, choice),
  story_op(N, build_options).

% travelling onwards gets rid of all spontaneous instances and properties
% thereof unless the party has picked up the instance to take with them:

travel_away_from(N, X, inst(Typ, Inst)) :-
  at(N, action(X, travel_onwards)),
  vignette(N, V),
  spontaneous(st(V, inst(Typ, Inst))),
  st(N, inst(Typ, Inst)),
  0 = {
    st(N, relation(has_item, inst(actor, the_party), Inst))
  },
  story_op(N, build_options).

at(N, consequence(X, _not, Inst)) :-
  travel_away_from(N, X, Inst).

at(N, consequence(X, _not, state(State, Inst))) :-
  st(N, state(State, Inst)),
  travel_away_from(N, X, Inst).

at(N, consequence(X, _not, property(Prop, Inst, Value))) :-
  st(N, property(Prop, Inst, Value)),
  travel_away_from(N, X, Inst).

at(N, consequence(X, _not, relation(Rel, Inst, Other))) :-
  st(N, relation(Rel, Inst, Other)),
  travel_away_from(N, X, Inst).


% Surface details
% ---------------

option_text(N, X, @fmt("N#{}/they V#travel/prs/{} onwards.", Subj, Subj)) :-
  at(N, action(X, travel_onwards)),
  at(N, arg(X, subject, inst(T1, Subj))),
  story_op(N, add_surface).

action_text(
  N,
  X,
  @fmt(
    "N#{}/they V#continue/prs/{} N#{}/their journey.",
    Subj, Subj, Subj
  )
) :-
  at(N, action(X, travel_onwards)),
  at(N, arg(X, subject, inst(T1, Subj))),
  story_op(N, add_surface).
