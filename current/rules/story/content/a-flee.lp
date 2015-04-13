% flee
action(flee).

chaotic(flee).

% arguments

argument(flee, fearful, actor).
argument(flee, from, actor).
initiator(flee, fearful).
injured_can_initiate(flee).
default_intent(flee, o(success, escape)).

% outcomes

outcome_val(flee, success, escape).
outcome_val(flee, success, failure).

outcome_val(flee, get_injured, injured).
outcome_val(flee, get_injured, safe).

% skills

at(
  N,
  skill_link(
    acrobatics, contest, no_tool,
    flee,
    between(fearful, from),
    either(o(success, escape), o(success, failure))
  )
) :-
  1 <= {
    setting(N, city);
    setting(N, town)
  },
  story_node(N).

at(
  N,
  skill_link(
    wilderness_lore, contest, no_tool,
    flee,
    between(fearful, from),
    either(o(success, escape), o(success, failure))
  )
) :-
  1 <= {
    setting(N, road);
    setting(N, wilderness)
  },
  story_node(N).

% Fleeing gets rid of threats
at(
  N,
  consequence_of(
    X,
    o(success, escape),
    resolves,
    potential(problem, relation(threatening, From, Fearful))
  )
) :-
  at(N, action(X, flee)),
  at(N, arg(X, fearful, Fearful)),
  at(N, arg(X, from, From)),
  st(N, relation(threatening, From, Fearful)),
  story_op(N, build_options).

% Fleeing puts things off-stage, unless they're a party member, in which case
% it puts everything not in your party or belonging to it off-stage:

at(
  N,
  consequence_of(X, o(success, escape), state(off_stage, Fearful))
) :-
  at(N, action(X, leave)),
  at(N, arg(X, fearful, Fearful)),
  not st(N, state(party_member, Fearful)),
  story_op(N, build_options).

at(
  N,
  consequence_of(X, o(success, escape), state(off_stage, Item))
) :-
  at(N, action(X, leave)),
  at(N, arg(X, fearful, Fearful)),
  not st(N, state(party_member, Fearful)),
  st(N, relation(has_item, Fearful, Item)),
  story_op(N, build_options).

% All of the things you are moving away from: 

move_away_from(N, X, inst(Type, Inst)) :-
  at(N, action(X, flee)),
  at(N, arg(X, fearful, Fearful)),
  st(N, state(party_member, Fearful)),
  st(N, inst(Type, Inst)),
  not st(N, state(party_member, inst(Type, Inst))),
  0 = {
    st(N, relation(has_item, PartyMember, inst(Type, Inst))) :
      st(N, state(party_member, PartyMember))
  },
  story_op(N, build_options).

at(
  N,
  consequence_of(X, o(success, escape), state(off_stage, Thing))
) :-
  at(N, action(X, leave)),
  at(N, arg(X, fearful, Fearful)),
  st(N, state(party_member, Fearful)),
  move_away_from(N, X, Thing),
  story_op(N, build_options).

% You can be injured if you fail to escape:

at(N, consequence_of(X, o(get_injured, injured), state(injured, Fearful))) :-
  at(N, action(X, flee)),
  at(N, arg(X, fearful, Fearful)),
  story_op(N, build_options).
% TODO: something about the attacker's intent?
