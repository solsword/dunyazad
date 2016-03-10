%% pursue
%action(pursue).
%
%chaotic(pursue).
%
%% arguments
%
%argument(pursue, subject, actor).
%argument(pursue, object, actor).
%off_stage_okay(pursue, object).
%initiator(pursue, subject).
%default_intent(pursue, o(result, caught_up)).
%
%% outcomes
%
%outcome_val(pursue, result, caught_up).
%outcome_val(pursue, result, lost).
%
%% skills (same as flee)
%
%at(
%  N,
%  skill_link(
%    acrobatics, contest, no_tool,
%    pursue,
%    between(subject, object),
%    either(o(result, caught_up), o(result, lost))
%  )
%) :-
%  1 <= {
%    setting(N, city);
%    setting(N, town)
%  },
%  story_node(N).
%
%at(
%  N,
%  skill_link(
%    wilderness_lore, contest, no_tool,
%    pursue,
%    between(subject, object),
%    either(o(result, caught_up), o(result, lost))
%  )
%) :-
%  1 <= {
%    setting(N, road);
%    setting(N, wilderness)
%  },
%  story_node(N).
%
%% You can't pursue if you're being threatened (you should flee
%% instead):
%
%error(m("Pursued out of a threatening situation.", N, X)) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  st(N, relation(threatening, Someone, Subject)),
%  story_op(N, build_options).
%
%% The object of pursuit must be off-stage:
%
%error(m("Pursued on-stage actor.", N, X)) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, object, Object)),
%  not st(N, state(off_stage, Object)),
%  story_op(N, build_options).
%
%% Pursuit puts things off-stage, unless they're you, in which case it
%% puts % everything not in your party or belonging to it off-stage,
%% while putting % previously off-stage things on-stage (unless you
%% get lost).
%
%at(
%  N,
%  consequence_of(X, o(result, caught_up), state(off_stage, Subject))
%) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  not st(N, state(party_member, Subject)),
%  story_op(N, build_options).
%
%at(
%  N,
%  consequence_of(X, o(result, caught_up), state(off_stage, Item))
%) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  not st(N, state(party_member, Subject)),
%  st(N, relation(has_item, Subject, Item)),
%  story_op(N, build_options).
%
%at(
%  N,
%  consequence_of(X, o(result, lost), state(off_stage, Subject))
%) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  not st(N, state(party_member, Subject)),
%  story_op(N, build_options).
%
%at(
%  N,
%  consequence_of(X, o(result, lost), state(off_stage, Item))
%) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  not st(N, state(party_member, Subject)),
%  st(N, relation(has_item, Subject, Item)),
%  story_op(N, build_options).
%
%% All of the things you are moving away from: 
%
%move_away_from(N, X, inst(Type, Inst)) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  st(N, state(party_member, Subject)),
%  st(N, inst(Type, Inst)),
%  not st(N, state(party_member, inst(Type, Inst))),
%  0 = {
%    st(N, relation(has_item, PartyMember, inst(Type, Inst))) :
%      st(N, state(party_member, PartyMember))
%  },
%  not st(N, state(off_stage, inst(Type, Inst))),
%  story_op(N, build_options).
%
%at(
%  N,
%  consequence_of(X, o(result, caught_up), state(off_stage, Thing))
%) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  st(N, state(party_member, Subject)),
%  move_away_from(N, X, Thing),
%  story_op(N, build_options).
%
%at(
%  N,
%  consequence_of(X, o(result, lost), state(off_stage, Thing))
%) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  st(N, state(party_member, Subject)),
%  move_away_from(N, X, Thing),
%  story_op(N, build_options).
%
%at(
%  N,
%  consequence_of(
%    X,
%    o(result, caught_up),
%    _not,
%    state(off_stage, Thing)
%  )
%) :-
%  at(N, action(X, pursue)),
%  at(N, arg(X, subject, Subject)),
%  st(N, state(party_member, Subject)),
%  st(N, state(off_stage, Thing)),
%  story_op(N, build_options).
%% if you're lost instead, you wind up alone
