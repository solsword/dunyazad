% leave
action(leave).

% arguments

argument(leave, subject, actor).
initiator(leave, subject).
default_intent(leave, o(result, gone)).

% outcomes

outcome_val(leave, result, gone).
outcome_val(leave, result, detained).

% skills

% none

% You can't leave if you're being threatened (you should flee instead):

error(m("Walked out of a threatening situation.", N, X)) :-
  at(N, action(X, leave)),
  at(N, arg(X, subject, Subject)),
  st(N, relation(threatening, Someone, Subject)),
  story_op(N, build_options).

% Since it's all the same in the end, the initator of leaving should always be
% you.

error(m("Party member other than you left...")) :-
  at(N, action(X, leave)),
  at(N, arg(X, subject, Subject)),
  st(N, state(party_member, Subject)),
  Subject != inst(actor, you),
  story_op(N, build_options).

% Leaving puts things off-stage, unless they're you, in which case it puts
% everything not in your party or belonging to it off-stage:

at(
  N,
  consequence_of(X, o(result, gone), state(off_stage, Subject))
) :-
  at(N, action(X, leave)),
  at(N, arg(X, subject, Subject)),
  Subject != inst(actor, you),
  story_op(N, build_options).

at(
  N,
  consequence_of(X, o(result, gone), state(off_stage, Item))
) :-
  at(N, action(X, leave)),
  at(N, arg(X, subject, Subject)),
  Subject != inst(actor, you),
  st(N, relation(has_item, Subject, Item)),
  story_op(N, build_options).

% All of the things you are moving away from: 

move_away_from(N, X, inst(Type, Inst)) :-
  at(N, action(X, leave)),
  at(N, arg(X, subject, inst(actor, you))),
  st(N, inst(Type, Inst)),
  not st(N, state(party_member, inst(Type, Inst))),
  0 = {
    st(N, relation(has_item, PartyMember, inst(Type, Inst))) :
      st(N, state(party_member, PartyMember))
  },
  story_op(N, build_options).

at(
  N,
  consequence_of(X, o(result, gone), state(off_stage, Thing))
) :-
  at(N, action(X, leave)),
  at(N, arg(X, subject, inst(actor, you))),
  move_away_from(N, X, Thing),
  story_op(N, build_options).
