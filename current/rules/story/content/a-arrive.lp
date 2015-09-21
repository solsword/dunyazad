%% arrive
%action(arrive).
%
%% arguments
%
%argument(arrive, subject, actor).
%off_stage_okay(arrive, subject).
%initiator(arrive, subject).
%default_intent(arrive, o(result, arrived)).
%
%% outcomes
%
%% It doesn't make sense for arriving to fail, so there's only one outcome:
%outcome_val(arrive, result, arrived).
%
%% skills
%
%% none
%
%% (hack) prohibit threatened actors from moving back on-stage:
%% TODO: More general mechanism for this kind of thing?
%
%error(m("Threatened actor arrived.", N, X)) :-
%  at(N, action(X, arrive)),
%  at(N, arg(X, subject, Subject)),
%  st(N, relation(threatening, Someone, Subject)),
%  story_op(N, build_options).
%
%% The subject must start off-stage and arrives on-stage:
%
%error(m("On-stage subject arrived.", N, X)) :-
%  at(N, action(X, arrive)),
%  at(N, arg(X, subject, Subject)),
%  not st(N, state(off_stage, Subject)),
%  story_op(N, build_options).
%
%at(N, consequence_of(o(result, arrived), _not, state(off_stage, Subject))) :-
%  at(N, action(X, arrive)),
%  at(N, arg(X, subject, Subject)),
%  story_op(N, build_options).
%
%at(N, consequence_of(o(result, arrived), _not, state(off_stage, Item))) :-
%  at(N, action(X, arrive)),
%  at(N, arg(X, subject, Subject)),
%  st(N, relation(has_item, Subject, Item)),
%  story_op(N, build_options).
