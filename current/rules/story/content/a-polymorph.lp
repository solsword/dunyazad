% polymorph
action(polymorph).

chaotic(polymorph).

% arguments

argument(polymorph, caster, actor).
argument(polymorph, target, actor).
initiator(polymorph, caster).
default_intent(polymorph, o(success, cursed)).

% outcomes

outcome_val(polymorph, success, cursed).
outcome_val(polymorph, success, no_effect).

% skills:

skill_link(
  sorcery, required, tool,
  polymorph, caster,
  o(success, cursed)
).

skill_link(
  sorcery, promotes, tool,
  polymorph, caster,
  o(success, cursed)
).

error(m("Unintelligent caster.", N, X)) :-
  at(N, action(X, polymorph)),
  at(N, arg(X, caster, Unintelligent)),
  st(N, property(has_skill, Unintelligent, unintelligent)),
  story_op(N, build_options).

% effects:

% Note: this one effect has many rammifications; see
% content/p-polymorphed.lp
at(
  N,
  consequence_of(
    X,
    o(success, cursed),
    property(polymorphed, Target, OriginalType)
  )
) :-
  at(N, action(X, polymorph)),
  at(N, arg(X, target, Target)),
  st(N, property(type, Target, OriginalType)),
  story_op(N, build_options).
