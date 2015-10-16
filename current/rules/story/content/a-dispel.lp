% dispel
action(dispel).

chaotic(dispel).

% arguments

argument(dispel, caster, actor).
argument(dispel, target, actor).
initiator(dispel, caster).
default_intent(dispel, o(success, dispelled)).

% outcomes

outcome_val(dispel, success, dispelled).
outcome_val(dispel, success, no_effect).

% skills:

skill_link(
  sorcery, contest, tool,
  dispel,
  between(caster, target),
  either(o(success, cursed), o(success, no_effect))
).

error(m("Attempt to dispel a non-cursed target.", N, X)) :-
  at(N, action(X, dispel)),
  at(N, arg(X, target, Target)),
  0 = {
    st(N, property(polymorphed, Target, Any)) :
      st(N, property(polymorphed, Target, Any)),
      any_class(Any)
  },
  story_op(N, build_options).

error(m("Unintelligent caster.", N, X)) :-
  at(N, action(X, dispel)),
  at(N, arg(X, caster, Unintelligent)),
  st(N, property(has_skill, Unintelligent, unintelligent)),
  story_op(N, build_options).

% effects:

% Note: this one effect has many rammifications; see content/p-polymorphed.lp
at(
  N,
  consequence_of(
    X,
    o(success, dispelled),
    resolved,
    potential(problem, property(polymorphed, Target, OriginalType))
  )
) :-
  at(N, action(X, dispel)),
  at(N, arg(X, target, Target)),
  st(N, property(polymorphed, Target, OriginalType)),
  story_op(N, build_options).
