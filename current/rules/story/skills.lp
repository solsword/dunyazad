% skills.lp
% Rules about skills and actions.

skill(unintelligent).
skill(monstrous).
restricted_skill(unintelligent).
restricted_skill(monstrous).

skill(tinkering).
skill(fighting).
skill(wilderness_lore).
skill(music).
skill(elocution).
skill(healing).
%skill(thievery). %TODO: Make this useful
skill(literacy).
%skill(prayer). TODO: Make this useful
skill(acrobatics).
skill(storytelling).
% TODO: sailing?
% TODO: More skills!

skill_name(unintelligent, "unintelligent").
skill_name(monstrous, "monstrous").
skill_name(tinkering, "tinkering").
skill_name(fighting, "fighting").
skill_name(wilderness_lore, "wilderness lore").
skill_name(music, "music").
skill_name(elocution, "elocution").
skill_name(healing, "healing").
%skill_name(thievery, "thievery").
skill_name(literacy, "literacy").
skill_name(prayer, "prayer").
skill_name(acrobatics, "acrobatics").

% Universal skill links:

at(N, skill_link(Skill, Link, NeedsTool, Action, Arg, Outcome)) :-
  skill_link(Skill, Link, NeedsTool, Action, Arg, Outcome),
  story_op(N, build_options).

% Skill link unpacking:

% 'required' skill links depress expectations if a skill and/or tool is missing:
at(N, unlikely_outcome(X, Outcome)) :-
  at(N, action(X, Action)),
  at(N, arg(X, Arg, Actor)),
  at(N, skill_link(Skill, required, NeedsTool, Action, Arg, Outcome)),
  not st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, unlikely_outcome(X, Outcome)) :-
  at(N, action(X, Action)),
  at(N, arg(X, Arg, Actor)),
  at(N, skill_link(Skill, required, tool, Action, Arg, Outcome)),
  st(N, property(has_skill, Actor, Skill)),
  not at(N, has_tool_for(Actor, Skill)),
  story_op(N, build_options).

% An outcome value is "likely" if all other possibilities are unlikely:
at(N, likely_outcome(X, o(OutVar, OutVal))) :-
  at(N, action(X, Action)),
  outcome_val(Action, OutVar, OutVal),
  PossibleOutcomes = { outcome_val(Action, OutVar, PossibleVal) },
  PossibleOutcomes - 1 = { at(N, unlikely_outcome(X, o(OutVar, UnlikelyVal))) },
  not at(N, unlikely_outcome(X, o(OutVar, OutVal))),
  story_op(N, build_options).

% An outcome is unlikely if it is excluded by a likely outcome:
at(N, unlikely_outcome(X, Excluded)) :-
  at(N, action(X, Action)),
  outcome_excludes(Action, Likely, Excluded),
  at(N, likely_outcome(X, Likely)).

% 'promotes' as opposed to 'required' links directly increase expectations:
at(N, likely_outcome(X, Outcome)) :-
  at(N, action(X, Action)),
  at(N, arg(X, Arg, Actor)),
  at(N, skill_link(Skill, promotes, no_tool, Action, Arg, Outcome)),
  st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, likely_outcome(X, Outcome)) :-
  at(N, action(X, Action)),
  at(N, arg(X, Arg, Actor)),
  at(N, skill_link(Skill, promotes, tool, Action, Arg, Outcome)),
  st(N, property(has_skill, Actor, Skill)),
  at(N, has_tool_for(Actor, Skill)),
  story_op(N, build_options).

% 'avoids' is the inverse of 'promotes':
at(N, unlikely_outcome(X, Outcome)) :-
  at(N, action(X, Action)),
  at(N, arg(X, Arg, Actor)),
  at(N, skill_link(Skill, avoids, no_tool, Action, Arg, Outcome)),
  st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, unlikely_outcome(X, Outcome)) :-
  at(N, action(X, Action)),
  at(N, arg(X, Arg, Actor)),
  at(N, skill_link(Skill, avoids, tool, Action, Arg, Outcome)),
  st(N, property(has_skill, Actor, Skill)),
  at(N, has_tool_for(Actor, Skill)),
  story_op(N, build_options).

% 'contest' links using between to name two arguments & compare skill levels:
at(N, likely_outcome(X, Win)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, AOne, Skill)),
  not st(N, property(has_skill, ATwo, Skill)),
  story_op(N, build_options).

at(N, unlikely_outcome(X, Lose)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, AOne, Skill)),
  not st(N, property(has_skill, ATwo, Skill)),
  story_op(N, build_options).

at(N, likely_outcome(X, Lose)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  not st(N, property(has_skill, AOne, Skill)),
  st(N, property(has_skill, ATwo, Skill)),
  story_op(N, build_options).

at(N, unlikely_outcome(X, Win)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  not st(N, property(has_skill, AOne, Skill)),
  st(N, property(has_skill, ATwo, Skill)),
  story_op(N, build_options).

at(N, likely_outcome(X, Win)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  not st(N, property(has_skill, AOne, Skill)),
  not st(N, property(has_skill, ATwo, Skill)),
  at(N, has_tool_for(AOne, Skill)),
  not at(N, has_tool_for(ATwo, Skill)),
  story_op(N, build_options).

at(N, unlikely_outcome(X, Lose)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  not st(N, property(has_skill, AOne, Skill)),
  not st(N, property(has_skill, ATwo, Skill)),
  at(N, has_tool_for(AOne, Skill)),
  not at(N, has_tool_for(ATwo, Skill)),
  story_op(N, build_options).

at(N, unlikely_outcome(X, Win)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  not st(N, property(has_skill, AOne, Skill)),
  not st(N, property(has_skill, ATwo, Skill)),
  not at(N, has_tool_for(AOne, Skill)),
  at(N, has_tool_for(ATwo, Skill)),
  story_op(N, build_options).

at(N, likely_outcome(X, Lose)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  not st(N, property(has_skill, AOne, Skill)),
  not st(N, property(has_skill, ATwo, Skill)),
  not at(N, has_tool_for(AOne, Skill)),
  at(N, has_tool_for(ATwo, Skill)),
  story_op(N, build_options).

at(N, likely_outcome(X, Win)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, AOne, Skill)),
  st(N, property(has_skill, ATwo, Skill)),
  at(N, has_tool_for(AOne, Skill)),
  not at(N, has_tool_for(ATwo, Skill)),
  story_op(N, build_options).

at(N, unlikely_outcome(X, Lose)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, AOne, Skill)),
  st(N, property(has_skill, ATwo, Skill)),
  at(N, has_tool_for(AOne, Skill)),
  not at(N, has_tool_for(ATwo, Skill)),
  story_op(N, build_options).

at(N, unlikely_outcome(X, Win)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, AOne, Skill)),
  st(N, property(has_skill, ATwo, Skill)),
  not at(N, has_tool_for(AOne, Skill)),
  at(N, has_tool_for(ATwo, Skill)),
  story_op(N, build_options).

at(N, likely_outcome(X, Lose)) :-
  at(N, action(X, Action)),
  at(N, arg(X, One, AOne)),
  at(N, arg(X, Two, ATwo)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(One, Two),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, AOne, Skill)),
  st(N, property(has_skill, ATwo, Skill)),
  not at(N, has_tool_for(AOne, Skill)),
  at(N, has_tool_for(ATwo, Skill)),
  story_op(N, build_options).

% Some predicates that make gleaning a list of applicable skills/tools easy:

at(N, relevant_skill(X, Actor, has, Skill)) :-
  at(N, action(X, Action)),
  at(N, skill_link(Skill, Link, NeedsTool, Action, Arg, Outcome)),
  at(N, arg(X, Arg, Actor)),
  st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, relevant_skill(X, Actor, missing, Skill)) :-
  at(N, action(X, Action)),
  at(N, skill_link(Skill, Link, NeedsTool, Action, Arg, Outcome)),
  at(N, arg(X, Arg, Actor)),
  not st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, relevant_tool(X, Actor, has, Item)) :-
  at(N, action(X, Action)),
  at(N, skill_link(Skill, LinkType, tool, Action, Arg, Outcome)),
  at(N, arg(X, Arg, Actor)),
  st(N, property(has_skill, Actor, Skill)),
  st(N, relation(has_item, Actor, Item)),
  is_instance(N, Item, Category),
  tool_for(Category, Skill),
  story_op(N, build_options).

at(N, relevant_tool(X, Actor, missing, Skill)) :-
  at(N, action(X, Action)),
  at(N, skill_link(Skill, LinkType, tool, Action, Arg, Outcome)),
  at(N, arg(X, Arg, Actor)),
  st(N, property(has_skill, Actor, Skill)),
  not at(N, has_tool_for(Actor, Skill)),
  story_op(N, build_options).

at(N, relevant_skill(X, Actor, has, Skill)) :-
  at(N, action(X, Action)),
  at(N, arg(X, AArg, Actor)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Action,
      between(AArg, Other),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, relevant_skill(X, Actor, missing, Skill)) :-
  at(N, action(X, Action)),
  at(N, arg(X, AArg, Actor)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Action,
      between(AArg, Other),
      either(Win, Lose)
    )
  ),
  not st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, relevant_skill(X, Actor, has, Skill)) :-
  at(N, action(X, Action)),
  at(N, arg(X, AArg, Actor)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Action,
      between(Other, AArg),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, relevant_skill(X, Actor, missing, Skill)) :-
  at(N, action(X, Action)),
  at(N, arg(X, AArg, Actor)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Action,
      between(Other, AArg),
      either(Win, Lose)
    )
  ),
  not st(N, property(has_skill, Actor, Skill)),
  story_op(N, build_options).

at(N, relevant_tool(X, Actor, has, Item)) :-
  at(N, action(X, Action)),
  at(N, arg(X, AArg, Actor)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(AArg, Other),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, Actor, Skill)),
  st(N, relation(has_item, Actor, Item)),
  is_instance(N, Item, Category),
  tool_for(Category, Skill),
  story_op(N, build_options).

at(N, relevant_tool(X, Actor, missing, Skill)) :-
  at(N, action(X, Action)),
  at(N, arg(X, AArg, Actor)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(AArg, Other),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, Actor, Skill)),
  not at(N, has_tool_for(Actor, Skill)),
  story_op(N, build_options).

at(N, relevant_tool(X, Actor, has, Item)) :-
  at(N, action(X, Action)),
  at(N, arg(X, AArg, Actor)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(Other, AArg),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, Actor, Skill)),
  st(N, relation(has_item, Actor, Item)),
  is_instance(N, Item, Category),
  tool_for(Category, Skill),
  story_op(N, build_options).

at(N, relevant_tool(X, Actor, missing, Skill)) :-
  at(N, action(X, Action)),
  at(N, arg(X, AArg, Actor)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Action,
      between(Other, AArg),
      either(Win, Lose)
    )
  ),
  st(N, property(has_skill, Actor, Skill)),
  not at(N, has_tool_for(Actor, Skill)),
  story_op(N, build_options).
