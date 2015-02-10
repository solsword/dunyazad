% skills.lp
% Rules about skills and actions.

skill(tinkering).
skill(fighting).
skill(wilderness_lore).
skill(music).
skill(elocution).
skill(healing).
skill(thievery).
skill(literacy).
skill(prayer).
skill(acrobatics).
skill(storytelling).
% TODO: sailing?
% TODO: More skills!

skill_name(tinkering, "tinkering").
skill_name(fighting, "fighting").
skill_name(wilderness_lore, "wilderness lore").
skill_name(music, "music").
skill_name(elocution, "elocution").
skill_name(healing, "healing").
skill_name(thievery, "thievery").
skill_name(literacy, "literacy").
skill_name(prayer, "prayer").
skill_name(acrobatics, "acrobatics").

% Universal skill links:
at(N, skill_link(Action, Skill)) :-
  story_node(N),
  skill_link(Action, Skill).

% Universal danger:
at(N, dangerous(Action)) :-
  story_node(N),
  dangerous(Action).

% Option danger:
at(N, dangerous(Option)) :-
  at(N, dangerous(Action)),
  at(N, action(Option, Action)).
