% reclaim property

% This is borderline high vs. low stakes...

at(N, goal_stakes(reclaim_property(Victim, Item), high)) :-
  at(N, potential(problem, relation(stolen_from, Victim, Item))).

state_achieves(
  reclaim_property(Victim, Item),
  relation(has_item, Victim, Item)
) :-
  at(N, potential(problem, relation(stolen_from, Victim, Item))).
