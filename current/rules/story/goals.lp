% goals.lp
% Rules dealing with player goals.

at(N, player_goal(preserve_health(inst(actor, you)))) :- story_node(N).
at(N, player_goal(preserve_health(Member))) :-
  story_node(N),
  st(N, state(party_member, Member)).

% TODO: more player goals!
