% goals.lp
% Rules dealing with player goals.

at(N, player_goal(preserve_health(inst(actor, you)))) :- story_node(N).
at(N, player_goal(preserve_health(Member))) :-
  story_node(N),
  st(N, state(party_member, Member)).

at(N, player_goal(avoid_threats_to(inst(actor, you)))) :- story_node(N).
at(N, player_goal(avoid_threats_to(Member))) :-
  story_node(N),
  st(N, state(party_member, Member)).

at(N, player_goal(as_intended(inst(actor, you)))) :- story_node(N).
at(N, player_goal(as_intended(Member))) :-
  story_node(N),
  st(N, state(party_member, Member)).

% TODO: more player goals!
