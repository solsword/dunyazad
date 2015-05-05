% goals.lp
% Rules dealing with player goals.

at(N, player_goal(preserve_health(Member))) :-
  story_node(N),
  st(N, state(party_member, Member)).

at(N, player_goal(avoid_threats_to(Member))) :-
  story_node(N),
  st(N, state(party_member, Member)).

at(N, player_goal(as_intended(Member))) :-
  story_node(N),
  st(N, state(party_member, Member)).

at(N, player_goal(have_tool_for(inst(actor, Member), Skill))) :-
  story_node(N),
  st(N, state(party_member, Member)),
  st(N, property(has_skill, Member, Skill)).

% TODO: more player goals!
