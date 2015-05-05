% goals.lp
% Rules dealing with player goals.

at(N, player_goal(preserve_health(Member))) :-
  st(N, state(party_member, Member)),
  story_op(N, initialize_node).

at(N, player_goal(avoid_threats_to(Member))) :-
  st(N, state(party_member, Member)),
  story_op(N, initialize_node).

at(N, player_goal(as_intended(Member))) :-
  st(N, state(party_member, Member)),
  story_op(N, initialize_node).

at(N, player_goal(have_tool_for(Member, Skill))) :-
  st(N, state(party_member, Member)),
  st(N, property(has_skill, Member, Skill)),
  story_op(N, initialize_node).

% the player doesn't want others to be threatened as long as they're not
% aggressive:
at(N, player_goal(avoid_threats_to(inst(actor, Innocent)))) :-
  st(N, inst(actor, Innocent)),
  st(N, relation(threatening, Someone, inst(actor, Innocent))),
  0 = {
    st(N, relation(threatening, inst(actor, Innocent), Anyone))
  },
  story_op(N, initialize_node).

% TODO: more player goals!
