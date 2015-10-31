% goals.lp
% Rules dealing with player goals.

at(N, player_goal(preserve_health(Member))) :-
  st(N, state(party_member, Member)),
  story_op(N, initialize_node).

at(N, player_goal(avoid_threats_to(Member))) :-
  st(N, state(party_member, Member)),
  story_op(N, initialize_node).

at(N, player_goal(avoid_accusations(Member))) :-
  st(N, state(party_member, Member)),
  story_op(N, initialize_node).

at(N, player_goal(preserve_original_form(Member))) :-
  st(N, state(party_member, Member)),
  story_op(N, initialize_node).

at(N, player_goal(reclaim_property(Member, StolenItem))) :-
  st(N, state(party_member, Member)),
  st(N, relation(stolen_from, Member, StolenItem)),
  story_op(N, initialize_node).

at(N, player_goal(as_intended(Member))) :-
  st(N, state(party_member, Member)),
  story_op(N, initialize_node).

at(N, player_goal(have_tool_for(Member, Skill))) :-
  st(N, state(party_member, Member)),
  st(N, property(has_skill, Member, Skill)),
  story_op(N, initialize_node).

% the player doesn't want others to be threatened or accused as long as they're
% not aggressive:

at(N, guilty(inst(actor, Guilty))) :-
  st(N, relation(threatening, inst(actor, Guilty), Anyone)).

at(N, guilty(inst(actor, Guilty))) :-
  st(N, relation(accusing, inst(actor, Guilty), inst(actor, Innocent))),
  not at(N, guity(inst(actor, Innocent))).

at(N, guilty(inst(actor, Guilty))) :-
  st(N, relation(has_item, inst(actor, Guilty), inst(item, Stolen))),
  st(N, relation(stolen_from, inst(actor, Victim), inst(item, Stolen))).

at(N, player_goal(avoid_threats_to(inst(actor, Innocent)))) :-
  st(N, inst(actor, Innocent)),
  st(N, relation(threatening, Someone, inst(actor, Innocent))),
  not at(N, guilty(inst(actor, Innocent))),
  story_op(N, initialize_node).

%at(N, player_goal(avoid_threats_to(inst(actor, Innocent)))) :-
%  st(N, inst(actor, Innocent)),
%  at(N, consequence(O, relation(threatening, Someone, inst(actor, Innocent)))),
%  not at(N, guilty(inst(actor, Innocent))),
%  story_op(N, initialize_node).

at(N, player_goal(avoid_accusations(inst(actor, Innocent)))) :-
  st(N, inst(actor, Innocent)),
  st(N, relation(accusing, Someone, inst(actor, Innocent))),
  not at(N, guilty(inst(actor, Innocent))),
  story_op(N, initialize_node).

% TODO: Some way of making this not crash & overload things?
%at(N, player_goal(avoid_accusations(inst(actor, Innocent)))) :-
%  st(N, inst(actor, Innocent)),
%  at(N, consequence(O, relation(accusing, Someone, inst(actor, Innocent)))),
%  not at(N, guilty(inst(actor, Innocent))),
%  story_op(N, initialize_node).

% TODO: more player goals!
