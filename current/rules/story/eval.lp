% eval.lp
% Rules dealing with how choice structures are evaluated.

% Expectations:
%--------------

option_relevance(irrelevant).
option_relevance(threatens).
option_relevance(enables).
option_relevance(fails).
option_relevance(achieves).

stakes_level(none).
stakes_level(low).
stakes_level(high).

higher_stakes(low, none).
higher_stakes(high, none).
higher_stakes(high, low).

at_least_as_high(SL1, SL2) :- higher_stakes(SL1, SL2).
at_least_as_high(SL, SL) :- stakes_level(SL).

% Options which have no other expectation are expected to be irrelevant:
at(N, expectation(option(X), irrelevant, G)) :-
  at(N, option(X)),
  at(N, player_goal(G)),
  0 = {
    at(N, expectation(option(X), threatens, G));
    at(N, expectation(option(X), enables, G));
    at(N, expectation(option(X), fails, G));
    at(N, expectation(option(X), achieves, G))
  }.

% Threatens and enables based on possible outcomes:

at(N, expectation(X, threatens, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_hinders(Goal, State).

at(N, expectation(X, threatens, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_fails(Goal, State).

at(N, expectation(X, threatens, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, Outcome, _not, State)),
  state_advances(Goal, State).

at(N, expectation(X, threatens, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, Outcome, _not, State)),
  state_achieves(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_advances(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_achieves(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, Outcome, _not, State)),
  state_hinders(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, Outcome, _not, State)),
  state_fails(Goal, State).

% Fails and achieves based on 'likely' outcomes:

at(N, expectation(X, fails, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_hinders(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_fails(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, Outcome, _not, State)),
  state_advances(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, Outcome, _not, State)),
  state_achieves(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_advances(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_achieves(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, Outcome, _not, State)),
  state_hinders(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, Outcome, _not, State)),
  state_fails(Goal, State),
  at(N, likely_outcome(X, Outcome)).

% Special expectations for travel_onwards and reach_destination.
% Basically, if there's a state that would be removed by one of these actions,
% we know that we're actually leaving it behind, so if that statie *either*
% hinders/fails *or* advances/achieves any goal, we expect to fail that goal by
% leaving the state behind, because either we're leaving a problem behind which
% will now never get solved (in the case that the state is hindering/failing a
% goal) or we're moving out of a situaiton in which our goal is
% advanced/achieved.

at(N, expectation(option(X), fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence_of(option(X), Outcome, _not, State)),
  state_fails(Goal, State).

at(N, expectation(option(X), fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence_of(option(X), Outcome, _not, State)),
  state_hinders(Goal, State).

% This one is threatens instead of fails.
at(N, expectation(option(X), threatens, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence_of(option(X), Outcome, _not, State)),
  state_advances(Goal, State).

at(N, expectation(option(X), fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence_of(option(X), Outcome, _not, State)),
  state_achieves(Goal, State).


% Stakes are based on the goals that are threatened:

% Percieved stakes (per-option):
at(N, perceived_option_stakes(X, high)) :-
  story_op(N, build_options),
  at(N, expectation(X, Exp, Goal)),
  Exp != irrelevant,
  at(N, goal_stakes(Goal, high)),
  at(N, player_goal(Goal)).

at(N, perceived_option_stakes(X, low)) :-
  story_op(N, build_options),
  at(N, expectation(X, Exp, Goal)),
  Exp != irrelevant,
  at(N, goal_stakes(Goal, low)),
  at(N, player_goal(Goal)),
  not at(N, perceived_option_stakes(X, high)).

at(N, perceived_option_stakes(option(X), none)) :-
  story_op(N, build_options),
  at(N, option(X)),
  not at(N, perceived_option_stakes(option(X), high)),
  not at(N, perceived_option_stakes(option(X), low)).

% Percieved stakes (overall):
at(N, perceived_stakes(high)) :-
  at(N, perceived_option_stakes(X, high)).

at(N, perceived_stakes(low)) :-
  at(N, perceived_option_stakes(X, low)),
  0 = {
    at(N, perceived_option_stakes(Any, high))
  }.

at(N, perceived_stakes(none)) :-
  story_node(N),
  0 = {
    at(N, perceived_option_stakes(Any, high));
    at(N, perceived_option_stakes(Any, low))
  }.

% Actual stakes (per outcome):
at(N, outcome_stakes(X, high)) :-
  story_op(N, build_options),
  at(N, outcome_perception(X, Any, Goal)),
  at(N, goal_stakes(Goal, high)),
  at(N, player_goal(Goal)).

at(N, outcome_stakes(X, low)) :-
  story_op(N, build_options),
  at(N, outcome_perception(X, Any, Goal)),
  at(N, goal_stakes(Goal, low)),
  at(N, player_goal(Goal)),
  not at(N, outcome_stakes(X, high)).

at(N, outcome_stakes(option(X), none)) :-
  story_op(N, build_options),
  at(N, option(X)),
  not at(N, outcome_stakes(option(X), high)),
  not at(N, outcome_stakes(option(X), low)).

% Actual stakes (overall):
at(N, actual_stakes(high)) :-
  story_op(N, build_options),
  at(N, outcome_perception(X, Any, Goal)),
  Any != irrelevant,
  at(N, player_goal(Goal)),
  at(N, goal_stakes(Goal, high)).

at(N, actual_stakes(low)) :-
  story_op(N, build_options),
  at(N, outcome_perception(X, Any, Goal)),
  Any != irrelevant,
  at(N, player_goal(Goal)),
  at(N, goal_stakes(Goal, low)),
  not at(N, actual_stakes(high)).

at(N, actual_stakes(none)) :-
  story_op(N, build_options),
  not at(N, actual_stakes(high)),
  not at(N, actual_stakes(low)).

% different types of option feels:

at(N, option_feel(option(X), sure_thing)) :-
  story_op(N, build_options),
  at(N, option(X)),
  at(N, player_goal(Goal)),
  at(N, expectation(option(X), achieves, Goal)),
  at(N, goal_stakes(Goal, Stakes)),
  Stakes != none,
  0 = {
    at(N, player_goal(G2)) : at(N, expectation(option(X), threatens, G2));
    at(N, player_goal(G3)) : at(N, expectation(option(X), fails, G3))
  }.

at(N, option_feel(option(X), safe)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= {
    at(N, player_goal(G1)) :
      at(N, expectation(option(X), achieves, G1)),
      at(N, goal_stakes(G1, Stakes1)),
      higher_stakes(Stakes1, none);
    at(N, player_goal(G2)) :
      at(N, expectation(option(X), enables, G2)),
      at(N, goal_stakes(G2, Stakes2)),
      higher_stakes(Stakes2, none)
  },
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(option(X), threatens, G3));
    at(N, player_goal(G4)) : at(N, expectation(option(X), fails, G4))
  }.

at(N, option_feel(option(X), hopeful)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= { at(N, perceived_stakes(low)); at(N, perceived_stakes(high)) },
  at(N, player_goal(HopeGoal)),
  at(N, expectation(option(X), achieves, HopeGoal)),
  at(N, goal_stakes(HopeGoal, HopeStakes)),
  at(N, player_goal(ThreatGoal)),
  at(N, expectation(option(X), threatens, ThreatGoal)),
  at(N, goal_stakes(ThreatGoal, ThreatStakes)),
  at_least_as_high(HopeStakes, ThreatStakes),
  higher_stakes(ThreatStakes, none),
  0 = {
    at(N, player_goal(G3)) :
      at(N, expectation(option(X), threatens, G3)), 
      at(N, goal_stakes(G3, S3)),
      higher_stakes(S3, ThreatStakes)
  },
  0 = {
    at(N, player_goal(G4)) : at(N, expectation(option(X), fails, G4))
  }.

at(N, option_feel(option(X), risky)) :-
  story_op(N, build_options),
  at(N, option(X)),
  at(N, player_goal(HopeGoal)),
  at(N, expectation(option(X), enables, HopeGoal)),
  at(N, goal_stakes(HopeGoal, HopeStakes)),
  at(N, player_goal(ThreatGoal)),
  at(N, expectation(option(X), threatens, ThreatGoal)),
  at(N, goal_stakes(ThreatGoal, ThreatStakes)),
  at_least_as_high(ThreatStakes, HopeStakes),
  higher_stakes(HopeStakes, none),
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(option(X), achieves, G3));
    at(N, player_goal(G4)) : at(N, expectation(option(X), fails, G4))
  }.

at(N, option_feel(option(X), tradeoff)) :-
  story_op(N, build_options),
  at(N, option(X)),
  at(N, player_goal(AchievesGoal)),
  at(N, expectation(option(X), achieves, AchievesGoal)),
  at(N, goal_stakes(AchievesGoal, Stakes)),
  at(N, player_goal(FailsGoal)),
  at(N, expectation(option(X), fails, FailsGoal)),
  at(N, goal_stakes(FailsGoal, Stakes)),
  higher_stakes(Stakes, none).

at(N, option_feel(option(X), irrelevant)) :-
  story_op(N, build_options),
  at(N, option(X)),
  0 = {
    at(N, player_goal(G1)) :
      at(N, expectation(option(X), enables, G1)),
      at(N, goal_stakes(G1, Stakes1)),
      higher_stakes(Stakes1, none);
    at(N, player_goal(G2)) :
      at(N, expectation(option(X), achieves, G2)),
      at(N, goal_stakes(G2, Stakes2)),
      higher_stakes(Stakes2, none);
    at(N, player_goal(G3)) :
      at(N, expectation(option(X), threatens, G3)),
      at(N, goal_stakes(G3, Stakes3)),
      higher_stakes(Stakes3, none);
    at(N, player_goal(G4)) :
      at(N, expectation(option(X), fails, G4)),
      at(N, goal_stakes(G4, Stakes4)),
      higher_stakes(Stakes4, none)
  }.

at(N, option_feel(option(X), longshot)) :-
  story_op(N, build_options),
  at(N, option(X)),
  at(N, player_goal(FailGoal)),
  at(N, expectation(option(X), fails, FailGoal)),
  at(N, goal_stakes(FailGoal, FailStakes)),
  at(N, player_goal(HopeGoal)),
  at(N, expectation(option(X), enables, HopeGoal)),
  at(N, goal_stakes(HopeGoal, HopeStakes)),
  at_least_as_high(FailStakes, HopeStakes),
  higher_stakes(HopeStakes, none),
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(option(X), achieves, G3))
  }.

at(N, option_feel(option(X), bad)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= {
    at(N, player_goal(G1)) :
      at(N, expectation(option(X), threatens, G1)),
      at(N, goal_stakes(G1, Stakes1)),
      higher_stakes(Stakes1, none);
    at(N, player_goal(G2)) :
      at(N, expectation(option(X), fails, G2)),
      at(N, goal_stakes(G2, Stakes2)),
      higher_stakes(Stakes2, none)
  },
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(option(X), enables, G3));
    at(N, player_goal(G4)) : at(N, expectation(option(X), achieves, G4))
  }.

at(N, option_feel(option(X), doomed)) :-
  story_op(N, build_options),
  at(N, option(X)),
  at(N, player_goal(DoomedGoal)),
  at(N, expectation(option(X), fails, DoomedGoal)),
  at(N, goal_stakes(DoomedGoal, DoomedStakes)),
  higher_stakes(DoomedStakes, none),
  0 = {
    at(N, player_goal(G2)) : at(N, expectation(option(X), enables, G2));
    at(N, player_goal(G3)) : at(N, expectation(option(X), achieves, G3))
  }.

% option-based choice structures:

at(N, option_structure(mysterious)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), irrelevant));
    -1,at(N, option(X)) : at(N, option(X))
  }.

at(N, option_structure(uncertain)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), risky));
    -1,at(N, option(X)) : at(N, option(X))
  }.

at(N, option_structure(obvious)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  1 = {
    at(N, option(X)) : at(N, option_feel(option(X), hopeful));
    at(N, option(X)) : at(N, option_feel(option(X), safe))
  },
  0 = {
    at(N, option(X)) : at(N, option_feel(option(X), tradeoff));
    at(N, option(X)) : at(N, option_feel(option(X), irrelevant))
  }.

at(N, option_structure(relaxed)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), safe));
    -1,at(N, option(X)) : at(N, option(X))
  },
  at(N, perceived_stakes(Stakes)),
  higher_stakes(high, Stakes).

at(N, option_structure(powerful)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  1 <= {
    at(N, option(X)) : at(N, option_feel(option(X), hopeful));
    at(N, option(X)) : at(N, option_feel(option(X), safe))
  },
  0 = {
    at(N, player_goal(G1)) : at(N, expectation(option(X), threatens, G1));
    at(N, player_goal(G2)) : at(N, expectation(option(X), fails, G2))
  },
  at(N, perceived_stakes(high)).

at(N, option_structure(uncomfortable)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  1 <= {
    at(N, option(X)) : at(N, option_feel(option(X), longshot));
    at(N, option(X)) : at(N, option_feel(option(X), risky))
  },
  0 = {
    at(N, option(X)) : at(N, option_feel(option(X), tradeoff));
    at(N, option(X)) : at(N, option_feel(option(X), irrelevant));
    at(N, option(X)) : at(N, option_feel(option(X), hopeful));
    at(N, option(X)) : at(N, option_feel(option(X), safe))
  },
  at(N, perceived_stakes(Stakes)),
  higher_stakes(Stakes, none).

at(N, option_structure(tradeoffs)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), tradeoff));
    -1,at(N, option(X)) : at(N, option(X))
  },
  at(N, player_goal(Goal1)),
  at(N, player_goal(Goal2)),
  Goal1 != Goal2,
  2 <= {
    at(N, option(X)) : at(N, expectation(option(X), achieves, Goal1));
    at(N, option(X)) : at(N, expectation(option(X), achieves, Goal2))
  },
  at(N, perceived_stakes(Stakes)),
  higher_stakes(Stakes, none).

at(N, option_structure(positive_alternatives)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  at(N, perceived_stakes(SomeStakes)),
  higher_stakes(SomeStakes, none),
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), safe));
    1,at(N, option(X)) : at(N, option_feel(option(X), hopeful));
    -1,at(N, option(X)) : at(N, option(X))
  },
  0 = #sum {
    1,at(N, option(X)) :
      at(N, expectation(option(X), achieves, SomeGoal)),
      at(N, goal_stakes(SomeGoal, SomeStakes)),
      at(N, player_goal(SomeGoal));
    -1,at(N, option(X)) : at(N, option(X))
  },
  at(N, player_goal(Goal1)),
  at(N, player_goal(Goal2)),
  Goal1 != Goal2,
  2 <= {
    at(N, option(X)) : at(N, expectation(option(X), achieves, Goal1));
    at(N, option(X)) : at(N, expectation(option(X), achieves, Goal2))
  }.

at(N, option_structure(negative_alternatives)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  at(N, perceived_stakes(SomeStakes)),
  higher_stakes(SomeStakes, none),
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), longshot));
    1,at(N, option(X)) : at(N, option_feel(option(X), bad));
    -1,at(N, option(X)) : at(N, option(X))
  },
  0 = #sum {
    1,at(N, option(X)) :
      at(N, expectation(option(X), fails, SomeGoal)),
      at(N, goal_stakes(SomeGoal, SomeStakes)),
      at(N, player_goal(Goal));
    -1,at(N, option(X)) : at(N, option(X))
  }.

at(N, option_structure(pressured)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = {
    at(N, option(X)) : at(N, option_feel(option(X), safe));
    at(N, option(X)) : at(N, option_feel(option(X), irrelevant))
  },
  1 <= {
    at(N, option(X)) : at(N, option_feel(option(X), hopeful));
    at(N, option(X)) : at(N, option_feel(option(X), tradeoff))
  },
  at(N, perceived_stakes(Stakes)),
  higher_stakes(Stakes, none).

at(N, option_structure(dangerous)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = {
    at(N, option(X)) : at(N, option_feel(option(X), safe));
    at(N, option(X)) : at(N, option_feel(option(X), hopeful));
    at(N, option(X)) : at(N, option_feel(option(X), tradeoff));
    at(N, option(X)) : at(N, option_feel(option(X), irrelevant))
  },
  1 <= {
    at(N, option(X)) : at(N, option_feel(option(X), risky))
  },
  at(N, perceived_stakes(high)).

at(N, option_structure(unfortunate)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = {
    at(N, option(X)) : at(N, option_feel(option(X), safe));
    at(N, option(X)) : at(N, option_feel(option(X), hopeful));
    at(N, option(X)) : at(N, option_feel(option(X), tradeoff));
    at(N, option(X)) : at(N, option_feel(option(X), irrelevant))
  },
  1 <= {
    at(N, option(X)) : at(N, option_feel(option(X), risky))
  },
  at(N, perceived_stakes(Stakes)),
  higher_stakes(high, Stakes).

at(N, option_structure(bleak)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), longshot));
    1,at(N, option(X)) : at(N, option_feel(option(X), bad));
    -1,at(N, option(X)) : at(N, option(X))
  },
  at(N, perceived_stakes(Stakes)),
  higher_stakes(Stakes, none).

at(N, option_structure(depressing)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) :
      at(N, expectation(option(X), fails, Goal)),
      at(N, player_goal(Goal));
    -1,at(N, option(X)) : at(N, option(X))
  },
  0 = {
    at(N, option(X)) :
      at(N, expectation(option(X), achieves, Goal)),
      at(N, player_goal(Goal));
    at(N, option(X)) :
      at(N, expectation(option(X), enables, Goal)),
      at(N, player_goal(Goal))
  },
  at(N, perceived_stakes(Stakes)),
  higher_stakes(high, Stakes).

at(N, option_structure(doomed)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) :
      at(N, expectation(option(X), fails, Goal)),
      at(N, player_goal(Goal));
    -1,at(N, option(X)) : at(N, option(X))
  },
  0 = {
    at(N, option(X)) :
      at(N, expectation(option(X), achieves, Goal)),
      at(N, player_goal(Goal));
    at(N, option(X)) :
      at(N, expectation(option(X), enables, Goal)),
      at(N, player_goal(Goal))
  },
  at(N, perceived_stakes(high)).

% TODO: Worry about options without any assigned feel?


% Outcomes:
%----------

outcome_perception(irrelevant).
outcome_perception(hinders).
outcome_perception(advances).
outcome_perception(fails).
outcome_perception(achieves).

% hinders and advances

at(N, outcome_perception(X, hinders, Goal)) :-
  at(N, consequence(X, State)),
  state_hinders(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, consequence(X, State)),
  state_advances(Goal, State).

at(N, outcome_perception(X, hinders, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence(X, _not, State)),
  state_advances(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence(X, _not, State)),
  state_hinders(Goal, State).

at(N, outcome_perception(X, hinders, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence(X, _not, State)),
  state_achieves(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence(X, _not, State)),
  state_fails(Goal, State).

% fails and achieves

at(N, outcome_perception(X, fails, Goal)) :-
  at(N, consequence(X, State)),
  state_fails(Goal, State).

at(N, outcome_perception(X, achieves, Goal)) :-
  at(N, consequence(X, State)),
  state_achieves(Goal, State).

% outcome perceptions for travel_onwards and reach_destination:

at(N, outcome_perception(option(X), fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence(option(X), _not, State)),
  state_achieves(Goal, State).

at(N, outcome_perception(option(X), hinders, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence(option(X), _not, State)),
  state_advances(Goal, State).

at(N, outcome_perception(option(X), fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence(option(X), _not, State)),
  state_hinders(Goal, State).

at(N, outcome_perception(option(X), fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence(option(X), _not, State)),
  state_fails(Goal, State).

% Possible outcome perceptions (used to determine which outcomes are important):

% hinders and advances

at(N, possible_outcome_perception(X, O, hinders, Goal)) :-
  at(N, consequence_of(X, O, State)),
  state_hinders(Goal, State).

at(N, possible_outcome_perception(X, O, advances, Goal)) :-
  at(N, consequence_of(X, O, State)),
  state_advances(Goal, State).

at(N, possible_outcome_perception(X, O, hinders, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, O, _not, State)),
  state_advances(Goal, State).

at(N, possible_outcome_perception(X, O, advances, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, O, _not, State)),
  state_hinders(Goal, State).

at(N, possible_outcome_perception(X, O, hinders, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, O, _not, State)),
  state_achieves(Goal, State).

at(N, possible_outcome_perception(X, O, advances, Goal)) :-
  at(N, action(X, Action)),
  Action != travel_onwards, Action != reach_destination,
  at(N, consequence_of(X, O, _not, State)),
  state_fails(Goal, State).

% fails and achieves

at(N, possible_outcome_perception(X, O, fails, Goal)) :-
  at(N, consequence_of(X, O, State)),
  state_fails(Goal, State).

at(N, possible_outcome_perception(X, O, achieves, Goal)) :-
  at(N, consequence_of(X, O, State)),
  state_achieves(Goal, State).

% possible outcome perceptions for travel_onwards and reach_destination:

at(N, possible_outcome_perception(option(X), Outcome, fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence_of(option(X), Outcome, _not, State)),
  state_achieves(Goal, State).

at(N, possible_outcome_perception(option(X), Outcome, hinders, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence_of(option(X), Outcome, _not, State)),
  state_advances(Goal, State).

at(N, possible_outcome_perception(option(X), Outcome, fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence_of(option(X), Outcome, _not, State)),
  state_hinders(Goal, State).

at(N, possible_outcome_perception(option(X), Outcome, fails, Goal)) :-
  at(N, option(X)),
  1 = {
    at(N, action(option(X), travel_onwards));
    at(N, action(option(X), reach_destination))
  },
  at(N, consequence_of(option(X), Outcome, _not, State)),
  state_fails(Goal, State).

% Mixed outcome perceptions:

% When we achieve a goal with no drawbacks:
at(N, outcome_overall(X, great)) :-
  at(N, outcome_perception(X, achieves, Goal)),
  at(N, player_goal(Goal)),
  at(N, outcome_stakes(X, Stakes)),
  at(N, goal_stakes(Goal, Stakes)),
  0 = {
    at(N, outcome_perception(X, fails, AnyGoal)) :
      at(N, outcome_perception(X, fails, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes));
    at(N, outcome_perception(X, hinders, AnyGoal)) :
      at(N, outcome_perception(X, hinders, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes))
  }.

% When we advance a goal with no drawbacks:
at(N, outcome_overall(X, good)) :-
  at(N, outcome_perception(X, advances, Goal)),
  at(N, player_goal(Goal)),
  at(N, outcome_stakes(X, Stakes)),
  at(N, goal_stakes(Goal, Stakes)),
  0 = {
    at(N, outcome_perception(X, achieves, AnyGoal)) :
      at(N, outcome_perception(X, achieves, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes));
    at(N, outcome_perception(X, fails, AnyGoal)) :
      at(N, outcome_perception(X, fails, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes));
    at(N, outcome_perception(X, hinders, AnyGoal)) :
      at(N, outcome_perception(X, hinders, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes))
  }.

% When we achieve at least one goal but fail at least one other goal, or
% advance one goal while hindering another:
at(N, outcome_overall(X, tradeoff)) :-
  at(N, outcome_stakes(X, Stakes)),
  at(N, outcome_perception(X, achieves, Goal)),
  at(N, player_goal(Goal)),
  at(N, goal_stakes(Goal, Stakes)),
  at(N, outcome_perception(X, fails, FailedGoal)),
  at(N, player_goal(FailedGoal)),
  at(N, goal_stakes(FailedGoal, Stakes)).

at(N, outcome_overall(X, tradeoff)) :-
  at(N, outcome_stakes(X, Stakes)),
  at(N, outcome_perception(X, advances, Goal)),
  at(N, player_goal(Goal)),
  at(N, goal_stakes(Goal, Stakes)),
  at(N, outcome_perception(X, hinders, HinderedGoal)),
  at(N, player_goal(HinderedGoal)),
  at(N, goal_stakes(HinderedGoal, Stakes)),
  0 = {
    at(N, outcome_perception(X, achieves, AnyGoal)) :
      at(N, outcome_perception(X, achieves, AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes)),
      at(N, player_goal(AnyGoal));
    at(N, outcome_perception(X, fails, AnyGoal)) :
      at(N, outcome_perception(X, fails, AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes)),
      at(N, player_goal(AnyGoal))
  }.

% When we achieve at least one goal but hinder at least one other:
at(N, outcome_overall(X, worth_it)) :-
  at(N, outcome_stakes(X, Stakes)),
  at(N, outcome_perception(X, achieves, Goal)),
  at(N, player_goal(Goal)),
  at(N, goal_stakes(Goal, Stakes)),
  at(N, outcome_perception(X, hinders, HinderedGoal)),
  at(N, player_goal(HinderedGoal)),
  at(N, goal_stakes(HinderedGoal, Stakes)),
  0 = {
    at(N, outcome_perception(X, fails, AnyGoal)) :
      at(N, outcome_perception(X, fails, AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes)),
      at(N, player_goal(AnyGoal))
  }.

% When we advance at least one goal but fail at least one other:
at(N, outcome_overall(X, not_worth_it)) :-
  at(N, outcome_stakes(X, Stakes)),
  at(N, outcome_perception(X, advances, Goal)),
  at(N, player_goal(Goal)),
  at(N, goal_stakes(Goal, Stakes)),
  at(N, outcome_perception(X, fails, FailedGoal)),
  at(N, player_goal(FailedGoal)),
  at(N, goal_stakes(FailedGoal, Stakes)),
  0 = {
    at(N, outcome_perception(X, achieves, AnyGoal)) :
      at(N, outcome_perception(X, achieves, AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes)),
      at(N, player_goal(AnyGoal))
  }.

% When we hinder a goal and make no progress otherwise:
at(N, outcome_overall(X, bad)) :-
  at(N, outcome_perception(X, hinders, Goal)),
  at(N, player_goal(Goal)),
  at(N, outcome_stakes(X, Stakes)),
  at(N, goal_stakes(Goal, Stakes)),
  0 = {
    at(N, outcome_perception(X, achieves, AnyGoal)) :
      at(N, outcome_perception(X, achieves, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes));
    at(N, outcome_perception(X, advances, AnyGoal)) :
      at(N, outcome_perception(X, advances, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes));
    at(N, outcome_perception(X, fails, AnyGoal)) :
      at(N, outcome_perception(X, fails, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes))
  }.

% When we fail a goal and make no progress otherwise:
at(N, outcome_overall(X, awful)) :-
  at(N, outcome_perception(X, fails, Goal)),
  at(N, player_goal(Goal)),
  at(N, outcome_stakes(X, Stakes)),
  at(N, goal_stakes(Goal, Stakes)),
  0 = {
    at(N, outcome_perception(X, achieves, AnyGoal)) :
      at(N, outcome_perception(X, achieves, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes));
    at(N, outcome_perception(X, advances, AnyGoal)) :
      at(N, outcome_perception(X, advances, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes))
  }.

% When an outcome doesn't affect max-stakes goals in the end:
at(N, outcome_overall(option(X), neutral)) :-
  at(N, option(X)),
  at(N, outcome_stakes(X, Stakes)),
  0 = {
    at(N, outcome_perception(option(X), NotIrrelevant, AnyGoal)) :
      at(N, outcome_perception(option(X), NotIrrelevant, AnyGoal)),
      at(N, player_goal(AnyGoal)),
      at(N, goal_stakes(AnyGoal, Stakes)),
      NotIrrelevant != irrelevant
  }.


% Outcomes affecting non-max-stakes or non-player goals are not considered
% important:
at(N, important_outcome(X, Outcome)) :-
  at(N, outcome(X, Outcome)),
  at(N, outcome_stakes(X, Stakes)),
  at(N, goal_stakes(Goal, Stakes)),
  at(N, player_goal(Goal)),
  at(N, possible_outcome_perception(X, Outcome, Any, Goal)).

% Non-effecacious outcomes that are the alternatives to outcomes that would be
% important count as important.
% TODO: Account for exclusion rules here as well to imply importance across
% multiple outcome variables?
at(N, important_outcome(X, o(SameVar, Happened))) :-
  at(N, outcome(X, o(SameVar, Happened))),
  at(N, outcome_stakes(X, Stakes)),
  at(N, goal_stakes(Goal, Stakes)),
  at(N, player_goal(Goal)),
  at(N, possible_outcome_perception(X, o(SameVar, CouldHave), Any, Goal)).

% Outcomes which are neither likely nor unlikely are labeled as "neutral:"

at(N, neutral_outcome(X, o(OutVar, OutVal))) :-
  at(N, action(X, Action)),
  outcome_val(Action, OutVar, OutVal),
  not at(N, likely_outcome(X, o(OutVar, OutVal))),
  not at(N, unlikely_outcome(X, o(OutVar, OutVal))).


% Outcome predictabilities:

at(N, outcome_predictability(X, o(OutVar, OutVal), predictable)) :-
  story_op(N, build_options),
  at(N, outcome(X, o(OutVar, OutVal))),
  at(N, likely_outcome(X, o(OutVar, OutVal))),
  1 = {
    at(N, likely_outcome(X, o(OutVar, AnyVal))) :
      at(N, likely_outcome(X, o(OutVar, AnyVal)))
  }.

at(N, outcome_predictability(X, o(OutVar, OutVal), expected)) :-
  story_op(N, build_options),
  at(N, outcome(X, o(OutVar, OutVal))),
  at(N, likely_outcome(X, o(OutVar, OutVal))),
  2 <= {
    at(N, likely_outcome(X, o(OutVar, AnyVal))) :
      at(N, likely_outcome(X, o(OutVar, AnyVal)))
  }.

at(N, outcome_predictability(X, o(OutVar, OutVal), average)) :-
  story_op(N, build_options),
  at(N, outcome(X, o(OutVar, OutVal))),
  not at(N, unlikely_outcome(X, o(OutVar, OutVal))),
  0 = {
    at(N, likely_outcome(X, o(OutVar, AnyVal))) :
      at(N, likely_outcome(X, o(OutVar, AnyVal)))
  },
  1 <= {
    at(N, unlikely_outcome(X, o(OutVar, AnyVal))) :
      at(N, unlikely_outcome(X, o(OutVar, AnyVal)))
  }.

at(N, outcome_predictability(X, o(OutVar, OutVal), unpredictable)) :-
  story_op(N, build_options),
  at(N, outcome(X, o(OutVar, OutVal))),
  0 = {
    at(N, likely_outcome(X, o(OutVar, AnyVal))) :
      at(N, likely_outcome(X, o(OutVar, AnyVal)));
    at(N, unlikely_outcome(X, o(OutVar, AnyVal))) :
      at(N, unlikely_outcome(X, o(OutVar, AnyVal)))
  }.

at(N, outcome_predictability(X, o(OutVar, OutVal), unpredictable)) :-
  story_op(N, build_options),
  at(N, action(X, Action)),
  at(N, outcome(X, o(OutVar, OutVal))),
  0 = #sum { % All possible outcome values for this variable are "unlikely"
    1, outcome_val(Action, OutVar, AnyVal) :
      outcome_val(Action, OutVar, AnyVal);
    -1, at(N, unlikely_outcome(X, o(OutVar, AnyVal))) :
      at(N, unlikely_outcome(X, o(OutVar, AnyVal)))
  }.

at(N, outcome_predictability(X, o(OutVar, OutVal), unexpected)) :-
  story_op(N, build_options),
  at(N, outcome(X, o(OutVar, OutVal))),
  at(N, neutral_outcome(X, o(OutVar, OutVal))),
  1 <= {
    at(N, likely_outcome(X, o(OutVar, AnyVal))) :
      at(N, likely_outcome(X, o(OutVar, AnyVal)))
  }.

at(N, outcome_predictability(X, o(OutVar, OutVal), unexpected)) :-
  story_op(N, build_options),
  at(N, outcome(X, o(OutVar, OutVal))),
  at(N, unlikely_outcome(X, o(OutVar, OutVal))),
  0 = {
    at(N, likely_outcome(X, o(OutVar, AnyVal))) :
      at(N, likely_outcome(X, o(OutVar, AnyVal)))
  },
  1 <= {
    at(N, neutral_outcome(X, o(OutVar, AnyVal))) :
      at(N, neutral_outcome(X, o(OutVar, AnyVal)))
  }.

at(N, outcome_predictability(X, o(OutVar, OutVal), unfair)) :-
  story_op(N, build_options),
  at(N, outcome(X, o(OutVar, OutVal))),
  at(N, unlikely_outcome(X, o(OutVar, OutVal))),
  at(N, likely_outcome(X, o(OutVar, OtherVal))),
  OutVal != OtherVal.

error(
  m(
    "Individual outcome has multiple predictabilities:",
    N, X, o(OutVar, OutVal), P1, P2
  )
) :-
  at(N, outcome_predictability(X, o(OutVar, OutVal), P1)),
  at(N, outcome_predictability(X, o(OutVar, OutVal), P2)),
  P1 < P2.

error(m("Individual outcome has no predictability:", N, X, o(OutVar, OutVal))):-
  at(N, outcome(X, o(OutVar, OutVal))),
  0 = {
    at(N, outcome_predictability(X, o(OutVar, OutVal), Any)) :
      at(N, outcome_predictability(X, o(OutVar, OutVal), Any))
  }.

% Combined predictabilities:
at(N, overall_predictability(X, predictable)) :-
  story_op(N, build_options),
  at(N, outcome(X, SomeOutcome)),
  0 = #sum { % All important outcomes for this option are predictable
    1, at(N, outcome(X, Outcome)) :
      at(N, important_outcome(X, Outcome));
    -1, at(N, outcome(X, Outcome)) :
      at(N, important_outcome(X, Outcome)),
      at(N, outcome_predictability(X, Outcome, predictable))
  },
  1 <= { at(N, important_outcome(X, Outcome)) }. % and there's at least one

at(N, overall_predictability(X, expected)) :-
  story_op(N, build_options),
  at(N, outcome(X, SomeOutcome)),
  not at(N, overall_predictability(X, predictable)),
  0 = #sum { % Outcomes for this option are predictable, expected, or average
    1, at(N, outcome(X, Outcome)) :
      at(N, important_outcome(X, Outcome));
    -1, at(N, outcome(X, Outcome)) :
      at(N, important_outcome(X, Outcome)),
      at(N, outcome_predictability(X, Outcome, predictable));
    -1, at(N, outcome(X, Outcome)) :
      at(N, important_outcome(X, Outcome)),
      at(N, outcome_predictability(X, Outcome, expected));
    -1, at(N, outcome(X, Outcome)) :
      at(N, important_outcome(X, Outcome)),
      at(N, outcome_predictability(X, Outcome, average))
  },
  1 <= { % And they're not *all* average
    at(N, outcome(X, Outcome)) :
      at(N, outcome_predictability(X, Outcome, predictable)),
      at(N, important_outcome(X, Outcome));
    at(N, outcome(X, Outcome)) :
      at(N, outcome_predictability(X, Outcome, expected)),
      at(N, important_outcome(X, Outcome))
  }.

at(N, overall_predictability(X, average)) :-
  story_op(N, build_options),
  at(N, outcome(X, SomeOutcome)),
  0 = #sum { % All important outcomes for this option are average
    1, at(N, outcome(X, Outcome)) :
      at(N, important_outcome(X, Outcome));
    -1, at(N, outcome(X, Outcome)) :
      at(N, important_outcome(X, Outcome)),
      at(N, outcome_predictability(X, Outcome, average))
  },
  1 <= { at(N, important_outcome(X, Outcome)) }. % and there's at least one

at(N, overall_predictability(X, unpredictable)) :-
  story_op(N, build_options),
  at(N, important_outcome(X, SomeOutcome)),
  at(N, outcome_predictability(X, SomeOutcome, unpredictable)),
  0 = {
    at(N, outcome(X, Outcome)) :
      at(N, outcome_predictability(X, Outcome, unexpected)),
      at(N, important_outcome(X, Outcome));
    at(N, outcome(X, Outcome)) :
      at(N, outcome_predictability(X, Outcome, unfair)),
      at(N, important_outcome(X, Outcome))
  }.

at(N, overall_predictability(X, unexpected)) :-
  story_op(N, build_options),
  at(N, important_outcome(X, SomeOutcome)),
  at(N, outcome_predictability(X, SomeOutcome, unexpected)),
  0 = {
    at(N, outcome(X, Outcome)) :
      at(N, outcome_predictability(X, Outcome, unfair)),
      at(N, important_outcome(X, Outcome))
  }.

at(N, overall_predictability(X, unfair)) :-
  story_op(N, build_options),
  at(N, important_outcome(X, SomeOutcome)),
  at(N, outcome_predictability(X, SomeOutcome, unfair)).

at(N, overall_predictability(option(X), irrelevant)) :-
  story_op(N, build_options),
  at(N, option(X)),
  0 = {
    at(N, outcome_perception(option(X), SomePerception, SomeGoal)):
     at(N, outcome_perception(option(X), SomePerception, SomeGoal)),
     at(N, player_goal(SomeGoal))
  }.

1 >= {
  at(N, overall_predictability(option(X), unrecognized))
} :-
  story_op(N, build_options),
  at(N, option(X)).

error(m("Outcome has multiple overall predictabilities:", N, X, P1, P2)) :-
  at(N, overall_predictability(X, P1)),
  at(N, overall_predictability(X, P2)),
  P1 < P2.

error(m("Outcome has no overall predictability:", N, option(X))) :-
  at(N, option(X)),
  0 = {
    at(N, overall_predictability(option(X), Any)) :
      at(N, overall_predictability(option(X), Any))
  }.

% Outcome feels:

at(N, outcome_feel(option(X), expected_success)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= {
    at(N, option_feel(option(X), sure_thing));
    at(N, option_feel(option(X), safe));
    at(N, option_feel(option(X), hopeful))
  },
  1 = {
    at(N, overall_predictability(option(X), predictable));
    at(N, overall_predictability(option(X), expected))
  },
  1 <= {
    at(N, outcome_overall(option(X), great));
    at(N, outcome_overall(option(X), good))
  }.

at(N, outcome_feel(option(X), unfair)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= {
    at(N, option_feel(option(X), sure_thing));
    at(N, option_feel(option(X), safe));
    at(N, option_feel(option(X), hopeful))
  },
  1 = {
    at(N, overall_predictability(option(X), unexpected));
    at(N, overall_predictability(option(X), unfair))
  },
  1 = {
  % TODO: Re-enable this when we've got a better lock on what is "worth it"
  %    at(N, outcome_overall(option(X), not_worth_it));
    at(N, outcome_overall(option(X), bad));
    at(N, outcome_overall(option(X), awful))
  }.

at(N, outcome_feel(option(X), nice_gamble)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= {
    at(N, option_feel(option(X), risky));
    at(N, option_feel(option(X), tradeoff));
    at(N, option_feel(option(X), irrelevant))
  },
  1 = {
    at(N, overall_predictability(option(X), average));
    at(N, overall_predictability(option(X), unpredictable))
  },
  1 = {
    at(N, outcome_overall(option(X), great));
    at(N, outcome_overall(option(X), good));
    at(N, outcome_overall(option(X), worth_it))
  }.

at(N, outcome_feel(option(X), bad_gamble)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= {
    at(N, option_feel(option(X), risky));
    at(N, option_feel(option(X), tradeoff));
    at(N, option_feel(option(X), irrelevant))
  },
  1 = {
    at(N, overall_predictability(option(X), average));
    at(N, overall_predictability(option(X), unpredictable))
  },
  1 = {
    at(N, outcome_overall(option(X), not_worth_it));
    at(N, outcome_overall(option(X), bad));
    at(N, outcome_overall(option(X), awful))
  }.

at(N, outcome_feel(option(X), expected_failure)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= {
    at(N, option_feel(option(X), longshot));
    at(N, option_feel(option(X), bad));
    at(N, option_feel(option(X), doomed))
  },
  1 = {
    at(N, overall_predictability(option(X), predictable));
    at(N, overall_predictability(option(X), expected));
    at(N, overall_predictability(option(X), average))
  },
  1 <= {
  % TODO: Re-enable this when we've got a better lock on what's "worth it"
  %  at(N, outcome_overall(option(X), not_worth_it));
    at(N, outcome_overall(option(X), bad));
    at(N, outcome_overall(option(X), awful))
  }.

at(N, outcome_feel(option(X), miracle)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= {
    at(N, option_feel(option(X), longshot));
    at(N, option_feel(option(X), bad));
    at(N, option_feel(option(X), doomed))
  },
  1 = {
    at(N, overall_predictability(option(X), unexpected));
    at(N, overall_predictability(option(X), unfair))
  },
  1 = {
    at(N, outcome_overall(option(X), great));
    at(N, outcome_overall(option(X), good));
  % TODO: but we're leaving this in for now...
    at(N, outcome_overall(option(X), worth_it))
  }.

% TODO: More outcome feels!
