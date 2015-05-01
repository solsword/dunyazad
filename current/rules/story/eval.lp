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
  at(N, consequence_of(X, Outcome, _not, State)),
  state_advances(Goal, State).

at(N, expectation(X, threatens, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_achieves(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_advances(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, State)),
  state_achieves(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_hinders(Goal, State).

at(N, expectation(X, enables, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_fails(Goal, State).

% Stakes are based on the goals that are threatened:
at(N, stakes(high)) :-
  at(N, expectation(X, Exp, Goal)),
  at(N, stakes(Goal, high)),
  at(N, player_goal(Goal)),
  story_op(N, build_options).

at(N, stakes(low)) :-
  at(N, expectation(X, Exp, Goal)),
  at(N, stakes(Goal, low)),
  at(N, player_goal(Goal)),
  not at(N, stakes(high)),
  story_op(N, build_options).

at(N, stakes(none)) :-
  story_node(N),
  not at(N, stakes(high)),
  not at(N, stakes(low)),
  story_op(N, build_options).

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
  at(N, consequence_of(X, Outcome, _not, State)),
  state_advances(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, fails, Goal)) :-
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
  at(N, consequence_of(X, Outcome, _not, State)),
  state_hinders(Goal, State),
  at(N, likely_outcome(X, Outcome)).

at(N, expectation(X, achieves, Goal)) :-
  at(N, consequence_of(X, Outcome, _not, State)),
  state_fails(Goal, State),
  at(N, likely_outcome(X, Outcome)).


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

at(N, outcome_perception(X, hinders, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_advances(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, consequence(X, State)),
  state_advances(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_hinders(Goal, State).

at(N, outcome_perception(X, hinders, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_achieves(Goal, State).

at(N, outcome_perception(X, advances, Goal)) :-
  at(N, consequence(X, _not, State)),
  state_fails(Goal, State).

% fails and achieves

at(N, outcome_perception(X, fails, Goal)) :-
  at(N, consequence(X, State)),
  state_fails(Goal, State).

at(N, outcome_perception(X, achieves, Goal)) :-
  at(N, consequence(X, State)),
  state_achieves(Goal, State).

% different types of option feels:

at(N, option_feel(option(X), sure_thing)) :-
  story_op(N, build_options),
  at(N, option(X)),
  at(N, expectation(option(X), achieves, Goal)),
  at(N, stakes(Goal, Stakes)),
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
      at(N, stakes(G1, Stakes1)),
      higher_stakes(Stakes1, none);
    at(N, player_goal(G2)) :
      at(N, expectation(option(X), enables, G2)),
      at(N, stakes(G2, Stakes2)),
      higher_stakes(Stakes2, none)
  },
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(option(X), threatens, G3));
    at(N, player_goal(G4)) : at(N, expectation(option(X), fails, G4))
  }.

at(N, option_feel(option(X), hopeful)) :-
  story_op(N, build_options),
  at(N, option(X)),
  1 <= { at(N, stakes(low)); at(N, stakes(high)) },
  at(N, player_goal(HopeGoal)),
  at(N, expectation(option(X), achieves, HopeGoal)),
  at(N, stakes(HopeGoal, HopeStakes)),
  at(N, player_goal(ThreatGoal)),
  at(N, expectation(option(X), threatens, ThreatGoal)),
  at(N, stakes(ThreatGoal, ThreatStakes)),
  at_least_as_high(HopeStakes, ThreatStakes),
  higher_stakes(ThreatStakes, none),
  0 = {
    at(N, player_goal(G3)) : at(N, expectation(option(X), fails, G3))
  }.

at(N, option_feel(option(X), risky)) :-
  story_op(N, build_options),
  at(N, option(X)),
  at(N, player_goal(HopeGoal)),
  at(N, expectation(option(X), enables, HopeGoal)),
  at(N, stakes(HopeGoal, HopeStakes)),
  at(N, player_goal(ThreatGoal)),
  at(N, expectation(option(X), threatens, ThreatGoal)),
  at(N, stakes(ThreatGoal, ThreatStakes)),
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
  at(N, player_goal(FailsGoal)),
  at(N, expectation(option(X), fails, FailsGoal)),
  at(N, stakes(AchievesGoal, Stakes)),
  at(N, stakes(FailsGoal, Stakes)),
  higher_stakes(Stakes, none).

at(N, option_feel(option(X), irrelevant)) :-
  story_op(N, build_options),
  at(N, option(X)),
  0 = {
    at(N, player_goal(G1)) :
      at(N, expectation(option(X), enables, G1)),
      at(N, stakes(G1, Stakes1)),
      higher_stakes(Stakes1, none);
    at(N, player_goal(G2)) :
      at(N, expectation(option(X), achieves, G2)),
      at(N, stakes(G2, Stakes2)),
      higher_stakes(Stakes2, none);
    at(N, player_goal(G3)) :
      at(N, expectation(option(X), threatens, G3)),
      at(N, stakes(G3, Stakes3)),
      higher_stakes(Stakes3, none);
    at(N, player_goal(G4)) :
      at(N, expectation(option(X), fails, G4)),
      at(N, stakes(G4, Stakes4)),
      higher_stakes(Stakes4, none)
  }.

at(N, option_feel(option(X), longshot)) :-
  story_op(N, build_options),
  at(N, option(X)),
  at(N, player_goal(FailGoal)),
  at(N, expectation(option(X), fails, FailGoal)),
  at(N, stakes(FailGoal, FailStakes)),
  at(N, player_goal(HopeGoal)),
  at(N, expectation(option(X), enables, HopeGoal)),
  at(N, stakes(HopeGoal, HopeStakes)),
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
      at(N, stakes(G1, Stakes1)),
      higher_stakes(Stakes1, none);
    at(N, player_goal(G2)) :
      at(N, expectation(option(X), fails, G2)),
      at(N, stakes(G2, Stakes2)),
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
  at(N, stakes(DoomedGoal, DoomedStakes)),
  higher_stakes(DoomedStakes, none),
  0 = {
    at(N, player_goal(G2)) : at(N, expectation(option(X), enables, G2));
    at(N, player_goal(G3)) : at(N, expectation(option(X), achieves, G3))
  }.

% choice structures:

at(N, choice_structure(mysterious)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), irrelevant));
    -1,at(N, option(X)) : at(N, option(X))
  }.

at(N, choice_structure(obvious)) :-
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

at(N, choice_structure(relaxed)) :-
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
  at(N, stakes(Stakes)),
  higher_stakes(high, Stakes).

at(N, choice_structure(powerful)) :-
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
  at(N, stakes(high)).

at(N, choice_structure(uncomfortable)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  1 <= {
    at(N, option(X)) : at(N, option_feel(option(X), longshot));
    at(N, option(X)) : at(N, option_feel(option(X), risky))
  },
  0 <= {
    at(N, option(X)) : at(N, option_feel(option(X), tradeoff));
    at(N, option(X)) : at(N, option_feel(option(X), irrelevant));
    at(N, option(X)) : at(N, option_feel(option(X), hopeful));
    at(N, option(X)) : at(N, option_feel(option(X), safe))
  },
  at(N, stakes(Stakes)),
  higher_stakes(Stakes, none).

at(N, choice_structure(tradeoffs)) :-
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
  at(N, stakes(Stakes)),
  higher_stakes(Stakes, none).

at(N, choice_structure(positive_alternatives)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  at(N, stakes(SomeStakes)),
  higher_stakes(SomeStakes, none),
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), safe));
    -1,at(N, option(X)) : at(N, option(X))
  },
  0 = #sum {
    1,at(N, option(X)) :
      at(N, expectation(option(X), achieves, SomeGoal)),
      at(N, stakes(SomeGoal, SomeStakes));
    -1,at(N, option(X)) : at(N, option(X))
  },
  at(N, player_goal(Goal1)),
  at(N, player_goal(Goal2)),
  Goal1 != Goal2,
  2 <= {
    at(N, option(X)) : at(N, expectation(option(X), achieves, Goal1));
    at(N, option(X)) : at(N, expectation(option(X), achieves, Goal2))
  }.

at(N, choice_structure(negative_alternatives)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  at(N, stakes(SomeStakes)),
  higher_stakes(SomeStakes, none),
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), longshot));
    1,at(N, option(X)) : at(N, option_feel(option(X), bad));
    1,at(N, option(X)) : at(N, option_feel(option(X), doomed));
    -1,at(N, option(X)) : at(N, option(X))
  },
  0 = #sum {
    1,at(N, option(X)) :
      at(N, expectation(option(X), fails, SomeGoal)),
      at(N, stakes(SomeGoal, SomeStakes));
    -1,at(N, option(X)) : at(N, option(X))
  },
  at(N, player_goal(Goal1)),
  at(N, player_goal(Goal2)),
  Goal1 != Goal2,
  2 <= {
    at(N, option(X)) : at(N, expectation(option(X), fails, Goal1));
    at(N, option(X)) : at(N, expectation(option(X), fails, Goal2))
  }.

at(N, choice_structure(pressured)) :-
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
  at(N, stakes(Stakes)),
  higher_stakes(Stakes, none).

at(N, choice_structure(dangerous)) :-
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
  at(N, stakes(high)).

at(N, choice_structure(unfortunate)) :-
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
  at(N, stakes(Stakes)),
  higher_stakes(high, Stakes).

at(N, choice_structure(bleak)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, option_feel(option(X), longshot));
    1,at(N, option(X)) : at(N, option_feel(option(X), bad));
    1,at(N, option(X)) : at(N, option_feel(option(X), doomed));
    -1,at(N, option(X)) : at(N, option(X))
  },
  at(N, stakes(Stakes)),
  higher_stakes(Stakes, none).

at(N, choice_structure(depressing)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, expectation(option(X), fails, Goal));
    -1,at(N, option(X)) : at(N, option(X))
  },
  0 = {
    at(N, option(X)) : at(N, expectation(option(X), achieves, Goal));
    at(N, option(X)) : at(N, expectation(option(X), enables, Goal))
  },
  at(N, stakes(Stakes)),
  higher_stakes(high, Stakes).

at(N, choice_structure(doomed)) :-
  story_op(N, build_options),
  1 < { at(N, option(X)) },
  0 = #sum {
    1,at(N, option(X)) : at(N, expectation(option(X), fails, Goal));
    -1,at(N, option(X)) : at(N, option(X))
  },
  0 = {
    at(N, option(X)) : at(N, expectation(option(X), achieves, Goal));
    at(N, option(X)) : at(N, expectation(option(X), enables, Goal))
  },
  at(N, stakes(high)).

% TODO: Worry about options without any assigned feel?
