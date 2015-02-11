% eval.lp
% Rules dealing with how choice structures are evaluated.

% Quantitative outcome valorization:

at(N, score(X, 12)) :-
  at(N, deals_with(X, Pt)),
  at(N, potential(problem, Pt)),
  story_op(N, build_options).

at(N, score(X, -12)) :-
  at(N, emphasizes(X, Pt)),
  at(N, potential(problem, Pt)),
  story_op(N, build_options).

at(N, score(X, 5)) :-
  at(N, deals_with(X, Pt)),
  at(N, potential(opportunity, Pt)),
  story_op(N, build_options).

at(N, score(X, -5)) :-
  at(N, emphasizes(X, Pt)),
  at(N, potential(opportunity, Pt)),
  story_op(N, build_options).

at(N, score(X, 3)) :-
  at(N, nullifies(X, Prb)),
  at(N, potential(problem, Prb)),
  story_op(N, build_options).

at(N, score(X, -3)) :-
  at(N, nullifies(X, Opp)),
  at(N, potential(opportunity, Opp)),
  story_op(N, build_options).

at(N, total_score(option(Opt), TS)) :-
  story_node(N),
  at(N, option(Opt)),
  TS = #sum{ S : at(N, score(option(Opt), S)) },
  story_op(N, build_options).

% Qualitative outcome valorization:

at(N, utility(X, helpful)) :-
  at(N, score(X, S)),
  S > 0.

at(N, utility(X, harmful)) :-
  at(N, score(X, S)),
  S < 0.

at(N, utility(option(O), neutral)) :-
  story_node(N),
  at(N, option(O)),
  not at(N, utility(option(O), helpful)),
  not at(N, utility(option(O), harmful)).

at(N, utility(X, mixed)) :-
  at(N, utility(X, helpful)),
  at(N, utility(X, harmful)).

at(N, utility(X, pure_helpful)) :-
  at(N, utility(X, helpful)),
  not at(N, utility(X, harmful)).

at(N, utility(X, pure_harmful)) :-
  not at(N, utility(X, helpful)),
  at(N, utility(X, harmful)).

% Some simple edge-case double-checks:

error(m("Purely helpful option with <= 0 utility.", N, X)) :-
  at(N, utility(X, pure_helpful)),
  at(N, total_score(X, S)),
  S <= 0.

error(m("Purely harmful option with >= 0 utility.", N, X)) :-
  at(N, utility(X, pure_harmful)),
  at(N, total_score(X, S)),
  S >= 0.

error(m("Neutral option with != 0 utility.", N, X)) :-
  at(N, utility(X, neutral)),
  at(N, total_score(X, S)),
  S != 0.


% Overall outcome classification:

at(N, outcome_classification(X, great)) :-
  at(N, utility(X, pure_helpful)),
  at(N, total_score(X, S)),
  S >= 6.

at(N, outcome_classification(X, good)) :-
  at(N, utility(X, pure_helpful)),
  at(N, total_score(X, S)),
  S <= 5.

at(N, outcome_classification(X, bad)) :-
  at(N, utility(X, pure_harmful)),
  at(N, total_score(X, S)),
  S >= -5.

at(N, outcome_classification(X, awful)) :-
  at(N, utility(X, pure_harmful)),
  at(N, total_score(X, S)),
  S <= -6.

at(N, outcome_classification(X, neutral)) :-
  at(N, utility(X, neutral)).

at(N, outcome_classification(X, ambiguous)) :-
  at(N, utility(X, mixed)),
  at(N, total_score(X, S)),
  S <= 3,
  S >= -3.

at(N, outcome_classification(X, worth)) :-
  at(N, utility(X, mixed)),
  at(N, total_score(X, S)),
  S >= 4.

at(N, outcome_classification(X, not_worth)) :-
  at(N, utility(X, mixed)),
  at(N, total_score(X, S)),
  S <= -4.

% Error check:

error(m("Unclassified outcome.", N, option(O))) :-
  story_node(N),
  at(N, option(O)),
  0 = {
    at(N, outcome_classification(option(O), OC))
  }.


% Option expectations:

at(N, expectation(X, unknown)) :-
  at(N, action(X, Action)),
  0 = {
    at(N, skill_link(Action, Skill))
  }.

at(N, expectation(X, success)) :-
  at(N, action(X, Action)),
  at(N, skill_link(Action, Skill)),
  at(N, initiator(X, Actor)),
  st(N, property(has_skill, Actor, Skill)),
  not requires_tool(Action).

at(N, expectation(X, success)) :-
  at(N, action(X, Action)),
  at(N, skill_link(Action, Skill)),
  at(N, initiator(X, Actor)),
  st(N, property(has_skill, Actor, Skill)),
  requires_tool(Action),
  st(N, property(has_item, Actor, Tool)),
  tool_for(Tool, Skill).

at(N, expectation(X, failure)) :-
  at(N, action(X, Action)),
  at(N, initiator(X, Actor)),
  0 = {
    st(N, property(has_skill, Actor, Skill)) : at(N, skill_link(Action, Skill))
  }.

at(N, expectation(X, failure)) :-
  at(N, action(X, Action)),
  at(N, initiator(X, Actor)),
  requires_tool(Action),
  0 = {
    st(N, property(has_skill, Actor, Skill)) :
      at(N, skill_link(Action, Skill)),
      st(N, property(has_item, Actor, Tool)),
      tool_for(Tool, Skill)
  }.

% Error check:

error(m("Expectation for both failure and success.", N, X)) :-
  at(N, expectation(X, failure)),
  at(N, expectation(X, success)).

% Overall choice structure evaluation:

at(N, choice_structure(straightforward)) :-
  node_status_reached(N, branched),
  at(N, expectation(X, success)),
  at(N, expectation(Y, failure)).

at(N, choice_structure(spoiled)) :-
  node_status_reached(N, branched),
  All = { at(N, expectation(X, success)) },
  All = { at(N, option(O)) }.

at(N, choice_structure(grim)) :-
  node_status_reached(N, branched),
  All = { at(N, expectation(X, failure)) },
  All = { at(N, option(O)) }.

at(N, choice_structure(mysterious)) :-
  node_status_reached(N, branched),
  story_node(N),
  0 = {
    at(N, expectation(X, success));
    at(N, expectation(Y, failure))
  }.

at(N, choice_structure(relaxed)) :-
  node_status_reached(N, branched),
  at(N, expectation(X, success)),
  at(N, expectation(Z, unknown)),
  0 = { at(N, expectation(W, failure)) }.

at(N, choice_structure(desperate)) :-
  node_status_reached(N, branched),
  at(N, expectation(X, failure)),
  at(N, expectation(Z, unknown)),
  0 = { at(N, expectation(W, success)) }.

% Error checking:

error(m("Choice has multiple structures.", N)) :-
  story_node(N),
  2 <= { at(N, choice_structure(CS)) }.
