% option constraints:
% -------------------

% relevance: any action which might cause a potential to dissappear is relevant
at(N, relevant(option(X))) :-
  at(N, consequence_of(option(X), Outcome, _not, Something)),
  at(N, potential(PTyp, Something)),
  at(N, option(X)),
  story_op(N, build_options).

error(m("Irrelevant option", N, option(X))) :-
  at(N, option(X)),
  not at(N, relevant(option(X))),
  not at(N, action(option(X), travel_onwards)),
  not at(N, action(option(X), reach_destination)),
  story_op(N, build_options).

% redundancy
error(m("Redundant option", N, option(X))) :-
  node_type(N, choice),
  at(N, action(option(X), Act)),
  at(N, action(option(Y), Act)),
  X != Y,
  story_op(N, build_options).

% your party as the active agent:
error(m("Non-party-initiated option", N, option(X))) :-
  node_type(N, choice),
  at(N, initiator(option(X), Init)),
  not st(N, state(party_member, Init)),
  story_op(N, build_options).

% overall structure constraints:
% ------------------------------

% the story shouldn't end too soon:
error(m("Story too short", N)) :-
  node_type(N, ending),
  shortest_path(N, L),
  L < min_story_length.

% choices should be frequent
error(m("Not enough choices", A, B, C)) :-
  successor(A, AOpt, B),
  successor(B, BOpt, C),
  not node_type(A, choice),
  not node_type(B, choice),
  not node_type(C, choice),
  not resolves_vignette(B, BOpt),
  story_op(C, build_options).

% events shouldn't re-hash the choice you just made or a prevoius event
arg_mismatch(N, option(NO), O, option(OO)) :-
  at(N, arg(option(NO), Arg, Val)),
  at(O, option(OO)),
  0 = {
    at(O, arg(option(OO), Arg, Val)) : at(O, arg(option(OO), Arg, Val))
  },
  N < O,
  story_op(N, build_options).

arg_mismatch(O, OOpt, N, NOpt) :-
  arg_mismatch(N, NOpt, O, OOpt),
  story_op(N, build_options).

error(m("Event duplicates previous action", N, Prev, Action)) :-
  successor(Prev, Opt, N),
  node_type(N, event),
  at(Prev, action(Opt, Action)),
  at(N, action(AnyOpt, Action)),
  not arg_mismatch(Prev, Opt, N, AnyOpt),
  story_op(N, build_options).

% failing to deal with a potential twice is enough
error(m("Repeated failure", Three, option(Z))) :-
  story_node(One),
  successor(One, option(X), Two),
  successor(Two, option(Y), Three),
  at(One, potential(PTyp, Something)),
  at(Two, potential(PTyp, Something)),
  at(Three, potential(PTyp, Something)),
  at(One, consequence_of(option(X), DidntHappen, _not, Something)),
  not at(One, outcome(option(X), DidntHappen)),
  at(Two, consequence_of(option(Y), AlsoDidntHappen, _not, Something)),
  not at(Two, outcome(option(Y), AlsoDidntHappen)),
  at(Three, consequence_of(option(Z), MustHappen, _not, Something)),
  not at(Three, outcome(option(Z), MustHappen)),
  story_op(Three, build_options).

% failing to deal with a problem is no excuse for ignoring it (opportunities
% don't care)
error(m("Ignored failure to solve problem", Two, option(Y))) :-
  story_node(One),
  successor(One, option(X), Two),
  successor(Two, option(Y), Three),
  at(One, potential(problem, Problem)),
  at(Two, potential(problem, Problem)),
  at(One, consequence_of(option(X), DidntHappen, _not, Problem)),
  0 = { at(Two, consequence_of(option(Y), Any, _not, Problem)) },
  story_op(Two, build_options).

% problems shouldn't re-occur within a vignette
error(m("Recurring potential", Second)) :-
  story_node(First),
  before(First, Second),
  vignette(First, V),
  vignette(Second, V),
  at(First, potential(PType, Pot)),
  not at(Second, potential(PType, Pot)),
  at(Second, consequence(option(X), Pot)),
  story_op(Second, build_options).

% vignette structure constraints:
% -------------------------------

% vignette-level boredom
error(m("Repeated vignette setup", N, R)) :-
  story_op(N, build_options),
  setup(N, Boring),
  successor(Prev, Opt, N),
  vignette(Prev, R),
  setup(R, Boring).

times_setup_used_before(N, Setup, Count) :-
  story_op(N, build_options),
  possible_setup(Setup),
  Count = {
    setup(Prev, Setup) : before(Prev, N)
  }.

unique_setups_used_before(N, Count) :-
  story_op(N, build_options),
  Count = {
    possible_setup(Setup) : setup(Prev, Setup), before(Prev, N)
  }.

error(m("Boring vignette setup", N)) :-
  story_op(N, build_options),
  setup(N, Boring),
  times_setup_used_before(N, Boring, Boredom),
  unique_setups_used_before(N, Unique),
  Boredom > 0,
  Boredom + 1 >= Unique.

% every vignette ends with one of the special actions "travel onwards" or
% "reach destination:"
1 = {
  node_type(N, event);
  node_type(N, ending);
  error(m("Vignette resolution has wrong node type.", End, Opt, N))
} :-
  resolves_vignette(End, Opt),
  successor(End, Opt, N),
  story_op(N, build_options).

at(N, action(option(1), travel_onwards)) :-
  node_type(N, event),
  resolves_vignette(End, Opt),
  successor(End, Opt, N),
  story_op(N, build_options).

at(N, action(option(1), reach_destination)) :-
  node_type(N, ending),
  resolves_vignette(End, Opt),
  successor(End, Opt, N),
  story_op(N, build_options).


% choice poetics constraints:
% ---------------------------

choices_before(N, X) :-
  X = {
    node_type(B, choice) : before(B, N)
  },
  story_op(N, build_options).


% the story must contain at least a minimum number of choices:
error(m("Too few choices before ending", N)) :-
  node_type(N, ending),
  choices_before(N, L),
  L < min_story_choices.


% The first few choices have a set structure:
%choice_structure_plan(0, relaxed).
%choice_structure_plan(1, relaxed).
%choice_structure_plan(2, straightforward).
%choice_structure_plan(3, mysterious).

error(m("Deviated from initial choices plan", N)) :-
  node_type(N, choice),
  story_op(N, build_options),
  choices_before(N, L),
  choice_structure_plan(L, CS),
  not at(N, choice_structure(CS)).

% The midgame is less constrained:
