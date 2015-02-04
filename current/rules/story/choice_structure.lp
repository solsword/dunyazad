% option constraints:
% -------------------

% relevance
at(N, relevant(option(X))) :-
  at(N, consequence(option(X), _not, Something)),
  at(N, potential(PTyp, Something)),
  at(N, option(X)),
  story_op(N, build_options).

at(N, relevant(option(X))) :-
  at(N, emphasizes(option(X), Something)),
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

% choices should be frequent
error(m("Not enough choices", A, B, C)) :-
  successor(A, AOpt, B),
  successor(B, BOpt, C),
  not node_type(A, choice),
  not node_type(B, choice),
  not node_type(C, choice),
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

% emphasizing something once is enough
error(m("Repeated emphasis", N, option(X))) :-
  story_node(N),
  at(Prev, emphasizes(option(X), Something)),
  successor(Prev, option(X), N),
  at(N, emphasizes(option(Y), Something)),
  story_op(N, build_options).

% within-vignette boredom

% TODO: within-vignette boredom!

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

% the story shouldn't end too soon:

error(m("Story too short", N)) :-
  node_type(N, ending),
  shortest_path(N, L),
  L < min_story_length.
