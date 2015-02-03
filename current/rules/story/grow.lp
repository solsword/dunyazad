% Nodes statuses
% --------------

% All nodes can be uninitialized, initialized, built, branched, or polished.

error(m("Node without status.", N)) :-
  story_node(N),
  0 = {
    node_status(N, uninitialized);
    node_status(N, initialized);
    node_status(N, built);
    node_status(N, branched);
    node_status(N, polished)
  }.

% Story operations allow nodes to reach new statuses:

node_status_reached(N, uninitialized) :- new_node(N).

node_status_reached(N, initialized) :- story_op(N, initialize_node).

node_status_reached(N, built) :- story_op(N, build_options).

node_status_reached(N, branched) :- story_op(N, add_branch_nodes).

node_status_reached(N, polished) :- story_op(N, add_surface).

% A node's actual status is the highest step reached:

node_status(N, uninitialized) :-
  node_status_reached(N, uninitialized),
  0 = {
    node_status_reached(N, initialized);
    node_status_reached(N, built);
    node_status_reached(N, branched);
    node_status_reached(N, polished)
  }.

node_status(N, initialized) :-
  node_status_reached(N, initialized),
  0 = {
    node_status_reached(N, built);
    node_status_reached(N, branched);
    node_status_reached(N, polished)
  }.

node_status(N, built) :-
  node_status_reached(N, built),
  0 = {
    node_status_reached(N, branched);
    node_status_reached(N, polished)
  }.

node_status(N, branched) :-
  node_status_reached(N, branched),
  0 = {
    node_status_reached(N, polished)
  }.

node_status(N, polished) :- node_status_reached(N, polished).

% You can't skip node status steps (but you can do several at once):

error(m("Node status skipped 'uninitialized'.", N)) :-
  node_status_reached(N, initialized),
  0 = {
    node_status_reached(N, uninitialized)
  }.

error(m("Node status skipped 'initialized'.", N)) :-
  node_status_reached(N, built),
  0 = {
    node_status_reached(N, initialized)
  }.

error(m("Node status skipped 'built'.", N)) :-
  node_status_reached(N, branched),
  0 = {
    node_status_reached(N, built)
  }.

error(m("Node status skipped 'branched'.", N)) :-
  node_status_reached(N, polished),
  0 = {
    node_status_reached(N, branched)
  }.


% Some sanity checks
% ------------------

% Choice nodes which have neither inherited potential nor a setup cannot reach
% "initialized":

inherited_potential(N) :-
  story_node(N),
  successor(Prev, Opt, N),
  0 < { unresolved_potential(Prev, Opt, Pt) }.

error(m("Built choice has no potential!", N)) :-
  node_status_reached(N, built),
  story_node(N),
  node_type(N, choice),
  0 = {
    setup(N, Setup) : possible_setup(Setup);
    inherited_potential(N)
  }.

% Nodes which have no options cannot reach "built":
optcount(N, X) :-
  story_node(N),
  X = {
    at(N, option(O)) : opt_num(O)
  }.

error(m("Built node has no options!", N)) :-
  node_status_reached(N, built),
  optcount(N, 0).

% Every option of a "branched" node must have a successor, unless it's an
% "ending" node:

error(m("Branched node's option is missing a successor.", N, option(O))) :-
  node_status_reached(N, branched),
  at(N, option(O)),
  0 = {
    successor(N, option(O), X) : story_node(X);
    node_type(N, ending)
  }.

% TODO: sanity checking for "polished" nodes?


% Setup implementation
% --------------------

% A setup is needed at the beginning of the story and after every
% travel_onwards event:
1 = {
  setup(R, Setup) : possible_setup(Setup);
  error(m("Root without setup.", R))
} :-
  story_root(R),
  story_op(R, initialize_node).

1 = {
  setup(N, Setup) : possible_setup(Setup);
  error(m("Post-travel node without setup.", Prev, Opt, N))
} :-
  successor(Prev, Opt, N),
  at(Prev, action(Opt, travel_onwards)),
  story_op(N, initialize_node).


% Node ordering
% -------------

% Transitive beforeness of states given a direct "successor" relation:
before(Prev, New) :-
  story_node(Prev),
  story_node(New),
  successor(Prev, Opt, New).

before(Prev, New) :-
  story_node(Prev),
  story_node(Int),
  story_node(New),
  successor(Prev, Opt, Int),
  before(Int, New).

% Succession involves either an existing node that matches the outcome state of
% the branching node, or a new node.
matching_successor(Br, option(Opt), Succ) :-
  story_node(Succ),
  state_match(Br, option(Opt), Succ),
  not before(Succ, Br),
  story_op(Br, add_branch_nodes).

1 = {
  successor(Br, option(Opt), Succ) :
    matching_successor(Br, option(Opt), Succ);
  successor(Br, option(Opt), @pred(@join_(Br, Opt)));
  error(m("Node without successor.", Br, option(Opt)))
} :-
  story_op(Br, add_branch_nodes),
  at(Br, option(Opt)),
  not node_type(Br, ending).


% New node creation
% -----------------

% New nodes need to be marked as such and filled in:
new_node(@pred(@join_(Br, Opt))) :-
  successor(Br, option(Opt), @pred(@join_(Br, Opt))),
  story_op(Br, add_branch_nodes).

story_node(N) :- new_node(N).

1 = {
  node_type(N, choice);
  node_type(N, event);
  node_type(N, ending);
  error(m("Invalid/missing node type.", N))
} :-
  story_node(N),
  story_op(N, build_options).

path_length(Next, Shortest+1) :-
  successor(Prev, Opt, Next),
  shortest_path(Prev, Shortest).

shortest_path(N, Shortest) :-
  path_length(N, Shortest),
  0 = {
    path_length(N, Shorter) : path_length(N, Shorter), Shorter < Shortest
  }.


% State copying
% -------------

st(New, PSt) :-
  new_node(New),
  successor(Old, Opt, New),
  st(Old, PSt),
  not at(Old, consequence(Opt, _not, PSt)).

st(New, NSt) :-
  new_node(New),
  successor(Old, Opt, New),
  at(Old, consequence(Opt, NSt)).

error(m("New node picked up extra state", New, State)) :-
  new_node(New),
  successor(Old, Opt, New),
  st(New, State),
  0 = {
    st(Old, State);
    at(Old, consequence(Opt, State));
    spontaneous(st(New, State))
  }.

error(m("New node lost a state", New, State)) :-
  new_node(New),
  successor(Old, Opt, New),
  st(Old, State),
  0 = {
    st(New, State);
    at(Old, consequence(Opt, _not, State));
    spontaneous(_not, st(New, State))
  }.

error(m("New node violated negative consequence", New, State)) :-
  new_node(New),
  successor(Old, Opt, New),
  at(Old, consequence(Opt, _not, State)),
  st(New, State).


% State matching
% --------------

% The concept of a "state match":
state_match(Prev, option(Opt), New) :-
  story_op(Prev, add_branch_nodes),
  story_node(New),
  New != Prev,
  at(Prev, option(Opt)),
  not state_mismatch(Prev, option(Opt), New).

% mismatch due to ignored consequence:
state_mismatch(Prev, option(Opt), New) :-
  story_op(Prev, add_branch_nodes),
  story_node(New),
  New != Prev,
  at(Prev, consequence(option(Opt), State)),
  not st(New, State).

% mismatch due to invalidated _not consequence:
state_mismatch(Prev, option(Opt), New) :-
  story_op(Prev, add_branch_nodes),
  at(Prev, option(Opt)),
  story_node(New),
  New != Prev,
  at(Prev, consequence(option(Opt), _not, State)),
  st(New, State).

% mismatch due to uncaused state addition:
state_mismatch(Prev, option(Opt), New) :-
  story_op(Prev, add_branch_nodes),
  at(Prev, option(Opt)),
  story_node(New),
  New != Prev,
  st(New, State),
  0 = {
    st(Prev, State);
    at(Prev, consequence(option(Opt), State));
    spontaneous(st(New, State))
  }.

% mismatch due to uncaused state dissapearance:
state_mismatch(Prev, option(Opt), New) :-
  story_op(Prev, add_branch_nodes),
  at(Prev, option(Opt)),
  story_node(New),
  New != Prev,
  st(Prev, State),
  0 = {
    st(New, State);
    at(Prev, consequence(option(Opt), _not, State));
    spontaneous(_not, st(New, State))
  }.

% insurance:
error(m("Option has state mismatch with successor.", Old, Opt)) :-
  state_mismatch(Old, Opt, New),
  successor(Old, Opt, New).
