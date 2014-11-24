% Nodes statuses
% --------------

% Choice nodes which have no options yet are unfinished:
optcount(N, X) :-
  story_node(N),
  node_type(N, choice),
  X = {
    at(N, option(O)) : opt_num(O)
  }.

node_status(N, unfinished) :- optcount(N, 0).

% Choice nodes which have neither inherited potential nor a setup are
% uninitialized:

inherited_potential(N) :-
  story_node(N),
  successor(Prev, Opt, N),
  0 < { unresolved_potential(Prev, Opt, Pt) }.

node_status(N, uninitialized) :-
  story_node(N),
  node_type(N, choice),
  0 = {
    setup(N, Setup) : possible_setup(Setup);
    inherited_potential(N)
  }.

% Setup implementation
% --------------------

1 = {
  setup(N, Setup) : possible_setup(Setup)
} :-
  not inherited_potential(N),
  node_type(N, choice),
  story_op(N, initialize_node).

st(N, State) :-
  story_op(N, initialize_node),
  s_st(Setup, State),
  setup(N, Setup).

spontaneous(st(N, State)) :-
  story_op(N, X),
  s_st(Setup, State),
  setup(N, Setup).

1 = {
  intro_text(N, Txt) :
    s_intro_text(Setup, Txt)
} :-
  story_op(N, initialize_node),
  setup(N, Setup).


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
1 = {
  successor(Br, option(Opt), Succ) :
    story_node(Succ),
    state_match(Br, option(Opt), Succ),
    not before(Succ, Br);
  successor(Br, option(Opt), @join(Br, ":", Opt))
} :-
  story_op(Br, add_branch_nodes),
  at(Br, option(Opt)).


% New node creation
% -----------------

% New nodes need to be marked as such and filled in:
new_node(@join(Br, ":", Opt)) :-
  successor(Br, option(Opt), @join(Br, ":", Opt)),
  story_op(Br, add_branch_nodes).

story_node(N) :- new_node(N).

1 = {
  node_type(N, choice);
  node_type(N, event)
} :- new_node(N).

st(New, PSt) :-
  new_node(New),
  successor(Old, Opt, New),
  st(Old, PSt),
  0 = {
    at(Old, consequence(Opt, _not, PSt))
  }.

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
    at(Old, consequence(Opt, _not, State))
  }.


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
  st(Prev, State),
  0 = {
    st(New, State);
    at(Prev, consequence(option(Opt), _not, State))
  }.

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
    at(Prev, consequence(option(Opt), _not, State))
  }.

% TODO: setup state dissapearance
