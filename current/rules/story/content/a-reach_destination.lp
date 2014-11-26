% reach_destination: a special party-only action that ends the story
action(reach_destination).

argument(reach_destination, subject, actor).

outcome(reach_destination, onwards).

% The argument of reach_destination is fixed:

at(N, arg(Opt, subject, inst(actor, the_party))) :-
  at(N, action(Opt, reach_destination)).

% reach_destination cannot happen except at an ending:

error(m("Reached destination before end of story", N, X)) :-
  at(N, action(X, reach_destination)),
  not node_type(N, ending).

% reach_destination nullifies all existing potentials:

at(N, nullifies(X, potential(Any, Pt))) :-
  at(N, action(X, reach_destination)),
  st(N, potential(Any, Pt)),
  story_op(N, build_options).

% reach_destination doesn't need to have any consequences as it can only happen
% at the end of a story.


% Surface details
% ---------------

option_text(
  N,
  X,
  @fmt(
    "N#{}/they V#reach/prs/{} N#{}/their destination.",
    Subj, Subj
  )
) :-
  at(N, action(X, travel_onwards)),
  at(N, arg(X, subject, inst(T1, Subj))),
  story_op(N, add_surface).

action_text(
  N,
  X,
  @fmt(
    "N#{}/they finally V#reach/prs/{} N#{}/their destination. All is well.",
    Subj, Subj
  )
) :-
  at(N, action(X, travel_onwards)),
  at(N, arg(X, subject, inst(T1, Subj))),
  story_op(N, add_surface).
