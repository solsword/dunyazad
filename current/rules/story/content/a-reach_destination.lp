% reach_destination: a special party-only action that ends the story
action(reach_destination).

argument(reach_destination, subject, actor).

outcome(reach_destination, journey_over).

% The argument of reach_destination is fixed:

at(N, arg(Opt, subject, inst(actor, you))) :-
  at(N, action(Opt, reach_destination)).

% reach_destination cannot happen except at an ending:

error(m("Reached destination before end of story", N, X)) :-
  at(N, action(X, reach_destination)),
  not node_type(N, ending).

% reach_destination nullifies all existing potentials:

at(N, nullifies(X, Pt)) :-
  at(N, action(X, reach_destination)),
  at(N, potential(Any, Pt)),
  story_op(N, build_options).

% reach_destination doesn't need to have any consequences as it can only happen
% at the end of a story.
