% flee
action(flee).

chaotic(attack).

% arguments

argument(flee, fearful, actor).
argument(flee, from, actor).
initiator(flee, fearful).
default_intent(flee, o(success, escape)).

% outcomes

outcome_val(flee, success, escape).
outcome_val(flee, success, failure).

outcome_val(flee, get_injured, injured).
outcome_val(flee, get_injured, safe).

% skills

at(
  N,
  skill_link(acrobatics, promotes, no_tool, flee, fearful, o(success, escape))
) :-
  1 <= {
    setting(N, city);
    setting(N, town)
  },
  story_node(N).

at(
  N,
  skill_link(
    wilderness_lore, promotes, no_tool,
    flee, fearful,
    o(success, escape)
  )
) :-
  1 <= {
    setting(N, road);
    setting(N, wilderness)
  },
  story_node(N).

% TODO: Put the fleeing actor and their items off-stage!

% Fleeing gets rid of threats
at(
  N,
  consequence_of(
    X,
    o(success, escape),
    _not,
    relation(threatening, From, Fearful)
  )
) :-
  at(N, action(X, flee)),
  at(N, arg(X, fearful, Fearful)),
  at(N, arg(X, from, From)),
  st(N, relation(threatening, From, Fearful)),
  story_op(N, build_options).

% You can be injured if you fail to escape:
at(N, consequence_of(X, o(get_injured, injured), state(injured, Fearful))) :-
  at(N, action(X, flee)),
  at(N, arg(X, fearful, Fearful)),
  story_op(N, build_options).
% TODO: something about the attacker's intent?
