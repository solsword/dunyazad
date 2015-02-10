% attack
action(attack).

argument(attack, aggressor, actor).
argument(attack, target, actor).
initiator(attack, aggressor).

outcome(attack, victory).
outcome(attack, defeat).
outcome(attack, parley).


dangerous(attack).
skill_link(attack, fighting).
requires_tool(attack).


% you can't parley if one side of the conflict is unintelligent:
error(m("Parley with unintelligent target.", N, X)) :-
  at(N, outcome(X, parley)),
  at(N, arg(X, target, Target)),
  st(N, state(unintelligent, Target)),
  story_op(N, build_options).

error(m("Parley with unintelligent agressor.", N, X)) :-
  at(N, outcome(X, parley)),
  at(N, arg(X, aggressor, Aggressor)),
  st(N, state(unintelligent, Aggressor)),
  story_op(N, build_options).

% attacking is one way to deal with a threat
at(N, deals_with(X, relation(threatening, Target, Someone))) :-
  at(N, action(X, attack)),
  1= {
    at(N, outcome(X, parley));
    at(N, outcome(X, victory))
  },
  at(N, arg(X, target, Target)),
  st(N, relation(threatening, Target, Someone)),
  story_op(N, build_options).

% if things go badly it serves to emphasize the problem
at(N, emphasizes(X, relation(threatening, Target, Someone))) :-
  at(N, action(X, attack)),
  at(N, outcome(X, defeat)),
  at(N, arg(X, target, Target)),
  st(N, relation(threatening, Target, Someone)),
  story_op(N, build_options).

% fighting is dangerous:
0 <= {
  at(N, consequence(X, state(injured, Aggressor)));
  at(N, consequence(X, state(injured, Target)))
} <= 2 :-
  at(N, action(X, attack)),
  at(N, arg(X, aggressor, Aggressor)),
  at(N, arg(X, target, Target)),
  story_op(N, build_options).

1 = {
  at(N, consequence(X, state(injured, Target)));
  at(N, consequence(X, state(killed, Target)));
  error(m("Victory without consequences.", N, X))
} :-
  at(N, action(X, attack)),
  at(N, arg(X, target, Target)),
  at(N, outcome(X, victory)),
  story_op(N, build_options).

1 = {
  at(N, consequence(X, state(injured, Aggressor)));
  at(N, consequence(X, state(killed, Aggressor)));
  error(m("Defeat without consequences.", N, X))
} :-
  at(N, action(X, attack)),
  at(N, arg(X, aggressor, Aggressor)),
  at(N, outcome(X, defeat)),
  story_op(N, build_options).

% Looting is a possibility:
0 <= {
  at(N, consequence(X, relation(has_item, Aggressor, Item))) :
    st(N, relation(has_item, Target, Item))
} <= 1 :-
  at(N, action(X, attack)),
  at(N, arg(X, aggressor, Aggressor)),
  at(N, arg(X, target, Target)),
  1 = {
    at(N, outcome(X, victory));
    at(N, outcome(X, parley))
  },
  story_op(N, build_options).
