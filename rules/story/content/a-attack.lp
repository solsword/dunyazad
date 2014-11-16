% attack
action(attack).

argument(attack, aggressor, actor).
argument(attack, target, actor).
initiator(attack, aggressor).

outcome(attack, victory).
outcome(attack, defeat).
outcome(attack, parley).

% you can't parley if one side of the conflict is unintelligent:
error(m("Parley with unintelligent target.", N, X)) :-
  at(N, outcome(X, parley)),
  at(N, arg(X, target, Target)),
  st(N, state(unintelligent, Target)),
  active_node(N).

error(m("Parley with unintelligent agressor.", N, X)) :-
  at(N, outcome(X, parley)),
  at(N, arg(X, aggressor, Aggressor)),
  st(N, state(unintelligent, Aggressor)),
  active_node(N).

% attacking is one way to deal with a threat
at(N, deals_with(X, threatening(Target, Someone))) :-
  at(N, action(X, attack)),
  1= {
    at(N, outcome(X, parley));
    at(N, outcome(X, victory))
  },
  at(N, arg(X, target, Target)),
  st(N, problem(threatening(Target, Someone))),
  active_node(N).

% fighting is dangerous:
0 <= {
  at(N, consequence(X, state(injured, Aggressor)));
  at(N, consequence(X, state(injured, Target)))
} <= 2 :-
  at(N, action(X, attack)),
  at(N, arg(X, aggressor, Aggressor)),
  at(N, arg(X, target, Target)),
  active_node(N).

1 = {
  at(N, consequence(X, state(injured, Aggressor)));
  at(N, consequence(X, state(killed, Aggressor)))
} :-
  at(N, action(X, attack)),
  at(N, arg(X, aggressor, Aggressor)),
  at(N, outcome(X, defeat)),
  active_node(N).

1 = {
  at(N, consequence(X, state(injured, Target)));
  at(N, consequence(X, state(killed, Target)))
} :-
  at(N, action(X, attack)),
  at(N, arg(X, target, Target)),
  at(N, outcome(X, victory)),
  active_node(N).

% TODO: SOMETHING ABOUT THIS?!?
%at(N, consequence(X, state(injured, Target))) :-
%  at(N, action(X, attack)),
%  at(N, arg(X, target, Target)),
%  at(N, outcome(X, victory)),
%  active_node(N).
%
%at(root, consequence(X, state(injured, Target))) :-
%  at(root, action(X, attack)),
%  at(root, arg(X, target, Target)).
%
%% Forcings:
%at(N, consequence(X, state(injured, Target))) :-
%  test_abcd(N, X, Target).
%at(root,outcome(X, victory)) :-
%  at(root, action(X, attack)).
%
%% Tests:
%test_abcd(N, X, Target) :-
%  at(N, action(X, attack)),
%  at(N, arg(X, target, Target)),
%  at(N, outcome(X, victory)),
%  active_node(N).
%
%test_abc(N, X, Target) :-
%  at(N, arg(X, target, Target)),
%  at(N, outcome(X, victory)),
%  active_node(N).
%
%test_ab(N, X) :-
%  at(N, outcome(X, victory)),
%  active_node(N).
%
%test_d(N, X) :-
%  at(N, action(X, attack)).
%
%test_c(N, X, Target) :-
%  at(N, arg(X, target, Target)).
%
%test_b(N, X) :-
%  at(N, outcome(X, victory)).
%
%test_a(N) :-
%  active_node(N).

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
  active_node(N).
