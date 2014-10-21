% attack
action(attack).

argument(attack, aggressor, actor).
argument(attack, target, actor).
initiator(attack, aggressor).

outcome(attack, victory).
outcome(attack, defeat).
outcome(attack, parley).

% you can't parley if one side of the conflict is unintelligent:
:-
  outcome(X, parley),
  arg(X, target, Target),
  unintelligent(Target).

:-
  outcome(X, parley),
  arg(X, aggressor, Aggressor),
  unintelligent(Aggressor).

% attacking is one way to deal with a threat
deals_with(X, threatening(Target, Someone)) :-
  action(X, attack),
  1= {
    outcome(X, parley);
    outcome(X, victory)
  },
  arg(X, target, Target),
  problem(threatening(Target, Someone)).

% fighting is dangerous:
0 <= {
  consequence(X, injured(Aggressor));
  consequence(X, injured(Target))
} <= 2 :-
  action(X, attack),
  arg(X, aggressor, Aggressor),
  arg(X, target, Target).

1 = {
  consequence(X, injured(Aggressor));
  consequence(X, killed(Aggressor))
} :-
  action(X, attack),
  arg(X, aggressor, Aggressor),
  outcome(X, defeat).

1 = {
  consequence(X, injured(Target));
  consequence(X, killed(Target))
} :-
  action(X, attack),
  arg(X, target, Target),
  outcome(X, victory).

0 <= {
  consequence(X, obtains(Aggressor, Item))
} <= 1 :-
  action(X, attack),
  arg(X, aggressor, Aggressor),
  arg(X, target, Target),
  trophy(Target, Item),
  1 = {
    outcome(X, victory);
    outcome(X, parley)
  }.
