% val.lp
% Valorization of events.

% Actions are valorized:

story(Story, val(Val, Action, Target)) :-
  story(Story, Action),
  story(Story, arg(Action, type, Atype)),
  val(Atype, Val, a(Arg)),
  story(Story, arg(Action, Arg, Target)).
