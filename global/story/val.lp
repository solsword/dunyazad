% val.lp
% Valorization of events and blame.

story(Story, val(Val, Evt, Target)) :-
  val(Action, Val, Arg),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)).

story(Story, val(Val, Evt, Target)) :-
  val(Action, Val, Arg),
  story(Story, action(Evt, Action)),
  story(Story, arg(Evt, Arg, Target)).
