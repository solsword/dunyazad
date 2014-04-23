% blame.lp
% Blame and praise for events.

0 <= { story(Story, blames_for(Action, Victim, Scapegoat)) } :-
  story(Story, val(harmful, Action, Victim)),
  story(Story, arg(Action, type, Atype)),
  blame(Atype, a(Arg)),
  story(Story, arg(Action, Arg, Scapegoat)).

0 <= { story(Story, blames_for(Action, Victim, fate)) } :-
  story(Story, val(harmful, Action, Victim)),
  story(Story, arg(Action, type, Atype)),
  blame(Atype, fate).

0 <= { story(Story, praises_for(Action, Victim, Scapegoat)) } :-
  story(Story, val(helpful, Action, Victim)),
  story(Story, arg(Action, type, Atype)),
  blame(Atype, a(Arg)),
  story(Story, arg(Action, Arg, Scapegoat)).

0 <= { story(Story, praises_for(Action, Victim, fate)) } :-
  story(Story, val(helpful, Action, Victim)),
  story(Story, arg(Action, type, Atype)),
  blame(Atype, fate).

% TODO: indirect blame/praise
