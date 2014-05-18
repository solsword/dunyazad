% Things:

id(chr, 1..3).
%id(itm, 1..5).
%id(loc, 1..5).

% Timesteps:

time(1..8).
unmotivated_action_limit(6).

%event(1, fall_ill).
%event(1, rob).
%event(2, rob).
%event(3, rob).

beginning(1).
ending(2).

% Events, outcomes, and arguments:

1 = {
  event(T, Act) : action(Act)
} :-
  time(T), time(T+1).

1 = {
  outcome(T, Outcome) : causes(Act, Outcome, Result)
} :-
  event(T, Act).

1 = {
  argument(T, Arg, id(Type, Target)) : id(Type, Target)
} :-
  event(T, Act),
  arg(Act, Arg, Type).

% Mimimize errors:

#minimize { 1@0,error(Message) : error(Message) }.
