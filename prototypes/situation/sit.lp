% Things:

% debug mode

% IDEA: scale characters by using pre-decided relationship constraints (e.g.,
% supporting characters don't need relationships to other supporting
% characters).
id(chr, 1..3).
%id(itm, 1..5).
%id(loc, 1..5).

% Let there be love:

%:- 0 = {
%    at(T, rel(id(chr, A), id(chr, B), feelings, loves)) :
%      time(T), id(chr, A), id(chr, B)
%  }.
%error(m("Not feeling it.")) :-
%  0 = {
%    at(T, rel(id(chr, A), id(chr, B), feelings, loves)) :
%      time(T), id(chr, A), id(chr, B)
%  }.

% Timesteps:

time(1..12).
unmotivated_action_limit(3).
destiny_action_limit(7).
unmet_expectation_limit(3).
boredom_limit(5).

%event(1, fall_ill).
%event(1, rob).
%event(2, rob).
%event(3, rob).

beginning(1).
ending(8).

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

%:- error(Message).

% Put #const debug=0/1 and use it as an extra prerequisite for errors
