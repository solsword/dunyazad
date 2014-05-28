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

time(1..10).
unmotivated_action_limit(1).
destiny_action_limit(3).
selfish_action_limit(3).
unmet_expectation_limit(1).
boredom_limit(2).

%event(1, fall_ill).
%event(1, rob).
%argument(1, subject, id(chr, 1)).
%argument(1, object, id(chr, 2)).
%at(1,rel(id(chr,1),id(chr,2),relationship,strangers)).
%at(1,rel(id(chr,2),id(chr,1),relationship,strangers)).
%event(2, rob).
%event(3, rob).

beginning(1).
ending(10).

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

% Disallow errors and minimize undesireable stuff:

:- error(Message).
%#minimize { 1@0,error(Message) : error(Message) }.

#minimize { X@1,undesirable(X, Message) : undesirable(X, Message) }.
