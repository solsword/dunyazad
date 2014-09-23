% wealth.lp
% Wealth and poverty.

% Fluents:
% --------

state(chr, wealth).
state_value(wealth, poor).
state_value(wealth, average).
state_value(wealth, wealthy).
state_value_order(wealth, poor, average).
state_value_order(wealth, average, wealthy).
default_state_value(wealth, average).
negative(state(wealth, poor)).
positive(state(wealth, wealthy)).
goal_of(motive(greed), state(wealth, wealthy)).
anti_goal_of(motive(greed), state(wealth, poor)).

%status(chr, super_wealthy).
%default_status(super_wealthy, _not(super_wealthy)).
%positive(status(super_wealthy)).
%goal_of(motive(greed), status(super_wealthy)).

% Actions:
% --------

action(find_riches).
argument(find_riches, subject, chr).
causes_relative(find_riches, increase, state(wealth), a(subject)).
