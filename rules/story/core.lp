% core.lp
% Core story rules.

% Errors are not allowed.
:- error(M).

% Default story elements:

% There is a single assumed reader:
id(reader, 0).

% There is a single "plot" (used for holding flags and other plot information):
id(plot, 0).
