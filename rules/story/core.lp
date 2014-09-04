% core.lp
% Core story rules.

% Errors are not allowed.
:- error(M).

% There is a single assumed reader:
id(reader, 0).

% There is a cast of characters:
id(chr, 0..8).

% There are some key items:
id(itm, 0..10).

% And some locations:
id(loc, 0..20).
