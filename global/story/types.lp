% types.lp
% Types of things.

% The basic story components:
type(evt).
type(chr).
type(itm).
type(loc).

% Each component can have only a few instances. This chokes off certain
% grounding issues (and also makes sense from a storytelling perspecitve):
id(1..7).

% Events are a special case:
evt_id(T) :- time(T).
