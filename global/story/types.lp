% types.lp
% Types of things.

% The basic story components:
type(chr). % characters
type(itm). % items
type(loc). % locations
type(evt). % events
type(flt). % fluents

% Most components can have only a few instances. This chokes off certain
% grounding issues (and also makes sense from a storytelling perspecitve):
possible_id(chr, 1..7).
possible_id(itm, 1..7).
possible_id(loc, 1..7).

% Fluents and events need to be much more numerous:
possible_id(evt, T) :- time(T).
possible_id(flt, T) :- time(T).
