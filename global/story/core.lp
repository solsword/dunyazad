% core.lp
% Core story predicates.

% There are two stories: the current story and the proposed story. Many rules
% operate over both, but some are specific to one or the other, or even ferry
% propositions and constraints between them.
story(current).
story(proposed).

% Errors shall be minimized:
:- error(Message).
%#minimize { 1,error(Message) : error(Message) }.
