error(m("Root choice has wrong option structure.")) :-
  node_status_reached(root, built),
  not at(root, option_structure(negative_alternatives)).

error(m("Root choice option has wrong outcome feel.", X)) :-
  at(root, option(X)),
  not at(root, outcome_feel(option(X), miracle)).

error(m("Root choice doesn't have three options.")) :-
  node_status_reached(root, built),
  optcount(root, N),
  N != 3.

error(m("Root choice doesn't have high stakes.")) :-
  node_status_reached(root, built),
  not at(root, perceived_stakes(high)).

error(m("Root option stakes are inconsistent.")) :-
  node_status_reached(root, built),
  at(root, perceived_option_stakes(A, SomeLevel)),
  at(root, perceived_option_stakes(B, SomeOtherLevel)),
  SomeLevel != SomeOtherLevel.

%error(m(x)) :- st(root, relation(threatening, Anyone, Victim)).
%
%errror(m(y)) :- 0 = { st(root, relation(accusing, Someone, Anyone)) }.

%st(root, relation(testtest, inst(actor, you), inst(actor, you))).
%some_accusation(N) :- st(N, relation(accusing, Someone, Anyone)).


%error(m("action", 1)) :-
%  node_status_reached(root, built),
%  not at(root, action(option(1), deny_blame)).
%
%error(m("action", 2)) :-
%  node_status_reached(root, built),
%  not at(root, action(option(2), talk_down)).
%
%error(m("action", 3)) :-
%  node_status_reached(root, built),
%  not at(root, action(option(3), shift_blame)).
%
%bad(m("opt feel", X)) :-
%  at(root, option(X)),
%  0 = {
%    at(root, option_feel(option(X), longshot));
%    at(root, option_feel(option(X), bad));
%    at(root, option_feel(option(X), doomed))
%  }.
%
%bad(m("predictability", X)) :-
%  at(root, option(X)),
%  0 = {
%    at(root, overall_predictability(option(X), unexpected));
%    at(root, overall_predictability(option(X), unfair))
%  }.
%
%bad(m("outcome", X)) :-
%  at(root, option(X)),
%  0 = {
%    at(root, outcome_overall(option(X), good));
%    at(root, outcome_overall(option(X), great))
%  }.
%
%#minimize {
%  1@100,bad(M) : bad(M)
%}.
