% setup:
% ------

inst(actor, the_party).

inst(item, supplies).
has_item(inst(actor, the_party), inst(item, supplies)).

inst(party_member, "Arel").
inst(party_member, "Jain").

setup(being_robbed).

% roc attack
inst(actor, roc) :- setup(roc_attack).
unintelligent(inst(actor, roc)) :- setup(roc_attack).

inst(item, roc_feather) :- setup(roc_attack).

trophy(inst(actor, roc), inst(item, roc_feather)) :- setup(roc_attack).

problem(threatening(inst(actor, roc), inst(actor, the_party))) :-
  setup(roc_attack).


% merchants being robbed
inst(item, treasure).

has_item(inst(actor, the_party), inst(item, treasure)).

inst(actor, robbers) :- setup(being_robbed).
inst(actor, merchants) :- setup(being_robbed).

problem(threatening(inst(actor, robbers), inst(actor, merchants))) :-
  setup(being_robbed).
