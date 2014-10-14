% setup:
% ------

inst(actor, the_party).
name(inst(actor, the_party), "party").
number(inst(actor, the_party), plural).
gender(inst(actor, the_party), neuter).

inst(item, supplies).
name(inst(item, supplies), "supplies").
number(inst(item, supplies), plural).
gender(inst(item, supplies), neuter).
has_item(inst(actor, the_party), inst(item, supplies)).

inst(party_member, "Arel").
name(inst(party_member, "Arel"), "Arel").
number(inst(party_member, "Arel"), singular).
gender(inst(party_member, "Arel"), feminine).
inst(party_member, "Jain").
name(inst(party_member, "Jain"), "Jain").
number(inst(party_member, "Jain"), singular).
gender(inst(party_member, "Jain"), masculine).

travel_mode(walking).

environment(hills).

1 = {
  setup(being_robbed);
  setup(roc_attack)
}.

% roc attack
inst(actor, roc) :- setup(roc_attack).
name(inst(actor, roc), "roc").
number(inst(actor, roc), singular).
gender(inst(actor, roc), neuter).
unintelligent(inst(actor, roc)) :- setup(roc_attack).

inst(item, roc_feather) :- setup(roc_attack).
name(inst(item, roc_feather), "roc feather").
number(inst(item, roc_feather), singular).
gender(inst(item, roc_feather), neuter).

trophy(inst(actor, roc), inst(item, roc_feather)) :- setup(roc_attack).

problem(threatening(inst(actor, roc), inst(actor, the_party))) :-
  setup(roc_attack).

intro_text(
  @join("You ", Travel, ", when suddenly, a giant bird swoops down from the skies, clearly intent on attacking your party!")
) :-
  travel_text(Travel),
  setup(roc_attack).


% merchants being robbed
inst(item, treasure).
name(inst(item, treasure), "treasure").
number(inst(item, treasure), singular).
gender(inst(item, treasure), neuter).

has_item(inst(actor, the_party), inst(item, treasure)).

inst(actor, bandits) :- setup(being_robbed).
name(inst(actor, bandits), "bandits").
number(inst(actor, bandits), plural).
gender(inst(actor, bandits), neuter).

inst(actor, merchants) :- setup(being_robbed).
name(inst(actor, merchants), "merchants").
number(inst(actor, merchants), plural).
gender(inst(actor, merchants), neuter).

problem(threatening(inst(actor, bandits), inst(actor, merchants))) :-
  setup(being_robbed).

intro_text(
  @join("You ", Travel, " and come across a small band of merchants being attacked by a group of bandits.")
) :-
  travel_text(Travel),
  setup(being_robbed).
