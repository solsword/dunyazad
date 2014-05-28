% names:

name_for(chr, "Likke").
name_for(chr, "Runar").
name_for(chr, "Jali").
name_for(chr, "Ystred").
name_for(chr, "Milei").
name_for(chr, "Genu").

1 = {
  name(id(Type, ID), Name) : name_for(Type, Name)
} :-
  id(Type, ID).

:-
  name(One, Name),
  name(Two, Name),
  One != Two.

% verbs:

verb_for(fall_ill, "fell ill").
verb_for(meet, "met").
verb_for(recover, "recovered").
verb_for(buy_medicine_for, "bought medicine for").
verb_for(accident, "had an accident").
verb_for(treat_injury, "treated").
verb_for(gamble, "gambled").
verb_for(rob, "robbed").
verb_for(fall_for, "fell for").
verb_for(confess_to, "confessed to").
verb_for(borrow_money_from, "borrowed money from").
verb_for(repay_loan, "repayed a loan from").
verb_for(give_wealth, "gave money to").

% tenses:
past_tense(ambivalent, "felt ambivalent towards").
past_tense(dislikes, "disliked").
past_tense(hates, "hated").
past_tense(likes, "liked").
past_tense(loves, "loved").
