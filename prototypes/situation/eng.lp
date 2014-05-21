% utility functions:

#script (python)
def join(*args):
  return ''.join(str(a) for a in args)
def js(*args):
  return ' '.join(str(a) for a in args)
#end.

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
verb_for(rob, "robbed").
verb_for(fall_for, "fell for").
verb_for(confess_to, "confessed to").
verb_for(borrow_money_from, "borrowed money from").
verb_for(repay_loan, "repayed a loan from").
verb_for(give_wealth, "gave money to").

% full text for a timestep:

#show full_text/2.

full_text(T, @join(Sentence, Changes)) :-
  sentence(T, Sentence),
  change_text(T, Changes).

% sentence construction:

#show sentence/2.

sentence(T, @join(@js(Subject, Verb, Object), ".")) :-
  eng_subject(T, Subject),
  eng_verb(T, Verb),
  eng_object(T, Object).

%#show eng_subject/2.
%#show eng_verb/2.
%#show eng_object/2.

eng_subject(T, Subject) :-
  argument(T, subject, S),
  name(S, Subject).

eng_verb(T, Verb) :-
  event(T, Act),
  verb_for(Act, Verb).

eng_object(T, Object) :-
  argument(T, object, O),
  name(O, Object).

eng_object(T, "") :-
  time(T),
  0 = { argument(T, object, O) }.

% change text construction

%change_text(T, @jand(
#show new_state_text/2.

new_state_text(T, @js(Subject, "was no longer", Status)) :-
  changed(T, status(id(Type, S), Status)),
  status(Type, Status),
  name(id(Type, S), Subject).

new_state_text(T, @js(Subject, "became", Status)) :-
  changed(T, status(S, _not(Status))),
  name(S, Subject).

new_state_text(T, @js(@join(Subject, "'s"), State, "became", New)) :-
  changed(T, state(S, State)),
  at(T+1, state(S, State, New)),
  name(S, Subject),
  State != wealth.

new_state_text(T, @js(Subject, "became wealthy")) :-
  changed(T, state(S, wealth)),
  at(T+1, state(S, wealth, has_wealth)),
  name(S, Subject).

new_state_text(T, @js(Subject, "became penniless")) :-
  changed(T, state(S, wealth)),
  at(T+1, state(S, wealth, needs_money)),
  name(S, Subject).

new_state_text(T, @js(Subject, "was financially stable again")) :-
  changed(T, state(S, wealth)),
  at(T+1, state(S, wealth, normal)),
  name(S, Subject).

new_state_text(T, @js(Subject, "and", Object, "became", New)) :-
  changed(T, rel(S, O, relationship)),
  at(T+1, rel(S, O, relationship, New)),
  at(T+1, rel(O, S, relationship, New)),
  O < S,
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "no longer cared about", Object)) :-
  changed(T, rel(S, O, feelings)),
  at(T+1, rel(S, O, feelings, ambivalent)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to dislike", Object)) :-
  changed(T, rel(S, O, feelings)),
  at(T+1, rel(S, O, feelings, dislikes)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to like", Object)) :-
  changed(T, rel(S, O, feelings)),
  at(T+1, rel(S, O, feelings, likes)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to hate", Object)) :-
  changed(T, rel(S, O, feelings)),
  at(T+1, rel(S, O, feelings, hates)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to love", Object)) :-
  changed(T, rel(S, O, feelings)),
  at(T+1, rel(S, O, feelings, loves)),
  name(S, Subject),
  name(O, Object).
