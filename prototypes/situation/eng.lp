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
verb_for(treat_disease, "treated").
verb_for(accident, "had an accident").
verb_for(treat_injury, "treated").
verb_for(rob, "robbed").
verb_for(fall_for, "fell for").
verb_for(confess_to, "confessed to").

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
  changed(T, status, id(Type, S), Status),
  status(Type, Status),
  name(id(Type, S), Subject).

new_state_text(T, @js(Subject, "became", Status)) :-
  changed(T, status, S, _not(Status)),
  name(S, Subject).

new_state_text(T, @js(@join(Subject, "'s"), State, "became", New)) :-
  changed(T, state, S, State),
  at(T+1, state, S, State, New),
  name(S, Subject),
  State != wealth, State != status.

new_state_text(T, @js(Subject, "became as wealthy as a", New)) :-
  changed(T, state, S, wealth),
  at(T+1, state, S, wealth, New),
  name(S, Subject).

new_state_text(T, @js(Subject, "became a", New)) :-
  changed(T, state, S, status),
  at(T+1, state, S, status, New),
  name(S, Subject).

new_state_text(T, @js(Subject, "and", Object, "became", New)) :-
  changed(T, rel, S, relationship, O),
  at(T+1, rel, S, relationship, New, O),
  at(T+1, rel, O, relationship, New, S),
  O < S,
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "no longer cared about", Object)) :-
  changed(T, rel, S, feelings, O),
  at(T+1, rel, S, feelings, ambivalent, O),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to like", Object)) :-
  changed(T, rel, S, feelings, O),
  at(T+1, rel, S, feelings, likes, O),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to hate", Object)) :-
  changed(T, rel, S, feelings, O),
  at(T+1, rel, S, feelings, hates, O),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to love", Object)) :-
  changed(T, rel, S, feelings, O),
  at(T+1, rel, S, feelings, loves, O),
  name(S, Subject),
  name(O, Object).
