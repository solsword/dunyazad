% utility functions:

#script (python)
def join(*args):
  return ''.join(str(a) for a in args)
def js(*args):
  return ' '.join(str(a) for a in args)
#end.

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

% change text construction:

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
  changed(T, state(S, wealth, Old)),
  at(T+1, state(S, wealth, has_wealth)),
  name(S, Subject).

new_state_text(T, @js(Subject, "became penniless")) :-
  changed(T, state(S, wealth, Old)),
  at(T+1, state(S, wealth, needs_money)),
  name(S, Subject).

new_state_text(T, @js(Subject, "was financially stable again")) :-
  changed(T, state(S, wealth, Old)),
  at(T+1, state(S, wealth, normal)),
  name(S, Subject).

new_state_text(T, @js(Subject, "and", Object, "became", New)) :-
  changed(T, rel(S, O, relationship, Old)),
  at(T+1, rel(S, O, relationship, New)),
  at(T+1, rel(O, S, relationship, New)),
  O < S,
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "no longer cared about", Object)) :-
  changed(T, rel(S, O, feelings, Old)),
  at(T+1, rel(S, O, feelings, ambivalent)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to dislike", Object)) :-
  changed(T, rel(S, O, feelings, Old)),
  at(T+1, rel(S, O, feelings, dislikes)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to like", Object)) :-
  changed(T, rel(S, O, feelings, Old)),
  at(T+1, rel(S, O, feelings, likes)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to hate", Object)) :-
  changed(T, rel(S, O, feelings, Old)),
  at(T+1, rel(S, O, feelings, hates)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "came to love", Object)) :-
  changed(T, rel(S, O, feelings, Old)),
  at(T+1, rel(S, O, feelings, loves)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "repaid their debt to", Object)) :-
  changed(T, rel(S, O, debt, Old)),
  at(T+1, rel(S, O, debt, no_debt_to)),
  name(S, Subject),
  name(O, Object).

new_state_text(T, @js(Subject, "owed money to", Object)) :-
  changed(T, rel(S, O, debt, Old)),
  at(T+1, rel(S, O, debt, owes_money_to)),
  name(S, Subject),
  name(O, Object).

% motive text construction:

#show motive_text/2.

motive_text(T, @js("And then,")) :-
  1 <= { motivated(T, destiny); unmotivated(T) },
  time(T),
  not beginning(T).

motive_text(B, @js("In the beginning,")) :-
  1 <= { motivated(B, destiny); unmotivated(B) },
  beginning(B).


motive_text(T, @js("Looking out for themself,")) :-
  motivated(T, self_interest),
  responsible_for(T, A),
  name(A, Actor).

motive_text(T, @js("Because", Actor, FPast, Victim)) :-
  motivated(T, enmity),
  responsible_for(T, A),
  1 <= { harmful_to(T, V); causes_problem_for(T, V) },
  at(T, rel(A, V, feelings, Feeling)),
  past_tense(Feeling, FPast),
  name(A, Actor),
  name(V, Victim).

motive_text(T, @js("Because", Actor, FPast, Victim)) :-
  motivated(T, friendship),
  responsible_for(T, A),
  1 <= { helpful_to(T, V); solves_problem_for(T, V) },
  at(T, rel(A, V, feelings, Feeling)),
  past_tense(Feeling, FPast),
  name(A, Actor),
  name(V, Victim).

motive_text(T, @js("Because", Subject, "loved", Object)) :-
  motivated(T, love),
  responsible_for(T, S),
  argument(T, object, O),
  name(S, Subject),
  name(O, Object).

motive_text(T, @js("Because", S)) :-
  motivated(T, revenge(P)),
  sentence(P, S).

motive_text(T, @js("Because", S)) :-
  motivated(T, gratitude(P)),
  sentence(P, S).

motive_text(T, @js("Because", Subject, "felt sorry for", Object)) :-
  motivated(T, charity),
  responsible_for(T, S),
  helpful_to(T, O),
  name(S, Subject),
  name(O, Object).
