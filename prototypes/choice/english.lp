% verbs

verb(attack).
verb(fight).
verb(beat).
verb(offer).
verb(find).
verb(agree).
verb(flee).
verb(fail).

1 = {
  conjugation(Verb, Tense, Number, Person, Form)
    : conjugation(Verb, Tense, Number, Person, Form)
} :-
  verb(Verb),
  tense(Tense),
  number(Number),
  person(Person).

% TODO: more tenses?
tense(present).

number(singular).
number(plural).

person(first).
person(second).
person(third).

% default conjugation
{ conjugation(Verb, present, singular, first, Verb) } :- verb(Verb).
{ conjugation(Verb, present, singular, second, Verb) } :- verb(Verb).
{ conjugation(Verb, present, singular, third, @join(Verb, "s")) } :- verb(Verb).

{ conjugation(Verb, present, plural, first, Verb) } :- verb(Verb).
{ conjugation(Verb, present, plural, second, Verb) } :- verb(Verb).
{ conjugation(Verb, present, plural, third, Verb) } :- verb(Verb).

% TODO: more conjugations?

% definite phrases
definite(inst(actor, X)) :- inst(actor, X).
definite(inst(item, X)) :- inst(item, X).

determined(inst(actor, X)) :- inst(actor, X).
determined(inst(item, X)) :- inst(item, X).

undetermined(inst(party_member, X)) :- inst(party_member, X).

det(inst(Type, Token), Name) :-
  undetermined(inst(Type, Token)),
  name(inst(Type, Token), Name).

det(inst(Type, Token), @the(Name)) :-
  determined(inst(Type, Token)),
  definite(inst(Type, Token)),
  name(inst(Type, Token), Name).

det(inst(Type, Token), @an(Name)) :-
  determined(inst(Type, Token)),
  indefinite(inst(Type, Token)),
  name(inst(Type, Token), Name).

% pronouns
pro(Inst, Person, Case, Position, Pro) :-
  number(Inst, Number),
  gender(Inst, Gender),
  pronoun(Person, Number, Gender, Case, Position, Pro).

gender(masculine).
gender(feminine).
gender(neuter).

case(subjective).
case(objective).
case(possessive).

position(modifier).
position(object).

pronoun(first, singular, G, subjective, P, "I") :- gender(G), position(P).
pronoun(first, singular, G, objective, P, "me") :- gender(G), position(P).
pronoun(first, singular, G, possessive, modifier, "my") :- gender(G).
pronoun(first, singular, G, possessive, object, "mine") :- gender(G).

pronoun(first, plural, G, subjective, P, "we") :- gender(G), position(P).
pronoun(first, plural, G, objective, P, "us") :- gender(G), position(P).
pronoun(first, plural, G, possessive, modifier, "our") :- gender(G).
pronoun(first, plural, G, possessive, object, "ours") :- gender(G).


pronoun(second, singular, G, subjective, P, "you") :- gender(G), position(P).
pronoun(second, singular, G, objective, P, "you") :- gender(G), position(P).
pronoun(second, singular, G, possessive, modifier, "your") :- gender(G).
pronoun(second, singular, G, possessive, object, "yours") :- gender(G).

pronoun(second, plural, G, subjective, P, "you") :- gender(G), position(P).
pronoun(second, plural, G, objective, P, "you") :- gender(G), position(P).
pronoun(second, plural, G, possessive, modifier, "your") :- gender(G).
pronoun(second, plural, G, possessive, object, "yours") :- gender(G).


pronoun(third, singular, masculine, subjective, P, "he") :- position(P).
pronoun(third, singular, masculine, objective, P, "him") :- position(P).
pronoun(third, singular, masculine, possessive, modifier, "his").
pronoun(third, singular, masculine, possessive, object, "his").

pronoun(third, singular, feminine, subjective, P, "she") :- position(P).
pronoun(third, singular, feminine, objective, P, "her") :- position(P).
pronoun(third, singular, feminine, possessive, modifier, "her").
pronoun(third, singular, feminine, possessive, object, "hers").

pronoun(third, singular, neuter, subjective, P, "it") :- position(P).
pronoun(third, singular, neuter, objective, P, "it") :- position(P).
pronoun(third, singular, neuter, possessive, modifier, "its").
pronoun(third, singular, neuter, possessive, object, "its").

pronoun(third, plural, G, subjective, P, "they") :- gender(G), position(P).
pronoun(third, plural, G, objective, P, "them") :- gender(G), position(P).
pronoun(third, plural, G, possessive, modifier, "their") :- gender(G).
pronoun(third, plural, G, possessive, object, "theirs") :- gender(G).
