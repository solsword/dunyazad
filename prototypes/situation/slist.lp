status(chr, sick).
default(chr, _not(sick)).
negative(chr, sick).


status(chr, injured).
default(chr, _not(injured)).
negative(chr, injured).


status(chr, heartbroken).
default(chr, _not(heartbroken)).
negative(chr, heartbroken).


state(chr, wealth).
value(wealth, needs_money).
value(wealth, normal).
value(wealth, has_wealth).
value(wealth, super_rich).

default(chr, wealth, normal).
default(chr, wealth, super_rich).
outstanding(chr, wealth, super_rich).
negative(chr, wealth, needs_money).
positive(chr, wealth, has_wealth).
positive(chr, wealth, super_rich).

ordered(wealth, needs_money, normal).
ordered(wealth, normal, has_wealth).

ordered(wealth, super_rich, super_rich).


relation(chr, chr, feelings).
value(feelings, ambivalent).
value(feelings, dislikes).
value(feelings, hates).
value(feelings, likes).
value(feelings, loves).

category(chr, chr, feelings, dislikes, negative).
category(chr, chr, feelings, hates, negative).
category(chr, chr, feelings, likes, positive).
category(chr, chr, feelings, loves, positive).
nonreflexive(chr, chr, feelings).
default(chr, chr, feelings, ambivalent).

ordered(feelings, hates, dislikes).
ordered(feelings, dislikes, ambivalent).
ordered(feelings, ambivalent, likes).
ordered(feelings, likes, loves).


relation(chr, chr, relationship).
value(relationship, strangers).
value(relationship, acquaintances).
value(relationship, enemies).
value(relationship, friends).
value(relationship, lovers).
value(relationship, family).

nonreflexive(chr, chr, relationship).
reciprocal(chr, chr, relationship).
default(chr, chr, relationship, strangers).
default(chr, chr, relationship, acquaintances).
outstanding(chr, chr, relationship, acquaintances).
default(chr, chr, relationship, friends).
outstanding(chr, chr, relationship, friends).
default(chr, chr, relationship, enemies).
outstanding(chr, chr, relationship, enemies).


relation(chr, chr, debt).
value(debt, owes_money_to).
value(debt, no_debt_to).

nonreflexive(chr, chr, debt).
default(chr, chr, debt, no_debt_to).
negative(chr, chr, debt, owes_money_to).
