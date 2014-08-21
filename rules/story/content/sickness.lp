status(chr, sickly). % prone to illness
negative(sickly).

status(chr, sick). % a short-term possibly life-threatening illness
ally_goal(status(sick), status(_not(sick))).
negative(sick).

status(chr, afflicted). % a long term non-life-threatening affliction
ally_goal(status(afflicted), status(_not(afflicted))).
negative(afflicted).

rel(itm, chr, is_medicine_for). % An item can be medicine for an illness

rel(itm, chr, is_cure_for). % An item can be the cure for an affliction

rel(chr, chr, knows_treatement_for). % Knows how to treat an affliction
