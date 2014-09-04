% sickness.lp
% Disease and its treatement.

% Fluents:
% --------

status(chr, sickly). % prone to illness
status_default(sickly, _not(sickly)).
negative(sickly).

status(chr, sick). % a short-term possibly life-threatening illness
status_default(sick, _not(sick)).
ally_goal(status(sick), status(_not(sick))).
negative(sick).

status(chr, afflicted). % a long term non-life-threatening affliction
status_default(afflicted, _not(afflicted)).
ally_goal(status(afflicted), status(_not(afflicted))).
negative(afflicted).

brel(itm, chr, is_medicine_for). % An item can be medicine for an illness
default_rel_value(is_medicine_for, false).

brel(itm, chr, is_cure_for). % An item can be the cure for an affliction
default_rel_value(is_cure_for, false).

brel(chr, chr, knows_treatement_for). % Knows how to treat an affliction
default_rel_value(knows_treatement_for, false).

% Actions:
% --------

action(treat_illness).
argument(treat_illness, subject, chr).
argument(treat_illness, object, chr).
argument(treat_illness, item, itm).
precondition(treat_illness, fl(rel(is_medicine_for, true), a(item, object)).
precondition(treat_illness, fl(rel(is_carried_by, true), a(item, subject)).
causes(treat_illness, fl(status(_not(sick)), a(object))).
causes(treat_illness, fl(rel(is_carried_by, false), a(item, subject))).
causes(treat_illness, fl(status(destroyed), a(item))).
