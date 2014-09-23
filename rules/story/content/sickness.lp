% sickness.lp
% Disease and its treatement.

% Fluents:
% --------

status(chr, sickly). % prone to illness
default_status(sickly, _not(sickly)).
negative(status(sickly)).

status(chr, sick). % a short-term possibly life-threatening illness
default_status(sick, _not(sick)).
negative(status(sick)).

status(chr, afflicted). % a long term non-life-threatening affliction
default_status(afflicted, _not(afflicted)).
ally_goal(status(afflicted), status(_not(afflicted))).
negative(status(afflicted)).

brel(itm, chr, is_medicine_for). % An item can be medicine for an illness
default_rel_value(is_medicine_for, false).

brel(itm, chr, is_cure_for). % An item can be the cure for an affliction
default_rel_value(is_cure_for, false).

brel(chr, chr, knows_treatement_for). % Knows how to treat an affliction
default_rel_value(knows_treatement_for, false).

% Actions:
% --------

action(fall_ill).
argument(fall_ill, subject, chr).
precondition(fall_ill, fl(status(_not(sick)), a(subject))).
causes(fall_ill, status(sick), a(subject)).

action(recover_from_illness).
argument(recover_from_illness, subject, chr).
precondition(recover_from_illness, fl(status(sick), a(subject))).
causes(recover_from_illness, status(_not(sick)), a(subject)).

action(become_afflicted).
argument(become_afflicted, subject, chr).
precondition(become_afflicted, fl(status(_not(afflicted)), a(subject))).
causes(become_afflicted, status(afflicted), a(subject)).

action(treat_illness).
argument(treat_illness, subject, chr).
argument(treat_illness, object, chr).
argument(treat_illness, item, itm).
precondition(treat_illness, fl(rel(is_medicine_for, true), a(item, object))).
precondition(treat_illness, fl(rel(is_carried_by, true), a(item, subject))).
causes(treat_illness, status(_not(sick)), a(object)).
causes(treat_illness, rel(is_carried_by, false), a(item, subject)).
causes(treat_illness, status(destroyed), a(item)).

action(treat_affliction).
argument(treat_affliction, subject, chr).
argument(treat_affliction, object, chr).
argument(treat_affliction, item, itm).
precondition(treat_affliction, fl(rel(is_cure_for, true), a(item, object))).
precondition(treat_affliction, fl(rel(is_carried_by, true), a(item, subject))).
causes(treat_affliction, status(_not(afflicted)), a(object)).
causes(treat_affliction, rel(is_carried_by, false), a(item, subject)).
causes(treat_affliction, status(destroyed), a(item)).
