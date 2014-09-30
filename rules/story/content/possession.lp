% posession.lp
% Items and inventories.

% Fluents:
% --------

status(itm, destroyed).
default_status(destroyed, _not(destroyed)).

ebrel(itm, chr, itm_carried_by).

ebrel(itm, chr, itm_owned_by).

% Actions:
% --------

action(give_item).
argument(give_item, subject, chr).
argument(give_item, object, chr).
argument(give_item, item, itm).
precondition(give_item, fl(rel(itm_carried_by, true), a(item, subject))).
precondition(give_item, fl(colocated, a(subject, object))).
causes(give_item, rel(itm_carried_by, true), a(item, object)).
causes(give_item, rel(itm_carried_by, false), a(item, subject)).

action(bestow_item).
argument(bestow_item, subject, chr).
argument(bestow_item, object, chr).
argument(bestow_item, item, itm).
precondition(bestow_item, fl(rel(itm_owned_by, true), a(item, subject))).
causes(bestow_item, rel(itm_owned_by, true), a(item, object)).
causes(bestow_item, rel(itm_owned_by, false), a(item, subject)).

action(destroy_item).
argument(destroy_item, subject, chr).
argument(destroy_item, item, itm).
precondition(destroy_item, fl(status(_not(destroyed)), a(item))).
causes(destroy_item, status(destroyed), a(item)).
