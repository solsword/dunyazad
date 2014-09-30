% location.lp
% Location of stuff.

% Fluents:
% --------

ebrel(chr, loc, chr_at).

ebrel(itm, loc, itm_at).

ebrel(monster, loc, mnst_at).

% Actions:
% --------

action(chr_move).
argument(chr_move, subject, chr).
argument(chr_move, starting_point, loc).
argument(chr_move, destination, loc).
auto_argument(
  chr_move,
  luggage,
  itm,
  fl(rel(itm_carried_by, true), a(luggage, subject))
).
precondition(chr_move, fl(rel(chr_at, true), a(subject, starting_point))).
causes(chr_move, rel(chr_at, false), a(subject, starting_point)).
causes(chr_move, rel(itm_at, false), a(luggage, starting_point)).
causes(chr_move, rel(chr_at, true), a(subject, destination)).
causes(chr_move, rel(itm_at, true), a(luggage, destination)).

action(mnst_move).
argument(mnst_move, subject, monster).
argument(mnst_move, starting_point, loc).
argument(mnst_move, destination, loc).
precondition(mnst_move, fl(rel(mnst_at, true), a(subject, starting_point))).
causes(mnst_move, rel(chr_at, false), a(subject, starting_point)).
causes(mnst_move, rel(chr_at, true), a(subject, destination)).
