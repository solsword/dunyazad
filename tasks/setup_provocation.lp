% External predicates:
% From global/interest.lp:
%   provocation(_)

{ l_selected(P) : provocation(P) } = 1.

l_selected(outrage).

spawn_task( 1, "provoke_outrage" ) :- l_selected(outrage).
spawn_task( 1, "provoke_sympathy" ) :- l_selected(sympathy).
spawn_task( 1, "provoke_curiosity" ) :- l_selected(curiosity).
