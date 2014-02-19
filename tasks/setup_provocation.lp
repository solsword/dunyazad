% External predicates:
% From global/interest.lp:
%   provocation(_)

1 { l_protagonist(id(chr, C)) : character(C) } 1.
1 { l_antagonist(id(chr, C)) : character(C) } 1.
1 { l_victim(id(chr, C)) : character(C) } 1.

:- l_protagonist(C), l_antagonist(C).
:- l_protagonist(C), l_victim(C).
:- l_antagonist(C), l_victim(C).

1 { selected(prov(P)) : provocation(P) } 1.

spawn_task( 1, "add_scene" ).

task_arg( 1, "type", "injustice" ) :- selected(prov(outrage)).
task_arg( 1, "subject", Chr ) :- l_chr_A(Chr), selected(prov(outrage)).
task_arg( 1, "object", Chr ) :- l_chr_B(Chr), selected(prov(outrage)).

task_arg( 1, "type", "misfortune" ) :- selected(prov(sympathy)).
task_arg( 1, "subject", Chr ) :- l_chr_A(Chr), selected(prov(sympathy)).

task_arg( 1, "type", "mystery" ) :- selected(prov(curiosity)).
task_arg( 1, "subject", Chr ) :- l_chr_A(Chr), selected(prov(curiosity)).
