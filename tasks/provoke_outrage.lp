1 { l_protagonist(id(chr, C)) : character(C) } 1.
1 { l_antagonist(id(chr, C)) : character(C) } 1.

:- l_protagonist(C), l_antagonist(C).

spawn_task( 1, "add_scene" ).

task_arg( 1, "type", "condemn" ).
task_arg( 1, "actor", Chr ) :- l_antagonist(Chr).
task_arg( 1, "acted_upon", Chr ) :- l_protagonist(Chr).
