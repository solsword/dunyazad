% args.lp
% Rules for default task arguments:

argument(Arg) :- mem(@join("args.", Arg), Value).

task_arg(Arg, Value) :- mem(@join("args.", Arg), Value).
{ task_arg(Arg, Default) } :- arg_default(Arg, Default).

1 = { task_arg(Arg, Value) : task_arg(Arg, Value) } :- argument(Arg).
