% args.lp
% Rules for default task arguments and results:

1 = {
  arg_value(Arg, Default);
  arg_value(Arg, Value) : mem(@mkmem(args, Arg), Value)
  } :-
  argument(Arg, Default).

local_mem(@mkmem(result, Slot), Value) :- result(Slot, Value).
