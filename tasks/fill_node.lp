status(completed).

tn(N) :- mem("args.target_node", N).

active_node(N) :- tn(N).
branch_node(N) :- tn(N).
