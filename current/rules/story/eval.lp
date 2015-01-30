% eval.lp
% Rules about what's a good and/or bad outcome.

at(N, score(X, 10)) :-
  at(N, deals_with(X, Pt)),
  at(N, potential(PTyp, Pt)),
  story_op(N, build_options).

at(N, score(X, -10)) :-
  at(N, emphasizes(X, Pt)),
  at(N, potential(PTyp, Pt)),
  story_op(N, build_options).

at(N, score(X, 3)) :-
  at(N, nullifies(X, Prb)),
  at(N, potential(problem, Prb)),
  story_op(N, build_options).

at(N, score(X, -3)) :-
  at(N, nullifies(X, Opp)),
  at(N, potential(opportunity, Opp)),
  story_op(N, build_options).

at(N, total_score(option(Opt), TS)) :-
  story_node(N),
  at(N, option(Opt)),
  TS = #sum{ S : at(N, score(option(Opt), S)) },
  story_op(N, build_options).
