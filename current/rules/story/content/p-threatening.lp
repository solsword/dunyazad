% threatening
potential(problem, relation, threatening).
pcategory(potential(problem, relation, threatening), urgent).
pcategory(potential(problem, relation, threatening), immediate).
initiated_by(problem, relation, threatening, from).
problematic_for(problem, relation, threatening, to).

at(N, moivated(X)) :-
  at(N, action(X, attack)),
  at(N, arg(X, aggressor, Aggressor)),
  at(N, arg(X, target, Target)),
  st(N, relation(threatening, Aggressor, Target)),
  story_op(N, build_options).
