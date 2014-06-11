% Portrayal

portrayed_as(T, Subj, Portrayal) :-
  event(T, Act),
  portrayal(Act, Arg, Portrayal),
  argument(T, Arg, Subj).

portrayed_as(T, Subj, Portrayal) :-
  event(T, Act),
  outcome(T, Outcome),
  portrayal(Act, Outcome, Arg, Portrayal),
  argument(T, Arg, Subj).

% Portrayal based on motivation:
portrayed_as(T, Subj, good) :-
  motivated(T, charity),
  responsible_for(T, Subj).

portrayed_as(T, Subj, good) :-
  motivated(T, friendship),
  responsible_for(T, Subj).

portrayed_as(T, Subj, bad) :-
  motivated(T, enmity),
  responsible_for(T, Subj).

portrayed_as(T, Subj, good) :-
  motivated(T, gratitude),
  responsible_for(T, Subj).

portrayed_as(T, Subj, bad) :-
  motivated(T, revenge),
  responsible_for(T, Subj).

% Plot outline:

basic_plot(Victim, T1) :-
  time(T1), T1 < 4,
  time(T2), T2 < 6,
  ending(E),
  character(Victim),
  character(Villain),
  Victim != Villain,
  1 <= { portrayed_as(T1, Victim, good); portrayed_as(T1, Victim, victim) },
  1 <= { harmful_to(T2, Victim); causes_problem_for(T2, Victim) },
  responsible_for(T2, Villain),
  0 = { has_problem(E, Victim) },
  1 <= { has_problem(E, Villain) }.

%:- 0 = { basic_plot(Chr, T) : character(Chr), time(T) }.
