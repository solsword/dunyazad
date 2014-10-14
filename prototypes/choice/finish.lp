% full text assembly:
% -------------------

scene_text(
  @join_lines(
    Intro,
    "What do you do?",
    "*choice",
    Options
  )
) :-
  intro_text(Intro),
  combined_options(Options).

error(m("Option without option text:", option(X))) :- 
  option(X),
  0 = {
    option_text(option(X), Text)
  }.

error(m("Option without action text:", option(X))) :- 
  option(X),
  0 = {
    action_text(option(X), Text)
  }.

% option combination:
% -------------------

num_options(N) :-
  N = {
    option_text(X, OptTxt) : option_text(X, OptTxt)
  }.

combined_options(
  @join_lines(
    @join("  #", OptTxt),
    @join("    ", OutTxt),
    "    *finish"
  )
) :-
  num_options(1),
  option_text(X, OptTxt),
  outcome_text(X, OutTxt).

combined_options(
  @join_lines(
    @join("  #", Opt1Txt),
    @join("    ", Out1Txt),
    "    *finish",
    @join("  #", Opt2Txt),
    @join("    ", Out2Txt),
    "    *finish"
  )
) :-
  num_options(2),
  option_text(X1, Opt1Txt),
  outcome_text(X1, Out1Txt),
  option_text(X2, Opt2Txt),
  outcome_text(X2, Out2Txt),
  X1 < X2.

combined_options(
  @join_lines(
    @join("  #", Opt1Txt),
    @join("    ", Out1Txt),
    "    *finish",
    @join("  #", Opt2Txt),
    @join("    ", Out2Txt),
    "    *finish",
    @join("  #", Opt3Txt),
    @join("    ", Out3Txt),
    "    *finish"
  )
) :-
  num_options(3),
  option_text(X1, Opt1Txt),
  outcome_text(X1, Out1Txt),
  option_text(X2, Opt2Txt),
  outcome_text(X2, Out2Txt),
  option_text(X3, Opt3Txt),
  outcome_text(X3, Out3Txt),
  X1 < X2,
  X1 < X3,
  X2 < X3.

combined_options(
  @join_lines(
    @join("  #", Opt1Txt),
    @join("    ", Out1Txt),
    "    *finish",
    @join("  #", Opt2Txt),
    @join("    ", Out2Txt),
    "    *finish",
    @join("  #", Opt3Txt),
    @join("    ", Out3Txt),
    "    *finish",
    @join("  #", Opt4Txt),
    @join("    ", Out4Txt),
    "    *finish"
  )
) :-
  num_options(4),
  option_text(X1, Opt1Txt),
  outcome_text(X1, Out1Txt),
  option_text(X2, Opt2Txt),
  outcome_text(X2, Out2Txt),
  option_text(X3, Opt3Txt),
  outcome_text(X3, Out3Txt),
  option_text(X4, Opt4Txt),
  outcome_text(X4, Out4Txt),
  X1 < X2,
  X1 < X3,
  X1 < X4,
  X2 < X3,
  X2 < X4,
  X3 < X4.

combined_options(
  @join_lines(
    @join("  #", Opt1Txt),
    @join("    ", Out1Txt),
    "    *finish",
    @join("  #", Opt2Txt),
    @join("    ", Out2Txt),
    "    *finish",
    @join("  #", Opt3Txt),
    @join("    ", Out3Txt),
    "    *finish",
    @join("  #", Opt4Txt),
    @join("    ", Out4Txt),
    "    *finish",
    @join("  #", Opt5Txt),
    @join("    ", Out5Txt),
    "    *finish"
  )
) :-
  num_options(5),
  option_text(X1, Opt1Txt),
  outcome_text(X1, Out1Txt),
  option_text(X2, Opt2Txt),
  outcome_text(X2, Out2Txt),
  option_text(X3, Opt3Txt),
  outcome_text(X3, Out3Txt),
  option_text(X4, Opt4Txt),
  outcome_text(X4, Out4Txt),
  option_text(X5, Opt5Txt),
  outcome_text(X5, Out5Txt),
  X1 < X2,
  X1 < X3,
  X1 < X4,
  X1 < X5,
  X2 < X3,
  X2 < X4,
  X2 < X5,
  X3 < X4,
  X3 < X5,
  X4 < X5.
