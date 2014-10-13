% surface.lp
% surface realization

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

% travel text possibilities:
% --------------------------

{
  travel_text("round a bend in the road");
  travel_text("walk for a ways")
} :- travel_mode(walking).

{
  travel_text("crest a hill");
  travel_text("enter a shallow valley");
  travel_text("cross a crude bridge over a stream")
} :- environment(hills).

% option combination:
% -------------------

test(N) :-
  N = {
    option_text(X, OptTxt) : option_text(X, OptTxt)
  }.

combined_options(
  @join_lines(
    @join("#", OptTxt),
    OutTxt,
    "*finish"
  )
) :-
  1 = {
    option_text(X, OptTxt) : option_text(X, OptTxt)
  },
  option_text(X, OptTxt),
  outcome_text(X, OutTxt).

combined_options(
  @join_lines(
    @join("#", Opt1Txt),
    Out1Txt,
    "*finish",
    @join("#", Opt2Txt),
    Out2Txt,
    "*finish"
  )
) :-
  2 = {
    option_text(X, OptTxt) : option_text(X, OptTxt)
  },
  option_text(X1, Opt1Txt),
  outcome_text(X1, Out1Txt),
  option_text(X2, Opt2Txt),
  outcome_text(X2, Out2Txt).

combined_options(
  @join_lines(
    @join("#", Opt1Txt),
    Out1Txt,
    "*finish",
    @join("#", Opt2Txt),
    Out2Txt,
    "*finish",
    @join("#", Opt3Txt),
    Out3Txt,
    "*finish"
  )
) :-
  3 = {
    option_text(X, OptTxt) : option_text(X, OptTxt)
  },
  option_text(X1, Opt1Txt),
  outcome_text(X1, Out1Txt),
  option_text(X2, Opt2Txt),
  outcome_text(X2, Out2Txt),
  option_text(X3, Opt3Txt),
  outcome_text(X3, Out3Txt).

combined_options(
  @join_lines(
    @join("#", Opt1Txt),
    Out1Txt,
    "*finish",
    @join("#", Opt2Txt),
    Out2Txt,
    "*finish",
    @join("#", Opt3Txt),
    Out3Txt,
    "*finish",
    @join("#", Opt4Txt),
    Out4Txt,
    "*finish"
  )
) :-
  4 = {
    option_text(X, OptTxt) : option_text(X, OptTxt)
  },
  option_text(X1, Opt1Txt),
  outcome_text(X1, Out1Txt),
  option_text(X2, Opt2Txt),
  outcome_text(X2, Out2Txt),
  option_text(X3, Opt3Txt),
  outcome_text(X3, Out3Txt),
  option_text(X4, Opt4Txt),
  outcome_text(X4, Out4Txt).

combined_options(
  @join_lines(
    @join("#", Opt1Txt),
    Out1Txt,
    "*finish",
    @join("#", Opt2Txt),
    Out2Txt,
    "*finish",
    @join("#", Opt3Txt),
    Out3Txt,
    "*finish",
    @join("#", Opt4Txt),
    Out4Txt,
    "*finish",
    @join("#", Opt5Txt),
    Out5Txt,
    "*finish"
  )
) :-
  5 = {
    option_text(X, OptTxt) : option_text(X, OptTxt)
  },
  option_text(X1, Opt1Txt),
  outcome_text(X1, Out1Txt),
  option_text(X2, Opt2Txt),
  outcome_text(X2, Out2Txt),
  option_text(X3, Opt3Txt),
  outcome_text(X3, Out3Txt),
  option_text(X4, Opt4Txt),
  outcome_text(X4, Out4Txt),
  option_text(X5, Opt5Txt),
  outcome_text(X5, Out5Txt).

% outcome text:
% -------------

% TODO: Real outcome text!
outcome_text(X, Atext) :-
  action_text(X, Atext).
