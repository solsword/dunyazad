% surface.lp
% surface realization

% travel text possibilities:
% --------------------------

1 = {
  travel_text(T) : travel_text(T)
}.

{
  travel_text("round a bend in the road");
  travel_text("walk for a ways")
} :- travel_mode(walking).

{
  travel_text("crest a hill");
  travel_text("enter a shallow valley");
  travel_text("cross a crude bridge over a stream")
} :- environment(hills).

% outcome text:
% -------------

% TODO: Real outcome text!
outcome_text(X, Atext) :-
  action_text(X, Atext).
