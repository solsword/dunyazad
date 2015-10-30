error(m("Party not simplified.")) :-
  party_size(PS),
  PS != 1.

% We want to ensure that we use diverse setups when the program is run multiple
% times:
#minimize {
  P@20, setup(root, Setup) :
    setup(root, Setup),
    setup_priority(Setup, P)
}.

% We want diverse action selection given a diverse setup. Note: This also winds
% up optimizing for choices with as few options as possible. Other constraints
% should be added if more than 2 options are desired.
#minimize {
  P@10, at(root, action(option(O), Action)) :
    at(root, action(option(O), Action)),
    action_priority(Action, Base),
    P = Base + 100
}.


at(N, text_mode(example)) :- story_node(N).
