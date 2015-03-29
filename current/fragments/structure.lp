% The overall story structure is as follows: in the first part of the story,
% each vignette is either a chance to help or a chance to rest. Once three
% chances to help and at least one chance to rest have happened, there's a
% transition vignette which inevitably inflicts the player with a persistent
% problem, followed by a comeuppance vignette where the player is rewarded or
% punished for their behavior when offered a chance to help.

1 = {
  structural_purpose(N, chance_to_help);
  structural_purpose(N, chance_to_rest);
  structural_purpose(N, transition);
  structural_purpose(N, comeuppance)
} :-
  story_node(N),
  vignette(N, N).

% All nodes in a vignette share the same structural purpose. Note that this can
% result in a non-initial node with multiple structural purposes if a branch
% into a vignette is added after the vignette is completed.
structural_purpose(N, SP) :-
  vignette(N, V),
  structural_purpose(V, SP).

% Structural purposes put limits on allowed choice structures:

error(m("Choice strucutre not allowed by structural constraint", N)) :-
  structural_purpose(N, Purpose),
  story_op(N, build_options),
  at(N, choice_structure(NotAllowed)),
  not allowed_choice_structure(Purpose, NotAllowed),
  not allows_all_choice_structures(Purpose).

% Some constraints on structural purpose:

error(m("Too many chances to help", N)) :-
  structural_purpose(N, chance_to_help),
  vignette(N, N),
  story_op(N, build_options),
  3 >= {
    before(B, N) : vignette(B, B), structural_purpose(B, chance_to_help)
  }.

vignette_before(Prev, Next) :-
  before(Prev, Next),
  vignette(Prev, Prev),
  vignette(Next, Next).

vignette_successor(Prev, Next) :-
  vignette_before(Prev, Next),
  0 = {
    before(Next, Between) : vignette(Between, Between), before(Prev, Between)
  }.

error(m("Repeated chance to rest", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, chance_to_rest),
  vignette_successor(Prev, N),
  structural_purpose(Prev, chance_to_rest).

error(m("Successive chances to help", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, chance_to_help),
  vignette_successor(Prev, N),
  structural_purpose(Prev, chance_to_help),
  vignette_successor(PPrev, Prev),
  structural_purpose(PPrev, chance_to_help).

error(m("Missed transition timing", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  3 = {
    vignette_before(X, N) : structural_purpose(X, chance_to_help)
  },
  not structural_purpose(N, chance_to_rest),
  not structural_purpose(N, transition).

error(m("Multiple transition", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, transition),
  vignette_before(X, N),
  structural_purpose(X, transition).

error(m("Skipped transition", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, comeuppance),
  0 = {
    vignette_before(X, N) : structural_purpose(X, transition)
  }.

error(m("Multiple comeuppance", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, comeuppance),
  vignette_before(X, N),
  structural_purpose(X, comeuppance).

% After the comeuppance, the story ends:
error(m("Traveled past comeuppance.", N)) :-
  at(N, action(X, travel_onwards)),
  story_op(N, build_options),
  structural_purpose(N, comeuppance).

error(m("Reached destination before comeuppance.", N)) :-
  at(N, action(X, reach_destination)),
  story_op(N, build_options),
  not structural_purpose(N, comeuppance).

% Structural purpose grounding:

% chance_to_help
% --------------

% Chances to help begin with a spontaneous powerless third party in peril:
chance_to_help(N, inst(T, K), Pt) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, chance_to_help),
  spontaneous(st(N, inst(T, K))),
  is_instance(N, inst(T, K), Category),
  powerless(Category),
  not st(N, state(party_member, inst(T, K))),
  at(N, problematic_for(Pt, inst(T, K))).

% If you solve someone else's problem, you've helped them out:
helped_out(N, Inst, potential(PType, Problem)) :-
  story_op(N, build_options),
  is_instance(N, Inst, Category),
  powerless(Category),
  at(N, problematic_for(potential(PType, Problem), Inst)),
  not st(N, state(party_member, Inst)),
  at(N, initiator(X, Helper)),
  st(N, state(party_member, Helper)),
  at(N, consequence(X, _not, Problem)).

% If you leave someone behind without helping them, you failed to help them:
didnt_help(Later, Orig, inst(T, K), potential(PType, Problem)) :-
  story_op(Later, build_options),
  chance_to_help(Orig, inst(T, K), potential(PType, Problem)),
  successor(Prev, Opt, Later),
  st(Prev, inst(T, K)),
  not st(Later, inst(T, K)),
  0 = {
    helped_out(When, inst(T, K), potential(PType, Problem))
      : before(When, Later)
  }.

% A "chance to help" vignette better live up to its name:
error(m("No chance to help", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, chance_to_help),
  0 = { chance_to_help(N, Inst, Problem) }.

% There should also be an option when the chance to help arises that allows you
% to help, and an option that doesn't involve helping:
error(m("No option to help", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, chance_to_help),
  0 = {
    at(N, option(X))
      : chance_to_help(N, inst(T, K), potential(PType, Problem)),
        at(N, consequence(option(X), _not, Problem))
  }.

error(m("No option not to help", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, chance_to_help),
  0 = {
    at(N, option(X))
      : chance_to_help(N, inst(T, K), potential(PType, Problem)),
        not at(N, consequence(option(X), _not, Problem))
  }.

allowed_choice_structure(chance_to_help, mysterious).
allowed_choice_structure(chance_to_help, obvious).
allowed_choice_structure(chance_to_help, relaxed).
allowed_choice_structure(chance_to_help, uncomfortable).
allowed_choice_structure(chance_to_help, multilemma).
allowed_choice_structure(chance_to_help, pressured).
allowed_choice_structure(chance_to_help, dangerous).
%allowed_choice_structure(chance_to_help, bleak).
%allowed_choice_structure(chance_to_help, doomed).

% chance_to_rest
% --------------

% Resting solves persistent problems:

error(m("No option to solve persistent problem", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, chance_to_rest),
  at(N, category(potential(problem, Problem), persistent)),
  0 = {
    at(N, option(X)) : at(N, consequence(option(X), _not, Problem))
  }.

% Chances to rest shouldn't introduce new problems:

error(m("Chance to rest introduced a problem", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, chance_to_rest),
  successor(Prev, X, N),
  at(N, potential(problem, Problem)),
  not at(Prev, potential(problem, Problem)).

% During resting vignettes, choice structures shouldn't be too gloomy:

allowed_choice_structure(chance_to_rest, mysterious).
allowed_choice_structure(chance_to_rest, obvious).
allowed_choice_structure(chance_to_rest, relaxed).
%allowed_choice_structure(chance_to_rest, uncomfortable).
allowed_choice_structure(chance_to_rest, multilemma).
%allowed_choice_structure(chance_to_rest, pressured).
%allowed_choice_structure(chance_to_rest, dangerous).
%allowed_choice_structure(chance_to_rest, bleak).
%allowed_choice_structure(chance_to_rest, doomed).

% transition
% ----------

error(m("Transition doesn't create persistent problem", N)) :-
  vignette(N, N),
  story_op(N, build_options),
  structural_purpose(N, transition),
  0 = {
    at(N, consequence(X, state(PState, Member)))
      : pcategory(potential(PTyp, state, PState), persistent),
        problematic_for(PTyp, state, PState, inst),
        st(N, state(party_member, Member));
    at(N, consequence(X, property(PProp, Member, PVal)))
      : pcategory(potential(PTyp, property, PProp, PVal), persistent),
        problematic_for(PTyp, property, PProp, PVal, inst),
        st(N, state(party_member, Member));
    at(N, consequence(X, relation(PRel, Member, To)))
      : pcategory(potential(PTyp, relation, PRel), persistent),
        problematic_for(PTyp, relation, PRel, from),
        st(N, state(party_member, Member));
    at(N, consequence(X, relation(PRel, From, Member)))
      : pcategory(potential(PTyp, relation, PRel), persistent),
        problematic_for(PTyp, relation, PRel, to),
        st(N, state(party_member, Member))
  }.

error(m("Transition node solves persistent problem", N)) :-
  story_op(N, build_options),
  structures(N, transition),
  st(N, state(party_member, Member)),
  at(N, problematic_for(potential(PTyp, Problem), Member)),
  at(N, category(potential(PTyp, Problem), persistent)),
  at(N, consequence(X, _not, Problem)).

% Transition vignettes have tense choice structures

allowed_choice_structure(transition, mysterious).
%allowed_choice_structure(transition, obvious).
%allowed_choice_structure(transition, relaxed).
allowed_choice_structure(transition, uncomfortable).
allowed_choice_structure(transition, multilemma).
allowed_choice_structure(transition, pressured).
allowed_choice_structure(transition, dangerous).
allowed_choice_structure(transition, bleak).
allowed_choice_structure(transition, doomed).

% comeuppance
% -----------

% Comeuppance vignettes can have any choice structure

allows_all_choice_structures(comeuppance).

% TODO: comeuppance structure constraints
