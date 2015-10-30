max_unique(0).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).

% This script helps assign random priorities to each possible setup and action
% that should be much less stable than orderings based on ASP constraints.
#script (python)
import random
import gringo

# TODO: seed this!

def random_priority(cat, ignored, setup_count, shuffled={}, indexes={}):
  if cat not in shuffled:
    shuffled[cat] = [x + 1 for x in range(setup_count)]
    indexes[cat] = [-1]
    random.shuffle(shuffled[cat])
  indexes[cat][0] += 1
  if indexes[cat][0] < len(shuffled[cat]):
    return shuffled[cat][indexes[cat][0]]
  else:
    # this shouldn't happen
    return shuffled[cat][-1]
#end.

setup_count(N) :- N = { possible_setup(Setup) }.

setup_priority(Setup, @random_priority(setup, Setup, N)) :-
  possible_setup(Setup),
  setup_count(N).

action_count(N) :- N = { action(Action) }.

action_priority(Action, @random_priority(action, Action, N)) :-
  action(Action),
  action_count(N).


%:- action(Action), 0 = { action_priority(Action, N) }.
%:- action(Action), { action_priority(Action, N) } >= 2.
%
%1 >= { action_priority(Action, 100) } :- action(Action).
%
%action_priority(attack, 1).
%action_priority(polymorph, 1).
%action_priority(pacify, 1).
