max_unique(0).
story_root(root).
story_node(root).
node_status_reached(root, uninitialized).
path_length(root, 0).
node_type(root, choice).

% This script helps assign a random priority to each possible setup that should
% be much less stable than orderings based on ASP constraints.
#script (python)
import random
import gringo

# TODO: seed this!

def setup_priority(ignored, setup_count, shuffled=[], index=[-1]):
  if shuffled == []:
    shuffled[:] = [x + 1 for x in range(setup_count)]
    random.shuffle(shuffled)
  index[0] += 1
  if index[0] < len(shuffled):
    return shuffled[index[0]]
  else:
    # this shouldn't happen
    return shuffled[-1]
#end.

setup_count(N) :-
  N = {
    possible_setup(Setup)
  }.

setup_priority(Setup, @setup_priority(Setup, N)) :-
  possible_setup(Setup),
  setup_count(N).


%setup_ordering_number(1..N) :- setup_count(N).
%1 = {
%  setup_priority(Setup, N) : setup_ordering_number(N)
%} :-
%  possible_setup(Setup).
%:- setup_priority(S1, N), setup_priority(S2, N), S1 != S2.
