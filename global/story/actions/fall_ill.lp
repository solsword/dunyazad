action(fall_ill).
precondition(fall_ill, not_state(a(subject), sick)).
consequence(fall_ill, initiates, state(a(subject), sick)).

val(fall_ill, harmful, a(subject)).
blame(fall_ill, fate).
