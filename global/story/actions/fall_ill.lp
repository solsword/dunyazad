action(fall_ill).
precondition(fall_ill, not_state(subject, sick)).
consequence(fall_ill, state(subject, sick)).
