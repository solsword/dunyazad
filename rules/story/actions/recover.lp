action(recover).
act_arg(recover, subject, chr).
precondition(recover, state(a(subject), sick)).
consequence(recover, terminates, state(a(subject), sick)).

val(recover, helpful, a(subject)).
blame(recover, fate).
