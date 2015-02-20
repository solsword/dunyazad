subclass(person, bad_guy).
subclass(bad_guy, stealer).
subclass(bad_guy, tough).

actor_def(bandits, tough, "bandits", plural, neuter).
actor_def(thugs, tough, "thugs", plural, neuter).
actor_def(raiders, tough, "raiders", plural, neuter).

actor_def(bandit, tough, "bandit", singular, either).
actor_def(thug, tough, "thug", singular, either).

actor_def(thief, stealer, "thief", singular, either).
actor_def(robber, stealer, "robber", singular, either).
