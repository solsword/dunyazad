subclass(person, bad_guy).
powerful(bad_guy).

subclass(bad_guy, stealer).
class_skill(stealer, acrobatics, sometimes).
class_skill(stealer, thievery, always).

subclass(bad_guy, tough).
class_skill(tough, fighting, always).
class_item(tough, artificial_weapon, 0, 1).


actor_def(bandits, tough, "bandits", plural, neuter).

actor_def(thief, stealer, "thief", singular, either).
