subclass(person, commoner).
powerless(commoner).

% General commoners
actor_def(innkeeper, commoner, "innkeeper", singular, either).
class_skill(innkeeper, literacy, sometimes).
class_skill(innkeeper, storytelling, sometimes).

% Businesspeople
subclass(commoner, businessperson).
class_skill(businessperson, literacy, always).

actor_def(merchant, businessperson, "merchant", singular, either).
class_skill(merchant, tinkering, sometimes).
class_skill(merchant, music, sometimes).
class_skill(merchant, elocution, sometimes).
class_skill(merchant, storytelling, sometimes).
class_item(merchant, treasure, 0, 1).
class_item(merchant, book, 0, 1).
class_item(merchant, artificial_weapon, 1, 1).

% Skilled workers
subclass(commoner, skilled).
class_skill(skilled, literacy, sometimes).

actor_def(healer, skilled, "healer", singular, either).
class_skill(healer, healing, always).
class_item(healer, medicine_chest, 1, 1).


% Laborers
subclass(commoner, laborer).
class_skill(laborer, storytelling, sometimes).

actor_def(peasant, laborer, "peasant", singular, either).
class_skill(peasant, wilderness_lore, sometimes).
