subclass(person, commoner).

% General commoners
actor_def(innkeeper, commoner, "innkeeper", singular, either).

% Businesspeople
subclass(commoner, businessperson).

actor_def(merchant, businessperson, "merchant", singular, either).

% Laborers
subclass(commoner, laborer).

actor_def(peasant, laborer, "peasant", singular, either).
