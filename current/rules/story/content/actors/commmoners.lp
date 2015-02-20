subclass(person, commoner).

% General commoners
actor_def(innkeeper, commoner, "innkeeper", singular, either).

% Businesspeople
subclass(commoner, businessperson).

actor_def(merchants, businessperson, "merchants", plural, neuter).
actor_def(traders, businessperson, "traders", plural, neuter).

actor_def(merchant, businessperson, "merchant", singular, either).
actor_def(trader, businessperson, "trader", singular, either).
actor_def(tinker, businessperson, "tinker", singular, either).

% Laborers
subclass(commoner, laborer).

actor_def(peasant, laborer, "peasant", singular, either).
actor_def(farmer, laborer, "farmer", singular, either).
actor_def(serf, laborer, "serf", singular, either).
