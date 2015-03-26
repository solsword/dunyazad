subclass(animal, monster).
powerful(monster).

actor_def(dragon, monster, "dragon", singular, neuter).
item_def(dragon_scale, treasure, "dragon scale", singular).
item_def(dragon_claws, weapon, "claws", plural).
natural_weapon(dragon, dragon_claws).
trophy(dragon, dragon_scale).

%actor_def(leviathan, monster, "leviathan", singular, neuter).
%item_def(leviathan_ambergris, treasure, "leviathan ambergris", singular).
%trophy(leviathan, leviathan_ambergris).

% TODO: "group" number
%actor_def(ripper_pack, monster, "pack of rippers", singular, neuter).
