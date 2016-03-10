subclass(animal, monster).
powerful(monster).
class_skill(monster, fighting, always).
class_skill(monster, monstrous, always).
class_skill(monster, wilderness_lore, sometimes).

%actor_def(dragon, monster, "dragon", singular, neuter).
%item_def(dragon_scale, treasure, "dragon scale", singular).
%item_def(dragon_claws, natural_weapon, "claws", plural).
%class_item(dragon, dragon_claws, 1, 1).
%class_item(dragon, dragon_scale, 0, 1).
%trophy(dragon, dragon_scale).

%actor_def(leviathan, monster, "leviathan", singular, neuter).
%item_def(
%  leviathan_ambergris,
%  treasure,
%  "leviathan ambergris",
%  singular
%).
%trophy(leviathan, leviathan_ambergris).

% TODO: "group" number
%actor_def(ripper_pack, monster, "pack of rippers", singular, neuter).

actor_def(ogre, monster, "ogre", singular, either).
item_def(sack_of_gold, treasure, "sack of gold", singular).
%item_def(dragon_claws, natural_weapon, "claws", plural).
%class_item(dragon, dragon_claws, 1, 1).
class_item(ogre, artificial_weapon, 0, 1).
class_item(ogre, sack_of_gold, 0, 1).
trophy(ogre, sack_of_gold).
