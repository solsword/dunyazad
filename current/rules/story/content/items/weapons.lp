subclass(item, weapon).

item_def(sword, weapon, "sword", singular).
item_def(spear, weapon, "spear", singular).
item_def(javelin, weapon, "javelin", singular).
item_def(dagger, weapon, "daggers", plural).
item_def(axe, weapon, "axe", singular).
item_def(mace, weapon, "mace", singular).
item_def(glaive, weapon, "glaive", singular).
item_def(bow, weapon, "bow", singular).
item_def(rapier, weapon, "rapier", singular).

tool_for(IType, fighting) :- item_def(IType, weapon, Name, Number).
