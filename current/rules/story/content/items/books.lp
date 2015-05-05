subclass(item, book).

%item_def(holy_book, book, "holy book", singular).
%teaches_skill(holy_book, prayer).
%tool_for(holy_book, prayer).

item_def(plants_book, book, "book of herbal lore", singular).
teaches_skill(plants_book, wilderness_lore).
tool_for(plants_book, wilderness_lore).
teaches_skill(plants_book, healing).
tool_for(plants_book, healing).

%item_def(animals_book, book, "guide to wild beasts", singular).
%teaches_skill(animals_book, wilderness_lore).
%tool_for(animals_book, wilderness_lore).

item_def(speech_book, book, "book of speeches", singular).
teaches_skill(speech_book, elocution).

item_def(music_book, book, "music book", singular).
teaches_skill(music_book, music).

item_def(medicine_book, book, "book of medicine", singular).
teaches_skill(medicine_book, healing).
tool_for(medicine_book, healing).

item_def(legends_book, book, "book of legends", singular).
teaches_skill(legends_book, storytelling).
tool_for(legends_book, storytelling).
