subclass(person, aristocrat).
powerful(aristocrat).
class_skill(aristocrat, literacy, always).
class_skill(aristocrat, elocution, sometimes).
class_skill(aristocrat, music, sometimes).
class_item(aristocrat, treasure, 0, 1).
class_item(aristocrat, book, 0, 1).

actor_def(noble, aristocrat, "noble", singular, either).
