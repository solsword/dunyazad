subclass(item, instrument).

%item_def(harp, instrument, "harp", singular).
%item_def(mandolin, instrument, "mandolin", singular).
%item_def(flute, instrument, "flute", singular).
item_def(oboe, instrument, "oboe", singular).
%item_def(trumpet, instrument, "trumpet", singular).
%item_def(accordian, instrument, "accordian", singular).

tool_for(IType, music) :- item_def(IType, instrument, Name, Number).
