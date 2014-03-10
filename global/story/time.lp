% transitive afterness

after(id(evt, Before), id(evt, After)) :-
  id(evt, Before),
  id(evt, After),
  link(id(evt, Before), LinkType, id(evt, After)),
  temporally_forward(LinkType).

after(id(evt, Before), id(evt, After)) :-
  id(evt, Before),
  id(evt, Between),
  id(evt, After),
  after(id(evt, Before), id(evt, Between)),
  after(id(evt, Between), id(evt, After)).

