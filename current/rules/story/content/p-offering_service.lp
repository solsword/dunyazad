% offering_service
potential(opportunity, property, offering_service, Action) :-
  action(Action).
initiated_by(opportunity, property, offering_service, Action, inst) :-
  action(Action).
% TODO: auto-cancel

error(m("Offered service without required skill.", N)) :-
  st(N, property(offering_service, Offering, Service)),
  at(
    N,
    skill_link(Skill, required, Tool, Service, SInitArg, SOutcome)
  ),
  default_intent(Service, SOutcome),
  initiator(Service, SInitArg),
  not st(N, property(has_skill, Offering, Skill)),
  story_op(N, build_options).

error(m("Offered service without promoting skill.", N)) :-
  st(N, property(offering_service, Offering, Service)),
  at(
    N,
    skill_link(Skill, promotes, Tool, Service, SInitArg, SOutcome)
  ),
  default_intent(Service, SOutcome),
  initiator(Service, SInitArg),
  not st(N, property(has_skill, Offering, Skill)),
  story_op(N, build_options).

error(m("Offered service without contest skill.", N)) :-
  st(N, property(offering_service, Offering, Service)),
  at(N,
    skill_link(
      Skill, contest, Tool,
      Service,
      between(SInitArg, Opponent),
      either(SOutcome, OtherOutcome)
    )
  ),
  default_intent(Service, SOutcome),
  initiator(Service, SInitArg),
  not st(N, property(has_skill, Offering, Skill)),
  story_op(N, build_options).

error(m("Offered service without contest skill.", N)) :-
  st(N, property(offering_service, Offering, Service)),
  at(
    N,
    skill_link(
      Skill, contest, Tool,
      Service,
      between(Opponent, SInitArg),
      either(OtherOutcome, SOutcome)
    )
  ),
  default_intent(Service, SOutcome),
  initiator(Service, SInitArg),
  not st(N, property(has_skill, Offering, Skill)),
  story_op(N, build_options).

error(m("Offered service without required tool.", N)) :-
  st(N, property(offering_service, Offering, Service)),
  at(
    N,
    skill_link(Skill, required, tool, Service, SInitArg, SOutcome)
  ),
  default_intent(Service, SOutcome),
  initiator(Service, SInitArg),
  not at(N, has_tool_for(Offering, Skill)),
  story_op(N, build_options).

error(m("Offered service without promoting tool.", N)) :-
  st(N, property(offering_service, Offering, Service)),
  at(
    N,
    skill_link(Skill, promotes, tool, Service, SInitArg, SOutcome)
  ),
  default_intent(Service, SOutcome),
  initiator(Service, SInitArg),
  not at(N, has_tool_for(Offering, Skill)),
  story_op(N, build_options).

error(m("Offered service without contest tool.", N)) :-
  st(N, property(offering_service, Offering, Service)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Service,
      between(SInitArg, Opponent),
      either(SOutcome, OtherOutcome)
    )
  ),
  default_intent(Service, SOutcome),
  initiator(Service, SInitArg),
  not at(N, has_tool_for(Offering, Skill)),
  story_op(N, build_options).

error(m("Offered service without contest tool.", N)) :-
  st(N, property(offering_service, Offering, Service)),
  at(
    N,
    skill_link(
      Skill, contest, tool,
      Service,
      between(Opponent, SInitArg),
      either(OtherOutcome, SOutcome)
    )
  ),
  default_intent(Service, SOutcome),
  initiator(Service, SInitArg),
  not at(N, has_tool_for(Offering, Skill)),
  story_op(N, build_options).
