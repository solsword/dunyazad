% a healer
% TODO: Merge this into a larger setup?

possible_setup(healer).

setup_argument_create(healer, doctor, healer).
setup_argument_create(healer, supplies, medicine_chest).

s_st(healer, relation(c(has_item), v(doctor), v(supplies))).

s_st(
  healer,
  property(c(offering_service), v(doctor), c(treat_injury))
).

error(m("Used healer setup without any patients", N)) :-
  setup(N, healer),
  0 = {
    st(N, state(injured, Inst))
  }.
