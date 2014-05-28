% Action definitions.

action(fall_ill).
arg(fall_ill, subject, chr).
causes(fall_ill, success, status(subject, sick)).
precondition_not(fall_ill, status(subject, sick)).
harmful(fall_ill, subject).
responsible(fall_ill, fate).

action(recover).
arg(recover, subject, chr).
causes(recover, success, status(subject, _not(sick))).
precondition(recover, status(subject, sick)).
helpful(recover, subject).
responsible(recover, fate).

action(buy_medicine_for).
arg(buy_medicine_for, subject, chr).
arg(buy_medicine_for, object, chr).
causes(buy_medicine_for, success, status(object, _not(sick))).
causes(buy_medicine_for, success, state_decrease(subject, wealth)).
initiates_acquaintanceship(buy_medicine_for).
causes_maybe(
  buy_medicine_for,
  success,
  rel(object, subject, feelings, likes)
).
precondition(buy_medicine_for, status(object, sick)).
helpful(buy_medicine_for, object).
responsible(buy_medicine_for, subject).

action(accident).
arg(accident, subject, chr).
causes(accident, success, status(subject, injured)).
precondition_not(accident, status(subject, injured)).
harmful(accident, subject).
responsible(accident, fate).

action(treat_injury).
arg(treat_injury, subject, chr).
arg(treat_injury, object, chr).
causes(treat_injury, success, status(object, _not(injured))).
initiates_acquaintanceship(treat_injury).
causes_maybe(
  treat_injury,
  success,
  rel(object, subject, feelings, likes)
).
precondition(treat_injury, status(object, injured)).
helpful(treat_injury, object).
responsible(treat_injury, subject).
nonreflexive(treat_injury).

action(gamble).
arg(gamble, subject, chr).
causes(gamble, success, state_increase(subject, wealth)).
causes(gamble, failure, state_decrease(subject, wealth)).
responsible(gamble, subject).

action(borrow_money_from).
arg(borrow_money_from, subject, chr).
arg(borrow_money_from, object, chr).
causes(borrow_money_from, success, state_increase(subject, wealth)).
causes(borrow_money_from, success, state_decrease(object, wealth)).
causes(borrow_money_from, success, rel(subject, object, debt, owes_money_to)).
precondition_not(borrow_money_from, rel(subject,object,relationship,strangers)).
precondition_not(borrow_money_from, state(subject, wealth, has_wealth)).
precondition_not(borrow_money_from, state(object, wealth, penniless)).
precondition_not(borrow_money_from, rel(subject, object, debt, owes_money_to)).
responsible(borrow_money_from, subject).
nonreflexive(borrow_money_from).

action(repay_loan).
arg(repay_loan, subject, chr).
arg(repay_loan, object, chr).
causes(repay_loan, success, state_decrease(subject, wealth)).
causes(repay_loan, success, state_increase(object, wealth)).
causes(repay_loan, success, rel(subject, object, debt, no_debt_to)).
precondition(repay_loan, rel(subject, object, debt, owes_money_to)).
precondition_not(repay_loan, state(subject, wealth, penniless)).
precondition_not(repay_loan, state(object, wealth, has_wealth)).
responsible(repay_loan, subject).
nonreflexive(repay_loan).

action(give_wealth).
arg(give_wealth, subject, chr).
arg(give_wealth, object, chr).
causes(give_wealth, success, state_decrease(subject, wealth)).
causes(give_wealth, success, state_increase(object, wealth)).
precondition_not(give_wealth, rel(subject, object, relationship, strangers)).
precondition_not(give_wealth, state(subject, wealth, penniless)).
precondition_not(give_wealth, state(object, wealth, has_wealth)).
helpful(give_wealth, object).
responsible(give_wealth, subject).
nonreflexive(give_wealth).

action(rob).
arg(rob, subject, chr).
arg(rob, object, chr).
causes(rob, success, state_increase(subject, wealth)).
causes(rob, success, state_decrease(object, wealth)).
causes(rob, success, rel(object, subject, feelings, hates)).
initiates_acquaintanceship(rob).
precondition(rob, rel_category(subject, object, feelings, negative)).
precondition_or(rob, rel(subject, object, feelings, hates)).
precondition_not(rob, state(subject, wealth, has_wealth)).
precondition_not(rob, state(object, wealth, penniless)).
helpful(rob, subject).
harmful(rob, object).
responsible(rob, subject).
nonreflexive(rob).
immoral(rob).

action(meet).
arg(meet, subject, chr).
arg(meet, object, chr).
causes(meet, success, rel(subject, object, relationship, acquaintances)).
causes(meet, success, rel(object, subject, relationship, acquaintances)).
causes_maybe(meet, success, rel(subject, object, feelings, likes)).
causes_maybe(meet, success, rel(subject, object, feelings, dislikes)).
causes_maybe(meet, success, rel(object, subject, feelings, likes)).
causes_maybe(meet, success, rel(object, subject, feelings, dislikes)).
precondition(meet, rel(subject, object, relationship, strangers)).
precondition(meet, rel(object, subject, relationship, strangers)).
responsible(meet, fate).
nonreflexive(meet).

action(fall_for).
arg(fall_for, subject, chr).
arg(fall_for, object, chr).
causes(fall_for, success, rel(subject, object, feelings, loves)).
causes(fall_for, success, status(subject, _not(heartbroken))).
precondition_not(fall_for, rel(subject, object, feelings, loves)).
precondition_not(fall_for, rel_category(subject, object, feelings, negative)).
precondition_not(fall_for, rel(subject, object, relationship, strangers)).
responsible(fall_for, fate).
nonreflexive(fall_for).

action(confess_to).
arg(confess_to, subject, chr).
arg(confess_to, object, chr).
causes(confess_to, success, rel(subject, object, relationship, lovers)).
causes(confess_to, success, rel(object, subject, relationship, lovers)).
causes(confess_to, success, status(subject, _not(heartbroken))).
causes(confess_to, success, status(object, _not(heartbroken))).
causes(confess_to, failure, status(subject, heartbroken)).
causes_maybe_ifnot(
  confess_to,
  failure,
  rel(object, subject, feelings, hates),
  rel(object, subject, feelings, dislikes)
).
precondition_not(confess_to, rel(subject, object, relationship, lovers)).
precondition(confess_to, rel(subject, object, feelings, loves)).
precondition_not(
  confess_to,
  success,
  rel_category(object, subject, feelings, negative)
).
precondition_not(
  confess_to,
  failure,
  rel_category(object, subject, feelings, positive)
).
responsible(confess_to, subject).
nonreflexive(confess_to).
