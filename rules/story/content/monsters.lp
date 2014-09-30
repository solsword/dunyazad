% monsters.lp
% Monsters.

% Fluents:
% --------

relationship(chr, monster, m_threat_level).
rel_value(m_threat_level, never_heard_of).
rel_value(m_threat_level, rumored).
rel_value(m_threat_level, sighted_benign).
rel_value(m_threat_level, sighted_threatening).
rel_value(m_threat_level, witnessed_aggression).
rel_value(m_threat_level, witnessed_passivity).
rel_value(m_threat_level, witnessed_horror).
default_rel_value(m_threat_level, never_heard_of).
rel_value_order(m_threat_level, never_heard_of, rumored).
rel_value_order(m_threat_level, rumored, sighted_benign).
rel_value_order(m_threat_level, rumored, sighted_threatening).
rel_value_order(m_threat_level, sighted_benign, sighted_threatening).
rel_value_order(m_threat_level, sighted_benign, witnessed_aggression).
rel_value_order(m_threat_level, sighted_benign, witnessed_passivity).
rel_value_order(m_threat_level, sighted_benign, witnessed_horror).
rel_value_order(m_threat_level, sighted_threatening, witnessed_aggression).
rel_value_order(m_threat_level, sighted_threatening, witnessed_passivity).
rel_value_order(m_threat_level, sighted_threatening, witnessed_horror).
rel_value_order(m_threat_level, witnessed_passivity, witnessed_aggression).
rel_value_order(m_threat_level, witnessed_aggression, witnessed_horror).

% Actions:
% --------

% m_gossip
action(m_gossip).
argument(m_gossip, subject, chr).
variable_argument(m_gossip, object, chr, 1, #supremum).
argument(m_gossip, monster, monster).
precondition_not(
  m_gossip,
  fl(rel(m_threat_level, never_heard_of), a(subject, monster))
).
precondition(
  m_gossip,
  fl(rel(m_threat_level, never_heard_of), a(object, monster))
).
causes(m_gossip, rel(m_threat_level, rumored), a(object, monster)).

% m_appear
action(m_appear).
argument(m_appear, subject, monster).
auto_argument(
  m_appear,
  setting,
  loc,
  fl(rel(mnst_at, true), a(subject, setting))
).
auto_argument(
  m_appear,
  witnesses,
  chr,
  fl(rel(chr_at, true), a(witnesses, setting))
).
causes_if(
  m_appear,
  fl(
    less_than(rel(m_threat_level, sighted_benign)),
    a(witnesses, subject)
  ),
  rel(m_threat_level, sighted_benign),
  a(witnesses, subject)
).

% m_menace
action(m_menace).
argument(m_menace, subject, monster).
auto_argument(
  m_menace,
  setting,
  loc,
  fl(rel(mnst_at, true), a(subject, setting))
).
auto_argument(
  m_menace,
  witnesses,
  chr,
  fl(rel(chr_at, true), a(witnesses, setting))
).
causes_if(
  m_menace,
  fl(
    less_than(rel(m_threat_level, sighted_threatening)),
    a(witnesses, subject)
  ),
  rel(m_threat_level, sighted_threatening),
  a(witnesses, subject)
).
