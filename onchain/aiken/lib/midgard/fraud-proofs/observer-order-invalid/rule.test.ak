use midgard/fraud_proofs/observer_order_invalid/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{ObserverOrderInvalid, OutputNonCanonical}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const checkpoint =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const observer_a = #"00000000000000000000000000000000000000000000000000000000"

const observer_b = #"01010101010101010101010101010101010101010101010101010101"

const observer_c = #"02020202020202020202020202020202020202020202020202020202"

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn rejected(index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(ObserverOrderInvalid { observer_index: index }),
  }
}

fn scan(
  subject: substrate.VerdictSubjectV1,
  target: Int,
  values: List<ByteArray>,
) -> rule.ScanStateV1 {
  scan_from(
    rule.initial_scan_v1(rule.bind_observer_v1(subject, target), checkpoint),
    values,
  )
}

fn scan_from(
  state: rule.ScanStateV1,
  values: List<ByteArray>,
) -> rule.ScanStateV1 {
  when values is {
    [] -> state
    [head, ..tail] ->
      scan_from(rule.scan_item_v1(state, state.seen, head), tail)
  }
}

test observer_order_invalid_first_pair_wrongful_acceptance() {
  let terminal = scan(accepted(), 1, [observer_b, observer_a])
  and {
    terminal.outcome == rule.outcome_violation,
    rule.terminal_contradiction_v1(rule.decision_v1(terminal)),
  }
}

test observer_order_invalid_middle_pair_wrongful_acceptance() {
  let terminal = scan(accepted(), 2, [observer_a, observer_c, observer_b])
  rule.terminal_contradiction_v1(rule.decision_v1(terminal))
}

test observer_order_invalid_last_pair_wrongful_acceptance() {
  let terminal = scan(
    accepted(),
    3,
    [observer_a, observer_b, observer_c, observer_c],
  )
  rule.terminal_contradiction_v1(rule.decision_v1(terminal))
}

test observer_order_invalid_duplicate_pair_wrongful_acceptance() {
  let terminal = scan(accepted(), 1, [observer_a, observer_a])
  rule.terminal_contradiction_v1(rule.decision_v1(terminal))
}

test observer_order_invalid_ordered_pair_wrongful_rejection() {
  let terminal = scan(rejected(2), 2, [observer_a, observer_b, observer_c])
  and {
    terminal.outcome == rule.outcome_ordered,
    rule.terminal_contradiction_v1(rule.decision_v1(terminal)),
  }
}

test observer_order_invalid_honest_acceptance_refuses() fail {
  rule.terminal_contradiction_v1(
    rule.decision_v1(scan(accepted(), 1, [observer_a, observer_b])),
  )
}

test observer_order_invalid_honest_rejection_refuses() fail {
  rule.terminal_contradiction_v1(
    rule.decision_v1(scan(rejected(1), 1, [observer_b, observer_a])),
  )
}

test observer_order_invalid_refuses_reason_coordinate_substitution() fail {
  rule.bind_observer_v1(rejected(2), 1).observer_index == 1
}

test observer_order_invalid_refuses_other_reason() fail {
  rule.bind_observer_v1(
    substrate.VerdictSubjectV1 {
      ..rejected(1),
      rejection_reason: Some(OutputNonCanonical { output_index: 1 }),
    },
    1,
  ).observer_index == 1
}

test observer_order_invalid_refuses_zero_coordinate() fail {
  rule.bind_observer_v1(accepted(), 0).observer_index == 0
}

test observer_order_invalid_refuses_substituted_width() fail {
  let state = rule.initial_scan_v1(
    rule.bind_observer_v1(accepted(), 1),
    checkpoint,
  )
  rule.scan_item_v1(state, 0, #"00").seen == 1
}

test observer_order_invalid_refuses_skipped_coordinate() fail {
  let state = rule.initial_scan_v1(
    rule.bind_observer_v1(accepted(), 2),
    checkpoint,
  )
  rule.scan_item_v1(state, 1, observer_a).seen == 1
}

test observer_order_invalid_refuses_earlier_violation() fail {
  scan(accepted(), 2, [observer_b, observer_a, observer_c]).seen == 3
}
