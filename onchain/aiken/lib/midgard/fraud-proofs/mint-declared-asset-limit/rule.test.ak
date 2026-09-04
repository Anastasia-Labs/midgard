use aiken/primitive/bytearray
use midgard/fraud_proofs/mint_declared_asset_limit/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/rejection_reason_v1.{MintDeclaredAssetLimit, OutputNonCanonical}

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const policy_a = #"00000000000000000000000000000000000000000000000000000000"

const policy_b = #"01010101010101010101010101010101010101010101010101010101"

const checkpoint =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

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
    rejection_reason: Some(MintDeclaredAssetLimit { policy_index: index }),
  }
}

fn singleton(policy: ByteArray) -> ByteArray {
  #"82581c" |> bytearray.concat(policy) |> bytearray.concat(#"a14001")
}

test mint_declared_limit_reads_exact_policy_header() {
  let header = rule.policy_header_v1(singleton(policy_a))
  and {
    header.policy_id == policy_a,
    header.declared_count == 1,
    rule.canonical_policy_asset_count_v1(singleton(policy_a)) == 1,
  }
}

test mint_declared_limit_accepted_crossing_uses_header_before_body() {
  // The target body is intentionally not canonical/complete. The frozen
  // machine takes the declared-count rejection before decoding it.
  let target = #"82581c"
    |> bytearray.concat(policy_b)
    |> bytearray.concat(#"b9400100")
  let bound = rule.bind_policy_v1(accepted(), 1)
  let initial = rule.initial_fold_v1(bound, target, checkpoint)
  let before = rule.FoldStateV1 {
    ..initial,
    accumulated_count: 1,
    previous_policy: policy_a,
  }
  let terminal = rule.fold_item_v1(before, 1, target)
  and {
    terminal.outcome == rule.outcome_crossing,
    rule.terminal_contradiction_v1(rule.decision_v1(terminal)),
  }
}

test mint_declared_limit_forced_non_crossing_completes_target_item() {
  let item = singleton(policy_a)
  let initial = rule.initial_fold_v1(
    rule.bind_policy_v1(rejected(0), 0),
    item,
    checkpoint,
  )
  let terminal = rule.fold_item_v1(initial, 0, item)
  and {
    terminal.outcome == rule.outcome_non_crossing,
    terminal.accumulated_count == 1,
    rule.terminal_contradiction_v1(rule.decision_v1(terminal)),
  }
}

test mint_declared_limit_honest_accepted_non_crossing_refuses() fail {
  let item = singleton(policy_a)
  let terminal = rule.fold_item_v1(
    rule.initial_fold_v1(rule.bind_policy_v1(accepted(), 0), item, checkpoint),
    0,
    item,
  )
  rule.terminal_contradiction_v1(rule.decision_v1(terminal))
}

test mint_declared_limit_honest_rejection_crossing_refuses() fail {
  let target = #"82581c"
    |> bytearray.concat(policy_b)
    |> bytearray.concat(#"b9400100")
  let initial = rule.initial_fold_v1(
    rule.bind_policy_v1(rejected(1), 1),
    target,
    checkpoint,
  )
  let terminal = rule.fold_item_v1(
    rule.FoldStateV1 {
      ..initial,
      accumulated_count: 1,
      previous_policy: policy_a,
    },
    1,
    target,
  )
  rule.terminal_contradiction_v1(rule.decision_v1(terminal))
}

test mint_declared_limit_refuses_reason_coordinate_substitution() fail {
  rule.bind_policy_v1(rejected(4), 3).policy_index == 3
}

test mint_declared_limit_refuses_other_reason() fail {
  rule.bind_policy_v1(
    substrate.VerdictSubjectV1 {
      ..rejected(0),
      rejection_reason: Some(OutputNonCanonical { output_index: 0 }),
    },
    0,
  ).policy_index == 0
}

test mint_declared_limit_refuses_target_item_substitution() fail {
  let expected = singleton(policy_a)
  let substituted = singleton(policy_b)
  let state = rule.initial_fold_v1(
    rule.bind_policy_v1(accepted(), 0),
    expected,
    checkpoint,
  )
  rule.fold_item_v1(state, 0, substituted).outcome == rule.outcome_scanning
}

test mint_declared_limit_refuses_zero_quantity_in_non_crossing_item() fail {
  let item = #"82581c"
    |> bytearray.concat(policy_a)
    |> bytearray.concat(#"a14000")
  let state = rule.initial_fold_v1(
    rule.bind_policy_v1(rejected(0), 0),
    item,
    checkpoint,
  )
  rule.fold_item_v1(state, 0, item).outcome == rule.outcome_non_crossing
}

test mint_declared_limit_refuses_out_of_order_prior_policy() fail {
  let target = singleton(policy_a)
  let state = rule.initial_fold_v1(
    rule.bind_policy_v1(accepted(), 1),
    target,
    checkpoint,
  )
  rule.fold_item_v1(
    rule.FoldStateV1 { ..state, previous_policy: policy_b },
    1,
    target,
  ).outcome == rule.outcome_non_crossing
}
