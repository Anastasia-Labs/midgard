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

/// `a3 40 01 41 00 01 42 00 00 01`: three ascending assets.
fn triple(policy: ByteArray) -> ByteArray {
  #"82581c"
    |> bytearray.concat(policy)
    |> bytearray.concat(#"a3400141000142000001")
}

/// Declares 16,385 assets; the body is deliberately absent.
fn crossing_target(policy: ByteArray) -> ByteArray {
  #"82581c" |> bytearray.concat(policy) |> bytearray.concat(#"b9400100")
}

/// Declares 16,383 assets; the body is deliberately absent.
fn boundary_target(policy: ByteArray) -> ByteArray {
  #"82581c" |> bytearray.concat(policy) |> bytearray.concat(#"b93fff00")
}

fn complete(
  state: rule.FoldStateV1,
  index: Int,
  item: ByteArray,
) -> rule.FoldStateV1 {
  let Pair(consumed, _) = rule.consume_assets_v1(
    rule.begin_policy_v1(state, index, item),
    index,
    item,
    rule.staged_fold_budget,
  )
  consumed
}

fn initial(
  subject: substrate.VerdictSubjectV1,
  index: Int,
  target: ByteArray,
) -> rule.FoldStateV1 {
  rule.initial_fold_v1(rule.bind_policy_v1(subject, index), target, checkpoint)
}

test mint_declared_limit_reads_exact_policy_header() {
  let header = rule.policy_header_v1(singleton(policy_a))
  and {
    header.policy_id == policy_a,
    header.declared_count == 1,
    header.assets_offset == 32,
  }
}

/// The machine reads the item's array head with
/// `decode_definite_array_header_at`; the twin uses the same reader, so a
/// two-element head spelled `98 02` is a policy item for both.
test mint_declared_limit_reads_the_machine_array_head() {
  let item = #"9802581c"
    |> bytearray.concat(policy_a)
    |> bytearray.concat(#"a14001")
  let state = complete(initial(rejected(0), 0, item), 0, item)
  and {
    rule.policy_header_v1(item).declared_count == 1,
    state.outcome == rule.outcome_non_crossing,
  }
}

test mint_declared_limit_accepted_crossing_uses_header_before_body() {
  let target = crossing_target(policy_b)
  let before = complete(initial(accepted(), 1, target), 0, singleton(policy_a))
  let terminal = rule.begin_policy_v1(before, 1, target)
  and {
    before.accumulated_count == 1,
    before.previous_policy == policy_a,
    before.active_policy == #"",
    terminal.outcome == rule.outcome_crossing,
    rule.terminal_contradiction_v1(rule.decision_v1(terminal)),
  }
}

test mint_declared_limit_forced_non_crossing_completes_target_item() {
  let item = triple(policy_a)
  let terminal = complete(initial(rejected(0), 0, item), 0, item)
  and {
    terminal.outcome == rule.outcome_non_crossing,
    terminal.accumulated_count == 3,
    terminal.active_policy == #"",
    terminal.assets_remaining == 0,
    rule.terminal_contradiction_v1(rule.decision_v1(terminal)),
  }
}

/// Consuming a policy across two budgets reaches the state one budget does.
test mint_declared_limit_resumes_inside_a_policy_item() {
  let item = triple(policy_a)
  let opened = rule.begin_policy_v1(initial(rejected(0), 0, item), 0, item)
  let Pair(partial, left) = rule.consume_assets_v1(opened, 0, item, 2)
  let Pair(resumed, _) = rule.consume_assets_v1(partial, 0, item, 5)
  and {
    left == 0,
    partial.outcome == rule.outcome_scanning,
    partial.active_policy == policy_a,
    partial.assets_remaining == 1,
    partial.policy_asset_cursor == 2,
    partial.previous_asset == #"00",
    partial.accumulated_count == 2,
    resumed == complete(initial(rejected(0), 0, item), 0, item),
  }
}

test mint_declared_limit_refuses_opening_over_an_open_policy() fail {
  let item = triple(policy_a)
  let opened = rule.begin_policy_v1(initial(rejected(0), 0, item), 0, item)
  rule.begin_policy_v1(opened, 0, item).outcome == rule.outcome_scanning
}

test mint_declared_limit_refuses_consuming_a_closed_policy() fail {
  let item = singleton(policy_a)
  let closed = complete(initial(rejected(0), 0, item), 0, item)
  rule.consume_asset_v1(closed, 0, item).accumulated_count == 2
}

test mint_declared_limit_accepts_the_exact_boundary() {
  let target = boundary_target(policy_b)
  let before = complete(initial(rejected(1), 1, target), 0, singleton(policy_a))
  let opened = rule.begin_policy_v1(before, 1, target)
  and {
    before.accumulated_count + 16383 == rule.max_distinct_asset_count,
    opened.outcome == rule.outcome_scanning,
    opened.active_policy == policy_b,
    opened.assets_remaining == 16383,
  }
}

test mint_declared_limit_crosses_one_past_the_boundary() {
  let target = crossing_target(policy_b)
  let before = complete(initial(accepted(), 1, target), 0, singleton(policy_a))
  // 1 + 16,385 > 16,384; the target body is never read.
  rule.begin_policy_v1(before, 1, target).outcome == rule.outcome_crossing
}

test mint_declared_limit_honest_accepted_non_crossing_refuses() fail {
  let item = singleton(policy_a)
  let terminal = complete(initial(accepted(), 0, item), 0, item)
  rule.terminal_contradiction_v1(rule.decision_v1(terminal))
}

test mint_declared_limit_honest_rejection_crossing_refuses() fail {
  let target = crossing_target(policy_b)
  let before = complete(initial(rejected(1), 1, target), 0, singleton(policy_a))
  rule.terminal_contradiction_v1(
    rule.decision_v1(rule.begin_policy_v1(before, 1, target)),
  )
}

test mint_declared_limit_refuses_an_unfinished_decision() fail {
  let item = triple(policy_a)
  let opened = rule.begin_policy_v1(initial(rejected(0), 0, item), 0, item)
  rule.decision_v1(opened).crossing
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

test mint_declared_limit_refuses_negative_coordinate() fail {
  rule.bind_policy_v1(accepted(), -1).policy_index == -1
}

test mint_declared_limit_refuses_item_past_the_bound_coordinate() fail {
  let item = singleton(policy_a)
  rule.begin_policy_v1(initial(accepted(), 0, item), 1, item).outcome == rule.outcome_scanning
}

test mint_declared_limit_refuses_target_item_substitution() fail {
  let expected = singleton(policy_a)
  let substituted = singleton(policy_b)
  rule.begin_policy_v1(initial(accepted(), 0, expected), 0, substituted).outcome == rule.outcome_scanning
}

test mint_declared_limit_refuses_declared_count_substitution() fail {
  let expected = triple(policy_a)
  let substituted = singleton(policy_a)
  rule.begin_policy_v1(initial(accepted(), 0, expected), 0, substituted).outcome == rule.outcome_scanning
}

test mint_declared_limit_refuses_malformed_checkpoint() fail {
  rule.initial_fold_v1(
    rule.bind_policy_v1(accepted(), 0),
    singleton(policy_a),
    #"aaaa",
  ).accumulated_count == 0
}

test mint_declared_limit_refuses_zero_quantity_in_non_crossing_item() fail {
  let item = #"82581c"
    |> bytearray.concat(policy_a)
    |> bytearray.concat(#"a14000")
  complete(initial(rejected(0), 0, item), 0, item).outcome == rule.outcome_non_crossing
}

test mint_declared_limit_refuses_unordered_assets() fail {
  let item = #"82581c"
    |> bytearray.concat(policy_a)
    |> bytearray.concat(#"a24100014001")
  complete(initial(rejected(0), 0, item), 0, item).outcome == rule.outcome_non_crossing
}

test mint_declared_limit_refuses_wide_asset_name() fail {
  let item = #"82581c"
    |> bytearray.concat(policy_a)
    |> bytearray.concat(#"a15821")
    |> bytearray.concat(
        #"000000000000000000000000000000000000000000000000000000000000000000",
      )
    |> bytearray.concat(#"01")
  complete(initial(rejected(0), 0, item), 0, item).outcome == rule.outcome_non_crossing
}

test mint_declared_limit_refuses_trailing_bytes_after_the_last_asset() fail {
  let item = #"82581c"
    |> bytearray.concat(policy_a)
    |> bytearray.concat(#"a1400100")
  complete(initial(rejected(0), 0, item), 0, item).outcome == rule.outcome_non_crossing
}

test mint_declared_limit_refuses_body_shorter_than_declared() fail {
  let item = #"82581c"
    |> bytearray.concat(policy_a)
    |> bytearray.concat(#"a24001")
  complete(initial(rejected(0), 0, item), 0, item).outcome == rule.outcome_non_crossing
}

test mint_declared_limit_refuses_out_of_order_prior_policy() fail {
  let target = singleton(policy_a)
  let state = initial(accepted(), 1, target)
  rule.begin_policy_v1(
    rule.FoldStateV1 { ..state, previous_policy: policy_b },
    1,
    target,
  ).outcome == rule.outcome_scanning
}

test mint_declared_limit_refuses_a_prior_first_crossing() fail {
  let target = singleton(policy_b)
  let state = rule.FoldStateV1 {
    ..initial(accepted(), 1, target),
    accumulated_count: 16383,
  }
  // A prior item declaring two assets is the first crossing, not the bound one.
  let prior = #"82581c"
    |> bytearray.concat(policy_a)
    |> bytearray.concat(#"a24001410001")
  rule.begin_policy_v1(state, 0, prior).outcome == rule.outcome_scanning
}

/// Golden wire vector shared with the TypeScript schema test.
test mint_declared_limit_fold_state_golden_vector() {
  let item = triple(policy_a)
  let opened = rule.begin_policy_v1(initial(rejected(0), 0, item), 0, item)
  let Pair(partial, _) = rule.consume_assets_v1(opened, 0, item, 2)
  trace @"fold_state_golden": rule.encode_fold_state_v1(partial)
  bytearray.length(rule.encode_fold_state_v1(partial)) > 0
}
