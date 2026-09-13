use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/mint_item_non_canonical/field_scan
use midgard/fraud_proofs/mint_item_non_canonical/rule
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1.{encode_field_preimage}
use midgard/rejection_reason_v1.{MintDeclaredAssetLimit}

const prefix = #"82581c11111111111111111111111111111111111111111111111111111111"

const policy = #"11111111111111111111111111111111111111111111111111111111"

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
    source_key: #"",
    rejection_reason: None,
  }
}

fn until(
  state: rule.MintScanStateV1,
  item: ByteArray,
  budget: Int,
) -> rule.MintScanStateV1 {
  if state.outcome != rule.outcome_scanning {
    state
  } else {
    expect budget > 0
    let start = state.control.cursor / 4095 * 4095
    let window = bytearray.slice(item, start, start + 8189)
    until(rule.advance_scan_v1(state, window), item, budget - 1)
  }
}

fn verdict(item: ByteArray, previous: ByteArray) -> Bool {
  let bound = rule.bind_item_v1(
    accepted(),
    if previous == #"" {
      0
    } else {
      1
    },
  )
  let state = until(rule.initial_scan_v1(bound, item, previous), item, 100)
  rule.terminal_contradiction_v1(state)
}

test ada_policy_is_noncanonical() {
  verdict(#"8240a14005", #"")
}

test malformed_headers_are_total() {
  list.all(
    [
      #"00", #"82581c11", #"8240a14005",
      #"82581b111111111111111111111111111111111111111111111111111111a14001",
      #"83581c11111111111111111111111111111111111111111111111111111111a14001",
      #"9802581c11111111111111111111111111111111111111111111111111111111a14001",
    ],
    fn(item) { verdict(item, #"") },
  )
}

test malformed_assets_are_total() {
  list.all(
    [
      #"a0", #"bf4001ff", #"b8014001", #"a14000", #"a1401801", #"a1403800",
      #"a140c24101", #"a240014002", #"a2410201410101", #"a1400100", #"a1401b01",
      #"a1582111111111111111111111111111111111111111111111111111111111111111111101",
    ],
    fn(body) { verdict(bytearray.concat(prefix, body), #"") },
  )
}

test canonical_mint_and_burn_do_not_convict() {
  list.all(
    [
      #"a14001", #"a14020", #"a1401bffffffffffffffff", #"a1403bffffffffffffffff",
      #"a3400141002042010002",
    ],
    fn(body) { !verdict(bytearray.concat(prefix, body), #"") },
  )
}

test duplicate_policy_convicts() {
  verdict(bytearray.concat(prefix, #"a14001"), policy)
}

test descending_policy_convicts() {
  verdict(
    bytearray.concat(prefix, #"a14001"),
    #"22222222222222222222222222222222222222222222222222222222",
  )
}

test ascending_policy_does_not_convict() {
  !verdict(
    bytearray.concat(prefix, #"a14001"),
    #"00000000000000000000000000000000000000000000000000000000",
  )
}

test empty_item_belongs_to_width_family() fail {
  expect _ = rule.initial_scan_v1(rule.bind_item_v1(accepted(), 0), #"", #"")
}

test unrelated_rejection_is_refused() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..accepted(),
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    source_key: #"00",
    rejection_reason: Some(MintDeclaredAssetLimit { policy_index: 0 }),
  }
  expect _ = rule.bind_item_v1(subject, 0)
}

test substituted_window_is_refused() fail {
  let initial = rule.initial_scan_v1(
    rule.bind_item_v1(accepted(), 0),
    #"8240a14005",
    #"",
  )
  expect _ = rule.advance_scan_v1(initial, #"8240a14006")
}

test incomplete_scan_cannot_finalize() fail {
  rule.terminal_contradiction_v1(
    rule.initial_scan_v1(
      rule.bind_item_v1(accepted(), 0),
      bytearray.concat(prefix, #"a14001"),
      #"",
    ),
  )
}

test malformed_predecessor_cannot_supply_ordering_key() fail {
  expect _ = rule.previous_policy_v1(#"8240a14005")
}

test field_cursor_resumes_without_rewalking_prefix() {
  let item = bytearray.concat(prefix, #"a14001")
  let field = encode_field_preimage(list.repeat(item, 65))
  expect Pair(first, False) = field_scan.advance(field, 64, None)
  expect Pair(second, False) = field_scan.advance(field, 64, Some(first))
  expect Pair(last, True) = field_scan.advance(field, 64, Some(second))
  and {
    first.next_index == 32,
    second.next_index == 64,
    last.next_index == 65,
    last.next_offset == bytearray.length(field),
    last.item_length == bytearray.length(item),
    last.previous_policy == policy,
  }
}

test field_envelope_trailing_bytes_are_refused() fail {
  let field = bytearray.concat(encode_field_preimage([#"8240a14005"]), #"00")
  expect _ = field_scan.advance(field, 0, None)
}
