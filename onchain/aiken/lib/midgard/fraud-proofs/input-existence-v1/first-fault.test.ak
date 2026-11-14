//// Source-bound original coordinates + actual empty-ledger nonmembership.
//// Existing bind/pre-state suites separately authenticate the event/root link.

use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use midgard/fraud_proofs/common.{RedeemerCarriedNonMembership}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyFieldOpening, FieldOpeningV1, opened_field_view,
}
use midgard/fraud_proofs/first_fault_v1 as order
use midgard/fraud_proofs/input_existence_v1/select
use midgard/fraud_proofs/input_existence_v1/terminal
use midgard/fraud_proofs/input_existence_v1/types.{Absent, LedgerStateV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  decode_forced_tx_compact_v1, encode_forced_tx_compact_v1,
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  ForcedTxCompact, NativeTxBodyCompact, TxCompactView,
} as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1.{
  Inline, encode_field_array_header, field_commitment,
}
use midgard/rejection_reason_v1 as reason
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, submitted_source,
}

fn input(index: Int) -> ByteArray {
  #"825820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f19"
    |> bytearray.concat(bytearray.from_int_big_endian(index, 2))
}

fn preimage(indices: List<Int>) -> ByteArray {
  encode_field_array_header(list.length(indices))
    |> bytearray.concat(
        list.foldl(
          indices,
          #"",
          fn(index, before) {
            bytearray.concat(before, bytearray.concat(#"5826", input(index)))
          },
        ),
      )
}

fn opening(compact_cbor: ByteArray, bytes: ByteArray) -> FieldOpeningV1 {
  BodyFieldOpening {
    native_tx_compact_cbor: compact_cbor,
    carriage: Inline { preimage: bytes },
  }
}

/// How the prover carries the claimed coordinate's field opening.
type ClaimOpening {
  /// Exactly when the claimed field is a known field other than the observed
  /// one and its index can be read: the honest prover's carriage.
  Honest
  Omitted
  /// The claimed field's commitment over an empty field instead.
  Substituted
  /// A spend-field opening regardless of the claim.
  Unused
}

fn run(
  observed_kind: Int,
  observed_index: Int,
  r: reason.RejectionReasonV1,
  substitute_claim: Bool,
  omit_claim: Bool,
  network_id: Int,
) -> Bool {
  run_in(
    [10, 30],
    [20],
    observed_kind,
    observed_index,
    r,
    if omit_claim {
      Omitted
    } else if substitute_claim {
      Substituted
    } else {
      Honest
    },
    network_id,
  )
}

fn claim(
  observed_kind: Int,
  observed_index: Int,
  r: reason.RejectionReasonV1,
) -> Bool {
  run_in([10, 30], [20], observed_kind, observed_index, r, Honest, 255)
}

fn run_in(
  spend_keys: List<Int>,
  reference_keys: List<Int>,
  observed_kind: Int,
  observed_index: Int,
  r: reason.RejectionReasonV1,
  claim_opening: ClaimOpening,
  network_id: Int,
) -> Bool {
  let spend = preimage(spend_keys)
  let reference = preimage(reference_keys)
  let base = decode_forced_tx_compact_v1(
    submitted_source(empty_v1_transaction()).compact_cbor,
  )
  let compact = ForcedTxCompact {
    ..base,
    body: NativeTxBodyCompact {
      ..base.body,
      network_id,
      spend_inputs_hash: field_commitment(spend),
      reference_inputs_hash: field_commitment(reference),
    },
  }
  let compact_cbor = encode_forced_tx_compact_v1(compact)
  let subject = substrate.VerdictSubjectV1 {
    version: 1,
    direction: 1,
    source_kind: 1,
    transaction_id: native_tx_id_for_version(
      1,
      encode_native_tx_body_compact(compact.body),
    ),
    source_key: #"00",
    rejection_reason: Some(r),
  }
  let view = opened_field_view(
    opening(
      compact_cbor,
      if observed_kind == 0 {
        spend
      } else {
        reference
      },
    ),
    BodyAnchor { tx_id: subject.transaction_id, source_kind: 1 },
    observed_kind,
    [],
    #"",
  )
  let index = select.selected_index_v1(subject, observed_kind, observed_index)
  let selected = select.selected_input_v1(subject, observed_kind, view, index)
  let honest_opening = when order.resolution_coordinate_of_reason_v1(r) is {
    Some(Pair(kind, index)) ->
      if ( kind == 0 || kind == 1 ) && kind != observed_kind && index >= 0 {
        Some(kind)
      } else {
        None
      }
    None -> None
  }
  let claimed_opening = when Pair(claim_opening, honest_opening) is {
    Pair(Honest, Some(kind)) ->
      Some(
        opening(
          compact_cbor,
          if kind == 0 {
            spend
          } else {
            reference
          },
        ),
      )
    Pair(Substituted, Some(_)) -> Some(opening(compact_cbor, #"80"))
    Pair(Unused, _) -> Some(opening(compact_cbor, spend))
    _ -> None
  }
  let claimed = select.claimed_input_v1(
    subject,
    observed_kind,
    view,
    claimed_opening,
    [],
    #"",
  )
  let root = mpf.root(mpf.empty)
  let compact_view = TxCompactView {
    body: compact.body,
    witness_set_hash: compact.witness_set_hash,
    validity_code: None,
  }
  let state = LedgerStateV1 {
    subject,
    compact: compact_view,
    network_order_context: order.network_order_context_v1(
      compact_view,
      fixture.challenged_block_v1().header,
    ),
    source_kind: observed_kind,
    input_index: index,
    selected_input: selected,
    claimed_input: claimed,
    pre_utxos_root: root,
  }
  expect Some(selected_input) = selected
  terminal.verdict_contradiction_v1(
    state,
    Absent {
      non_membership: RedeemerCarriedNonMembership {
        non_membership_proof: [],
        non_membership_proof_script_redeemer_index: 0,
      },
    },
    [],
    [
      fixture.pexcludes_withdraw_redeemer_entry(
        root,
        encode_midgard_tx_input(selected_input),
        [],
      ),
    ],
  )
}

test actual_source_smaller_spend_absence_convicts_later_reference_absence() {
  run(
    0,
    0,
    reason.InputNotFound { source_kind: 1, input_index: 0 },
    False,
    False,
    255,
  )
}

test actual_source_reference_absence_precedes_later_spend_raw_index_one() {
  run(
    1,
    0,
    reason.InputNotFound { source_kind: 0, input_index: 1 },
    False,
    False,
    255,
  )
}

test actual_source_later_spend_absence_refuses_earlier_reference_claim() {
  !run(
    0,
    1,
    reason.InputNotFound { source_kind: 1, input_index: 0 },
    False,
    False,
    255,
  )
}

test actual_source_true_first_absence_is_refused() {
  !run(
    0,
    0,
    reason.InputNotFound { source_kind: 0, input_index: 0 },
    False,
    False,
    255,
  )
}

test actual_source_same_input_absence_convicts_output_subcheck_claim() {
  run(
    0,
    0,
    reason.InputSpentOutputNonCanonical { source_kind: 0, input_index: 0 },
    False,
    False,
    255,
  )
}

test actual_source_later_input_absence_refuses_earlier_signer_subcheck_claim() {
  !run(
    1,
    0,
    reason.SpendInputSignerMissing { input_index: 0 },
    False,
    False,
    255,
  )
}

test actual_source_missing_cross_field_opening_is_refused() fail {
  run(
    0,
    0,
    reason.InputNotFound { source_kind: 1, input_index: 0 },
    False,
    True,
    255,
  )
}

test actual_source_substituted_claimed_field_is_refused() fail {
  run(
    0,
    0,
    reason.InputNotFound { source_kind: 1, input_index: 0 },
    True,
    False,
    255,
  )
}

// Spend keys 10, 30 and reference key 20 throughout, unless a row names its
// own fields. Every observed input is absent from the empty ledger.

test n7_reference_absence_convicts_later_spend_signer_claim() {
  claim(1, 0, reason.SpendInputSignerMissing { input_index: 1 })
}

// One field whose raw positions run against its out-ref keys: the key decides.
test n7_within_field_smaller_key_at_later_index_is_admitted() {
  !run_in(
    [30, 10],
    [20],
    0,
    0,
    reason.InputNotFound { source_kind: 0, input_index: 1 },
    Honest,
    255,
  )
}

test n7_within_field_smaller_key_at_later_index_convicts() {
  run_in(
    [30, 10],
    [20],
    0,
    1,
    reason.InputNotFound { source_kind: 0, input_index: 0 },
    Honest,
    255,
  )
}

// A claimed coordinate that names no input of the transaction convicts.
test n7_out_of_domain_absence_claim_in_unknown_field_convicts() {
  claim(0, 0, reason.InputNotFound { source_kind: 2, input_index: 0 })
}

test n7_out_of_domain_absence_claim_at_reference_count_convicts() {
  claim(0, 0, reason.InputNotFound { source_kind: 1, input_index: 1 })
}

test n7_out_of_domain_output_claim_at_negative_index_convicts() {
  claim(
    0,
    0,
    reason.InputSpentOutputNonCanonical { source_kind: 0, input_index: -1 },
  )
}

test n7_out_of_domain_reference_script_claim_at_reference_count_convicts() {
  claim(
    0,
    0,
    reason.ResolvedReferenceScriptMalformed { source_kind: 1, input_index: 1 },
  )
}

test n7_out_of_domain_node_limit_claim_at_spend_count_convicts() {
  claim(
    0,
    0,
    reason.ResolvedReferenceScriptNodeLimit { source_kind: 0, input_index: 2 },
  )
}

test n7_out_of_domain_depth_limit_claim_in_negative_field_convicts() {
  claim(
    1,
    0,
    reason.ResolvedReferenceScriptDepthLimit { source_kind: -1, input_index: 0 },
  )
}

test n7_out_of_domain_signer_claim_at_spend_count_convicts() {
  claim(1, 0, reason.SpendInputSignerMissing { input_index: 2 })
}

// The last input of a field is read and ordered, not treated as out of domain.
test n7_last_spend_input_claim_is_ordered_and_admitted() {
  !run_in(
    [10, 30],
    [40],
    1,
    0,
    reason.InputSpentOutputNonCanonical { source_kind: 0, input_index: 1 },
    Honest,
    255,
  )
}

test n7_last_reference_input_true_first_fault_is_admitted() {
  !claim(1, 0, reason.InputNotFound { source_kind: 1, input_index: 0 })
}

test n7_unused_claimed_opening_is_refused() fail {
  run_in(
    [10, 30],
    [20],
    0,
    0,
    reason.InputNotFound { source_kind: 0, input_index: 1 },
    Unused,
    255,
  )
}

test n7_claimed_opening_for_unknown_field_is_refused() fail {
  run_in(
    [10, 30],
    [20],
    0,
    0,
    reason.InputNotFound { source_kind: 2, input_index: 0 },
    Unused,
    255,
  )
}

test n7_count_bound_without_claimed_opening_is_refused() fail {
  run_in(
    [10, 30],
    [20],
    0,
    0,
    reason.InputNotFound { source_kind: 1, input_index: 1 },
    Omitted,
    255,
  )
}

test n7_count_bound_over_substituted_opening_is_refused() fail {
  run_in(
    [10, 30],
    [20],
    0,
    0,
    reason.InputNotFound { source_kind: 1, input_index: 1 },
    Substituted,
    255,
  )
}

test n7_actual_source_body_mismatch_admits_earlier_network_claim() {
  let network = fixture.challenged_block_v1().header.expected_network_id
  !run(
    0,
    0,
    reason.NetworkIdMismatch,
    False,
    False,
    if network == 0 {
      1
    } else {
      0
    },
  )
}

test n7_actual_source_matching_body_refutes_later_network_claim() {
  run(
    0,
    0,
    reason.NetworkIdMismatch,
    False,
    False,
    fixture.challenged_block_v1().header.expected_network_id,
  )
}

test n7_actual_source_untagged_body_refutes_later_network_claim() {
  run(0, 0, reason.NetworkIdMismatch, False, False, 255)
}

// This positional wire vector tests serialization, not source authentication.
fn wire_compact() -> TxCompactView {
  TxCompactView {
    body: NativeTxBodyCompact {
      spend_inputs_hash: #"0101010101010101010101010101010101010101010101010101010101010101",
      reference_inputs_hash: #"0202020202020202020202020202020202020202020202020202020202020202",
      outputs_hash: #"0303030303030303030303030303030303030303030303030303030303030303",
      fee: 4,
      validity_interval_start: 5,
      validity_interval_end: 6,
      required_observers_hash: #"0707070707070707070707070707070707070707070707070707070707070707",
      required_signers_hash: #"0808080808080808080808080808080808080808080808080808080808080808",
      mint_hash: #"0909090909090909090909090909090909090909090909090909090909090909",
      script_integrity_hash: #"0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a",
      auxiliary_data_hash: #"0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b",
      network_id: 1,
    },
    witness_set_hash: #"0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c",
    validity_code: None,
  }
}

test n7_network_context_wire_and_purpose_receipts_match_sdk() {
  let coordinate = order.PurposeCoordinateV1 { kind: -1, index: 2 }
  and {
    cbor.serialise(order.NetworkOrderContextV1 { body_network_mismatch: False }) == #"d8799fd87980ff",
    cbor.serialise(order.NetworkOrderContextV1 { body_network_mismatch: True }) == #"d8799fd87a80ff",
    cbor.serialise(coordinate) == #"d8799f2002ff",
    cbor.serialise(order.ExistingPurpose { coordinate }) == #"d8799fd8799f2002ffff",
    cbor.serialise(order.ImpossiblePurpose { coordinate }) == #"d87a9fd8799f2002ffff",
  }
}

test n7_ledger_state_wire_has_context_after_compact() {
  let state = LedgerStateV1 {
    subject: substrate.VerdictSubjectV1 {
      version: 1,
      direction: 0,
      source_kind: 0,
      transaction_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      source_key: #"",
      rejection_reason: None,
    },
    compact: wire_compact(),
    network_order_context: order.NetworkOrderContextV1 {
      body_network_mismatch: True,
    },
    source_kind: 1,
    input_index: 7,
    selected_input: Some(
      native_types.MidgardTxInput {
        tx_id: #"dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd",
        output_index: 9,
      },
    ),
    claimed_input: None,
    pre_utxos_root: #"eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
  }
  cbor.serialise(state) == #"d8799fd8799f0100005820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa40d87a80ffd8799fd8799f58200101010101010101010101010101010101010101010101010101010101010101582002020202020202020202020202020202020202020202020202020202020202025820030303030303030303030303030303030303030303030303030303030303030304050658200707070707070707070707070707070707070707070707070707070707070707582008080808080808080808080808080808080808080808080808080808080808085820090909090909090909090909090909090909090909090909090909090909090958200a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a0a58200b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b01ff58200c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0c0cd87a80ffd8799fd87a80ff0107d8799fd8799f5820dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd09ffffd87a805820eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeff"
}
