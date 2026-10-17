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

fn run(
  observed_kind: Int,
  observed_index: Int,
  r: reason.RejectionReasonV1,
  substitute_claim: Bool,
  omit_claim: Bool,
  network_id: Int,
) -> Bool {
  let spend = preimage([10, 30])
  let reference = preimage([20])
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
  let claimed_kind = when r is {
    reason.InputNotFound { source_kind, .. } -> source_kind
    reason.InputSpentOutputNonCanonical { source_kind, .. } -> source_kind
    reason.SpendInputSignerMissing { .. } -> 0
    _ -> observed_kind
  }
  let claimed = select.claimed_input_v1(
    subject,
    observed_kind,
    view,
    if omit_claim || claimed_kind == observed_kind {
      None
    } else {
      Some(
        opening(
          compact_cbor,
          if substitute_claim {
            #"80"
          } else if claimed_kind == 0 {
            spend
          } else {
            reference
          },
        ),
      )
    },
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
