//// Actual body compact/field-door custody controls; no prover legality Bool.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/field_item_width_illegal/rule
use midgard/fraud_proofs/field_item_width_illegal/step_02
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyFieldOpening, opened_whole_field_view,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_forced_tx_compact_v1, encode_forced_tx_compact_v1,
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/types.{
  ForcedTxCompact, NativeTxBodyCompact, TxCompactView,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_field_access_v1.{
  Inline, encode_field_array_header, field_commitment,
}
use midgard/rejection_reason_v1 as reason
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, submitted_source,
}

fn hash_item(prefix: ByteArray, first: ByteArray) -> ByteArray {
  prefix
    |> bytearray.concat(first)
    |> bytearray.concat(
        #"000102030405060708090a0b0c0d0e0f101112131415161718191a",
      )
}

fn fixture_at_field(
  field: Int,
  items: List<ByteArray>,
  selected: Int,
  rejected: Option<reason.RejectionReasonV1>,
  substitute: Bool,
) -> rule.AuthenticatedItemV1 {
  let preimage = encode_field_array_header(list.length(items))
    |> bytearray.concat(
        list.foldl(
          items,
          #"",
          fn(item, prior) { bytearray.concat(prior, item) },
        ),
      )
  let base = decode_forced_tx_compact_v1(
    submitted_source(empty_v1_transaction()).compact_cbor,
  )
  let compact = ForcedTxCompact {
    ..base,
    body: NativeTxBodyCompact {
      ..base.body,
      spend_inputs_hash: if field == 0 {
        field_commitment(preimage)
      } else {
        base.body.spend_inputs_hash
      },
      reference_inputs_hash: if field == 1 {
        field_commitment(preimage)
      } else {
        base.body.reference_inputs_hash
      },
      required_signers_hash: if field == 4 {
        field_commitment(preimage)
      } else {
        base.body.required_signers_hash
      },
    },
  }
  let compact_cbor = encode_forced_tx_compact_v1(compact)
  let subject = substrate.VerdictSubjectV1 {
    version: 1,
    direction: if rejected == None {
      0
    } else {
      1
    },
    source_kind: 1,
    transaction_id: native_tx_id_for_version(
      1,
      encode_native_tx_body_compact(compact.body),
    ),
    source_key: #"00",
    rejection_reason: rejected,
  }
  let bound = rule.bind_coordinate_v1(
    subject,
    field,
    selected,
    TxCompactView {
      body: compact.body,
      witness_set_hash: compact.witness_set_hash,
      validity_code: None,
    },
  )
  let view = opened_whole_field_view(
    BodyFieldOpening {
      native_tx_compact_cbor: compact_cbor,
      carriage: Inline {
        preimage: if substitute {
          #"80"
        } else {
          preimage
        },
      },
    },
    BodyAnchor { tx_id: subject.transaction_id, source_kind: 1 },
    field,
    [],
    #"",
  )
  rule.authenticate_item_legality_v1(bound, view)
}

fn fixture(
  items: List<ByteArray>,
  selected: Int,
  rejected: Option<reason.RejectionReasonV1>,
  substitute: Bool,
) -> rule.AuthenticatedItemV1 {
  fixture_at_field(4, items, selected, rejected, substitute)
}

fn input_item(index: Int) -> ByteArray {
  #"5826825820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f19"
    |> bytearray.concat(bytearray.from_int_big_endian(index, 2))
}

fn ordered_item_control(field: Int, repeated: Bool, rejected: Bool) -> Bool {
  let low = if field == 4 {
    hash_item(#"581c", #"01")
  } else {
    input_item(8)
  }
  let high = if repeated {
    low
  } else if field == 4 {
    hash_item(#"581c", #"02")
  } else {
    input_item(9)
  }
  let claimed = if rejected {
    Some(reason.FieldItemWidthIllegal { field_index: field, item_index: 1 })
  } else {
    None
  }
  rule.terminal_contradiction_v1(
    fixture_at_field(field, [high, low], 1, claimed, False),
  )
}

test fiwi_descending_spends_convict_accepted() {
  ordered_item_control(0, False, False)
}

test fiwi_descending_spends_uphold_exact_rejection() {
  !ordered_item_control(0, False, True)
}

test fiwi_descending_references_convict_accepted() {
  ordered_item_control(1, False, False)
}

test fiwi_descending_references_uphold_exact_rejection() {
  !ordered_item_control(1, False, True)
}

test fiwi_descending_signers_do_not_convict_accepted() {
  !ordered_item_control(4, False, False)
}

test fiwi_descending_signers_refute_exact_rejection() {
  ordered_item_control(4, False, True)
}

test fiwi_repeated_signers_do_not_convict_accepted() {
  !ordered_item_control(4, True, False)
}

test fiwi_repeated_signers_refute_exact_rejection() {
  ordered_item_control(4, True, True)
}

test fiwi_repeated_spend_is_not_an_item_shape_fault() {
  !ordered_item_control(0, True, False)
}

test fiwi_repeated_reference_is_not_an_item_shape_fault() {
  !ordered_item_control(1, True, False)
}

fn bad_pair() -> List<ByteArray> {
  [hash_item(#"581d", #"01"), hash_item(#"581d", #"02")]
}

fn claim(index: Int) -> Option<reason.RejectionReasonV1> {
  Some(reason.FieldItemWidthIllegal { field_index: 4, item_index: index })
}

test n7_fiwi_actual_earlier_illegal_item_convicts_later_illegal_claim() {
  rule.terminal_contradiction_v1(fixture(bad_pair(), 0, claim(1), False))
}

test n7_fiwi_actual_later_illegal_item_does_not_convict_true_first_fault() {
  !rule.terminal_contradiction_v1(fixture(bad_pair(), 1, claim(0), False))
}

test n7_fiwi_actual_true_first_illegal_claim_is_refused() {
  !rule.terminal_contradiction_v1(fixture(bad_pair(), 0, claim(0), False))
}

test n7_fiwi_actual_accepted_bad_wrapper_is_convicted() {
  rule.terminal_contradiction_v1(fixture(bad_pair(), 0, None, False))
}

test n7_fiwi_actual_legal_item_at_exact_claim_is_convicted() {
  rule.terminal_contradiction_v1(
    fixture([hash_item(#"581c", #"01")], 0, claim(0), False),
  )
}

test n7_fiwi_actual_legal_other_coordinate_cannot_refute_claim() {
  !rule.terminal_contradiction_v1(
    fixture(
      [hash_item(#"581c", #"01"), hash_item(#"581c", #"02")],
      0,
      claim(1),
      False,
    ),
  )
}

test n7_fiwi_actual_out_of_domain_exact_claim_is_convicted() {
  rule.terminal_contradiction_v1(
    fixture([hash_item(#"581c", #"01")], 1, claim(1), False),
  )
}

test n7_fiwi_actual_out_of_domain_other_coordinate_is_refused() {
  !rule.terminal_contradiction_v1(
    fixture([hash_item(#"581c", #"01")], 2, claim(1), False),
  )
}

test n7_fiwi_actual_accepted_out_of_domain_is_refused() {
  !rule.terminal_contradiction_v1(fixture([], 0, None, False))
}

test n7_fiwi_actual_earlier_item_convicts_later_semantic_arm() {
  rule.terminal_contradiction_v1(
    fixture(bad_pair(), 0, Some(reason.NetworkIdMismatch), False),
  )
}

test n7_fiwi_actual_source_substitution_is_refused() fail {
  rule.terminal_contradiction_v1(fixture(bad_pair(), 0, None, True))
}

fn abi_subject() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f",
    source_key: #"",
    rejection_reason: None,
  }
}

fn abi_bound(validity: Option<Int>) -> rule.BoundCoordinateV1 {
  let hash = #"0000000000000000000000000000000000000000000000000000000000000000"
  rule.BoundCoordinateV1 {
    subject: abi_subject(),
    field_index: 5,
    item_index: 0,
    compact: TxCompactView {
      body: NativeTxBodyCompact {
        spend_inputs_hash: hash,
        reference_inputs_hash: hash,
        outputs_hash: hash,
        fee: 42,
        validity_interval_start: 3,
        validity_interval_end: 99,
        required_observers_hash: hash,
        required_signers_hash: hash,
        mint_hash: hash,
        script_integrity_hash: hash,
        auxiliary_data_hash: hash,
        network_id: 255,
      },
      witness_set_hash: #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      validity_code: validity,
    },
  }
}

fn abi_item(observation: rule.ItemObservationV1) -> rule.AuthenticatedItemV1 {
  rule.AuthenticatedItemV1 {
    subject: abi_subject(),
    field_index: 5,
    item_index: 0,
    observation,
  }
}

test fiwi_bound_abi_carries_compact_and_some_validity() {
  rule.encode_bound_coordinate_v1(abi_bound(Some(0))) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d8799fd8799f582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000182a031863582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000018ffff5820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd8799f00ffffff"
}

test fiwi_bound_abi_carries_compact_and_no_validity() {
  rule.encode_bound_coordinate_v1(abi_bound(None)) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d8799fd8799f582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000182a031863582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000058200000000000000000000000000000000000000000000000000000000000000000582000000000000000000000000000000000000000000000000000000000000000005820000000000000000000000000000000000000000000000000000000000000000018ffff5820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffd87a80ffff"
}

test fiwi_item_abi_preserves_legal_observation() {
  rule.encode_authenticated_item_v1(
    abi_item(rule.ItemLegality { item_is_legal: True }),
  ) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d87a9fd87a80ffff"
}

test fiwi_item_abi_preserves_illegal_observation() {
  rule.encode_authenticated_item_v1(
    abi_item(rule.ItemLegality { item_is_legal: False }),
  ) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d87a9fd87980ffff"
}

test fiwi_item_abi_preserves_out_of_domain_observation() {
  rule.encode_authenticated_item_v1(abi_item(rule.ItemOutOfDomain)) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff0500d87980ff"
}

test fiwi_step02_abi_preserves_absent_opening() {
  cbor.serialise(
    step_02.Args { input_index: 0, output_index: 1, opening: None },
  ) == #"d8799f0001d87a80ff"
}

test fiwi_step02_abi_preserves_present_opening() {
  cbor.serialise(
    step_02.Args {
      input_index: 0,
      output_index: 1,
      opening: Some(
        BodyFieldOpening {
          native_tx_compact_cbor: #"01",
          carriage: Inline { preimage: #"80" },
        },
      ),
    },
  ) == #"d8799f0001d8799fd8799f4101d8799f4180ffffffff"
}
