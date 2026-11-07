use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/redeemer
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_redeemer_witness}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}

fn item(
  purpose: native_types.MidgardRedeemerPurpose,
  data: ByteArray,
) -> ByteArray {
  encode_midgard_redeemer_witness(
    native_types.MidgardRedeemerWitness {
      purpose,
      index: 7,
      redeemer_cbor: data,
      execution_units: native_types.MidgardExecutionUnits {
        memory: 12345,
        steps: 987654321,
      },
    },
  )
}

test actual_redeemer_data_and_declared_units_are_preserved_for_each_purpose_kind() {
  expect Some(spend) = redeemer.from_item_v1(
    PurposeCoordinateV1 { kind: 0, index: 7 },
    2,
    item(native_types.SpendRedeemer, #"182a"),
  )
  expect Some(mint) = redeemer.from_item_v1(
    PurposeCoordinateV1 { kind: 1, index: 7 },
    2,
    item(native_types.MintRedeemer, #"182a"),
  )
  expect Some(observe) = redeemer.from_item_v1(
    PurposeCoordinateV1 { kind: 2, index: 7 },
    2,
    item(native_types.RewardRedeemer, #"182a"),
  )
  expect Some(receive) = redeemer.from_item_v1(
    PurposeCoordinateV1 { kind: 3, index: 7 },
    2,
    item(native_types.ReceiveRedeemer, #"182a"),
  )
  and {
    spend.execution_memory == 12345,
    spend.execution_steps == 987654321,
    spend.data_length == 2,
    mint.execution_memory == spend.execution_memory,
    observe.execution_steps == spend.execution_steps,
    receive.data_length == spend.data_length,
  }
}

test wrong_pointer_or_purpose_does_not_match() {
  and {
    redeemer.from_item_v1(
      PurposeCoordinateV1 { kind: 0, index: 6 },
      0,
      item(native_types.SpendRedeemer, #"00"),
    ) == None,
    redeemer.from_item_v1(
      PurposeCoordinateV1 { kind: 2, index: 7 },
      0,
      item(native_types.MintRedeemer, #"00"),
    ) == None,
  }
}

test large_actual_data_is_committed_without_copying_it_into_thread_state() {
  let actual = encode_definite_bytes(
    bytearray.concat(
      builtin.replicate_byte(8192, 0),
      builtin.replicate_byte(8192, 0),
    ),
  )
  expect Some(found) = redeemer.from_item_v1(
    PurposeCoordinateV1 { kind: 3, index: 7 },
    0,
    item(native_types.ReceiveRedeemer, actual),
  )
  and {
    found.data_length == 16387,
    bytearray.length(found.item_commitment) == 32,
    bytearray.length(cbor.serialise(found)) < 200,
    found.execution_memory == 12345,
    found.execution_steps == 987654321,
  }
}

test trailing_redeemer_bytes_are_refused() fail {
  redeemer.from_item_v1(
    PurposeCoordinateV1 { kind: 3, index: 7 },
    0,
    bytearray.concat(item(native_types.ReceiveRedeemer, #"00"), #"00"),
  ) != None
}

fn pointer_scan(
  witnesses: List<native_types.MidgardRedeemerWitness>,
) -> (redeemer.PointerStateV1, ByteArray, FieldOpeningV1) {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      witness_set: native_types.MidgardTransactionWitnessSet {
        ..tx.witness_set,
        redeemer_tx_wits: witnesses,
      },
    },
  )
  let opening = fixture.inline_witness_opening(
    block.compact_cbor,
    block.witness_set_compact,
    list.map(witnesses, encode_midgard_redeemer_witness),
  )
  let bound = BoundV1 {
    subject: VerdictSubjectV1 {
      version: 1,
      direction: 0,
      source_kind: 0,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    compact_cbor: block.compact_cbor,
    network_order_context: NetworkOrderContextV1 {
      body_network_mismatch: False,
    },
    event_pre_root: #"4444444444444444444444444444444444444444444444444444444444444444",
  }
  let Pair(initial, checkpoint) = cursor.initial_v1(
    bound,
    8,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let (ready, checkpoint, complete) = cursor.certify_with_completion_at_v1(
    initial,
    8,
    opening,
    checkpoint,
    16,
    [],
    fixture.test_certificate_policy_id,
  )
  (
    redeemer.PointerStateV1 {
      cursor: ready,
      coordinate: PurposeCoordinateV1 { kind: 3, index: 7 },
      found: False,
      complete,
    },
    checkpoint,
    opening,
  )
}

fn witness(
  purpose: native_types.MidgardRedeemerPurpose,
  index: Int,
) -> native_types.MidgardRedeemerWitness {
  native_types.MidgardRedeemerWitness {
    purpose,
    index,
    redeemer_cbor: #"00",
    execution_units: native_types.MidgardExecutionUnits { memory: 1, steps: 2 },
  }
}

test certified_pointer_scan_resumes_and_requires_the_actual_tag_and_index() {
  let (initial, checkpoint, opening) = pointer_scan(
    [
      witness(native_types.SpendRedeemer, 7),
      witness(native_types.ReceiveRedeemer, 8),
      witness(native_types.ReceiveRedeemer, 7),
    ],
  )
  let Pair(first, checkpoint) = redeemer.advance_pointers_v1(
    initial,
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(second, checkpoint) = redeemer.advance_pointers_v1(
    first,
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(third, _) = redeemer.advance_pointers_v1(
    second,
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  and {
    !first.found,
    !first.complete,
    !second.found,
    !second.complete,
    third.found,
    third.complete,
  }
}

test exhausted_unrelated_pointers_prove_only_local_absence() {
  let (initial, checkpoint, opening) = pointer_scan(
    [
      witness(native_types.SpendRedeemer, 7),
      witness(native_types.ReceiveRedeemer, 8),
    ],
  )
  let Pair(actual, _) = redeemer.advance_pointers_v1(
    initial,
    opening,
    checkpoint,
    2,
    [],
    fixture.test_certificate_policy_id,
  )
  and {
    actual.complete,
    !actual.found,
  }
}

test empty_pointer_field_is_complete_after_grammar() {
  let (initial, _, _) = pointer_scan([])
  and {
    initial.complete,
    !initial.found,
  }
}

test pointer_prefix_maps_all_four_purpose_kinds() {
  and {
    redeemer.pointer_matches_v1(
      PurposeCoordinateV1 { kind: 0, index: 7 },
      redeemer.pointer_from_prefix_v1(#"840007"),
    ),
    redeemer.pointer_matches_v1(
      PurposeCoordinateV1 { kind: 1, index: 7 },
      redeemer.pointer_from_prefix_v1(#"840107"),
    ),
    redeemer.pointer_matches_v1(
      PurposeCoordinateV1 { kind: 2, index: 7 },
      redeemer.pointer_from_prefix_v1(#"840307"),
    ),
    redeemer.pointer_matches_v1(
      PurposeCoordinateV1 { kind: 3, index: 7 },
      redeemer.pointer_from_prefix_v1(#"840607"),
    ),
  }
}

test pointer_prefix_keeps_the_full_unsigned_index() {
  redeemer.pointer_from_prefix_v1(#"84061bffffffffffffffff") == Pair(
    6,
    18446744073709551615,
  )
}

test nonminimal_pointer_index_refuses() fail {
  redeemer.pointer_from_prefix_v1(#"84061807") == Pair(6, 7)
}

test resumed_pointer_scan_refuses_an_old_checkpoint() fail {
  let (initial, checkpoint, opening) = pointer_scan(
    [
      witness(native_types.SpendRedeemer, 7),
      witness(native_types.ReceiveRedeemer, 7),
    ],
  )
  let Pair(first, _) = redeemer.advance_pointers_v1(
    initial,
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  )
  redeemer.advance_pointers_v1(
    first,
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  ).1st.complete
}
