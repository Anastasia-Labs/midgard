//// Fixtures for the certified route of `validation_machine_field_door_v1`: a
//// normal-source transaction committing one variable-width field, and the
//// field grammar certificate over that field.
////
//// The certificate is computed here from the field's bytes alone, by a walk
//// of its §5.1 item heads that shares no code with the certifier or the door:
//// the item count, and the wrapper offset of every sixteenth item as a
//// two-byte waypoint. A vector that passes against it therefore shows the
//// door reading the certificate the certifier would have written.

use aiken/builtin
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use cardano/address
use cardano/assets.{PolicyId}
use cardano/transaction.{InlineDatum, Input, Output, OutputReference}
use midgard/field_grammar_certificate_v1.{
  Complete, FieldGrammarCertificateV1, field_grammar_certificate_asset_name,
  field_grammar_waypoint_bytes, field_grammar_waypoint_interval,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as binding_fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, encode_native_tx_compact_v1,
  encode_native_tx_field_preimage_lengths_v1,
  encode_native_tx_witness_set_compact, native_tx_id_for_version,
  tx_proof_commitment_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxCompact, NativeTxFieldPreimageLengthsV1,
  NativeTxWitnessSetCompact, native_tx_version_v1,
}
use midgard/native_tx_field_access_v1.{
  FieldCarriageV1, empty_field_commitment, encode_field_array_header,
  field_commitment,
}
use midgard/validation_machine_field_door_v1.{MachineFieldDoorV1}
use midgard/validation_trace_v1

/// The field grammar certificate policy the certified vectors run under.
pub const grammar_certificate_policy_id: PolicyId =
  #"6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b6b"

/// Who opened the certificate. Nothing on the read side looks at it.
pub const grammar_certificate_owner: ByteArray =
  #"6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c6c"

const grammar_certificate_utxo_id: ByteArray =
  #"6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d"

// ---------------------------------------------------------------------------
// Fields
// ---------------------------------------------------------------------------

/// `block` repeated `times` over, by doubling.
pub fn repeat(block: ByteArray, times: Int) -> ByteArray {
  binding_fixture.repeated_bytes_v1(block, times)
}

/// A §5.1 field of `count` copies of `wrapped`, one item with its wrapper.
pub fn uniform_field(count: Int, wrapped: ByteArray) -> ByteArray {
  bytearray.concat(encode_field_array_header(count), repeat(wrapped, count))
}

/// `payload` under its §5.1 byte-string wrapper.
pub fn wrap(payload: ByteArray) -> ByteArray {
  let length = bytearray.length(payload)
  if length <= 23 {
    bytearray.push(payload, 64 + length)
  } else if length <= 255 {
    bytearray.concat(#"58", bytearray.push(payload, length))
  } else {
    bytearray.concat(
      #"59",
      bytearray.concat(bytearray.from_int_big_endian(length, 2), payload),
    )
  }
}

/// A §5.1 field of the given payloads, each under its wrapper.
pub fn field_of(payloads: List<ByteArray>) -> ByteArray {
  do_field_of(payloads, 0, #"")
}

fn do_field_of(payloads: List<ByteArray>, count: Int, joined: ByteArray) {
  when payloads is {
    [] -> bytearray.concat(encode_field_array_header(count), joined)
    [payload, ..rest] ->
      do_field_of(rest, count + 1, bytearray.concat(joined, wrap(payload)))
  }
}

// ---------------------------------------------------------------------------
// The independent certificate
// ---------------------------------------------------------------------------

/// The array header's width, read from its first byte.
fn array_header_length(preimage: ByteArray) -> Int {
  let first = builtin.index_bytearray(preimage, 0)
  if first < 152 {
    1
  } else if first == 152 {
    2
  } else {
    3
  }
}

/// The byte length of the wrapped item whose head starts at `offset`.
fn wrapped_length(preimage: ByteArray, offset: Int) -> Int {
  let info = builtin.index_bytearray(preimage, offset) - 64
  if info < 24 {
    1 + info
  } else if info == 24 {
    2 + builtin.index_bytearray(preimage, offset + 1)
  } else {
    3 + bytearray.to_int_big_endian(bytearray.slice(preimage, offset + 1, 2))
  }
}

/// `(item count, waypoints)` of a §5.1 field, walked here head by head.
pub fn field_grammar(preimage: ByteArray) -> Pair<Int, ByteArray> {
  do_field_grammar(
    preimage,
    bytearray.length(preimage),
    array_header_length(preimage),
    0,
    #"",
  )
}

fn do_field_grammar(
  preimage: ByteArray,
  total: Int,
  offset: Int,
  count: Int,
  waypoints: ByteArray,
) -> Pair<Int, ByteArray> {
  if offset >= total {
    Pair(count, waypoints)
  } else {
    let waypoints = if count % field_grammar_waypoint_interval == 0 {
      bytearray.concat(
        waypoints,
        bytearray.from_int_big_endian(offset, field_grammar_waypoint_bytes),
      )
    } else {
      waypoints
    }
    do_field_grammar(
      preimage,
      total,
      offset + wrapped_length(preimage, offset),
      count + 1,
      waypoints,
    )
  }
}

/// The `Complete` certificate over `preimage` as field `field_index` of
/// transaction `tx_id`.
pub fn complete_certificate(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> FieldGrammarCertificateV1 {
  let Pair(item_count, waypoints) = field_grammar(preimage)
  Complete {
    owner: grammar_certificate_owner,
    tx_id,
    field_index,
    field_hash: field_commitment(preimage),
    total_length: bytearray.length(preimage),
    item_count,
    waypoints,
  }
}

/// `certificate` as the reference input the door reads: at `policy_id`'s
/// script address, holding one of its certificate tokens.
pub fn certificate_input_under(
  policy_id: PolicyId,
  certificate: FieldGrammarCertificateV1,
) -> Input {
  let datum: Data = certificate
  Input {
    output_reference: OutputReference {
      transaction_id: grammar_certificate_utxo_id,
      output_index: 0,
    },
    output: Output {
      address: address.from_script(policy_id),
      value: assets.from_lovelace(5_000_000)
        |> assets.add(policy_id, field_grammar_certificate_asset_name, 1),
      datum: InlineDatum(datum),
      reference_script: None,
    },
  }
}

/// The honest certificate input for field `field_index` of `tx_id`.
pub fn certificate_input(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> Input {
  certificate_input_under(
    grammar_certificate_policy_id,
    complete_certificate(tx_id, field_index, preimage),
  )
}

// ---------------------------------------------------------------------------
// Carriage
// ---------------------------------------------------------------------------

/// How a vector delivers the field and where its grammar certificate sits.
pub type CertifiedDeliveryV1 {
  door: MachineFieldDoorV1,
  carriage: FieldCarriageV1,
  grammar_index: Int,
}

/// Tier 1: the preimage rides the redeemer; the certificate is reference
/// input 0.
pub fn inline_delivery(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> CertifiedDeliveryV1 {
  CertifiedDeliveryV1 {
    door: MachineFieldDoorV1 {
      reference_inputs: [certificate_input(tx_id, field_index, preimage)],
      certificate_policy_id: binding_fixture.test_certificate_policy_id,
    },
    carriage: native_tx_field_access_v1.Inline { preimage },
    grammar_index: 0,
  }
}

/// Tier 3, the only carriage for a field wider than an L1 transaction: the
/// field-preimage certificate at reference input 0, its chunks after it, and
/// the grammar certificate last.
pub fn certified_delivery(
  tx_id: ByteArray,
  field_index: Int,
  preimage: ByteArray,
) -> CertifiedDeliveryV1 {
  let carriage_inputs = binding_fixture.certified_reference_inputs_v1(
    tx_id,
    field_index,
    preimage,
  )
  CertifiedDeliveryV1 {
    door: MachineFieldDoorV1 {
      reference_inputs: list_append(
        carriage_inputs,
        certificate_input(tx_id, field_index, preimage),
      ),
      certificate_policy_id: binding_fixture.test_certificate_policy_id,
    },
    carriage: binding_fixture.certified_carriage_v1(preimage),
    grammar_index: list_length(carriage_inputs),
  }
}

fn list_append(inputs: List<Input>, last: Input) -> List<Input> {
  when inputs is {
    [] -> [last]
    [first, ..rest] -> [first, ..list_append(rest, last)]
  }
}

fn list_length(inputs: List<Input>) -> Int {
  when inputs is {
    [] -> 0
    [_, ..rest] -> 1 + list_length(rest)
  }
}

// ---------------------------------------------------------------------------
// A normal-source transaction committing one field
// ---------------------------------------------------------------------------

/// A transaction whose only non-empty field is `preimage` at `field_index`,
/// with every anchor the proof source verifies.
pub type FieldSourceV1 {
  field_index: Int,
  preimage: ByteArray,
  tx_id: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  transaction_commitment: ByteArray,
}

pub fn field_source(field_index: Int, preimage: ByteArray) -> FieldSourceV1 {
  let slot = fn(index) {
    if index == field_index {
      field_commitment(preimage)
    } else {
      empty_field_commitment
    }
  }
  let length = fn(index) {
    if index == field_index {
      bytearray.length(preimage)
    } else {
      1
    }
  }
  let body = NativeTxBodyCompact {
    spend_inputs_hash: slot(0),
    reference_inputs_hash: slot(1),
    outputs_hash: slot(2),
    fee: 0,
    validity_interval_start: 0,
    validity_interval_end: 0,
    required_observers_hash: slot(3),
    required_signers_hash: slot(4),
    mint_hash: slot(5),
    script_integrity_hash: empty_field_commitment,
    auxiliary_data_hash: empty_field_commitment,
    network_id: 255,
  }
  let witness_set_compact_cbor = encode_native_tx_witness_set_compact(
    NativeTxWitnessSetCompact {
      addr_tx_wits_hash: slot(7),
      script_tx_wits_hash: slot(6),
      redeemer_tx_wits_hash: slot(8),
    },
  )
  let compact_cbor = encode_native_tx_compact_v1(
    NativeTxCompact {
      body,
      witness_set_hash: blake2b_256(witness_set_compact_cbor),
      validity_code: 0,
    },
  )
  let field_preimage_lengths_cbor = encode_native_tx_field_preimage_lengths_v1(
    NativeTxFieldPreimageLengthsV1 {
      spend_inputs: length(0),
      reference_inputs: length(1),
      outputs: length(2),
      required_observers: length(3),
      required_signers: length(4),
      mint: length(5),
      address_witnesses: length(7),
      script_witnesses: length(6),
      redeemers: length(8),
    },
  )
  FieldSourceV1 {
    field_index,
    preimage,
    tx_id: native_tx_id_for_version(
      native_tx_version_v1,
      encode_native_tx_body_compact(body),
    ),
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
    transaction_commitment: tx_proof_commitment_v1(
      validation_trace_v1.Normal,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ),
  }
}

/// The pending normal-source machine state over `source` at `(phase,
/// program_counter)`, its work root committing `work_witness_cbor`.
pub fn source_state(
  source: FieldSourceV1,
  context_cbor: ByteArray,
  phase: validation_trace_v1.ValidationPhase,
  program_counter: Int,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    transaction_id: source.tx_id,
    transaction_commitment: source.transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context_cbor,
    ),
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
    phase,
    program_counter,
    work_root: validation_trace_v1.hash_work_witness(
      phase,
      program_counter,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
  }
}
