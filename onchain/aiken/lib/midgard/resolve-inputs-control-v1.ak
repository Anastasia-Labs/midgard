use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_trace_v1

pub type ResolveInputsControlRawV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  cursor: Int,
  accumulator: ByteArray,
  remaining_schedule_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  pending_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
}

pub fn control_raw_from_witness(
  witness_cbor: ByteArray,
) -> ResolveInputsControlRawV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data, cursor_data,
        accumulator_data, remaining_schedule_hash_data, signer_count_data,
        signer_frontier_commitment_data, pending_cbor_data,
        resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(data)
  ResolveInputsControlRawV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    cursor: builtin.un_i_data(cursor_data),
    accumulator: builtin.un_b_data(accumulator_data),
    remaining_schedule_hash: builtin.un_b_data(remaining_schedule_hash_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    pending_cbor: builtin.un_b_data(pending_cbor_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

pub fn encode_control_raw(control: ResolveInputsControlRawV1) -> ByteArray {
  expect control.cursor >= 0
  expect bytearray.length(control.accumulator) == 32
  expect bytearray.length(control.remaining_schedule_hash) == 32
  expect control.signer_count >= 0
  expect bytearray.length(control.signer_frontier_commitment) == 32
  expect bytearray.length(control.resolution_schedule_hash) == 32
  #"8b"
    |> bytearray.concat(encode_definite_bytes(control.compact_cbor))
    |> bytearray.concat(encode_definite_bytes(control.witness_set_compact_cbor))
    |> bytearray.concat(
        encode_definite_bytes(control.field_preimage_lengths_cbor),
      )
    |> bytearray.concat(encode_definite_bytes(control.context_cbor))
    |> bytearray.concat(cbor.serialise(control.cursor))
    |> bytearray.concat(encode_definite_bytes(control.accumulator))
    |> bytearray.concat(encode_definite_bytes(control.remaining_schedule_hash))
    |> bytearray.concat(cbor.serialise(control.signer_count))
    |> bytearray.concat(
        encode_definite_bytes(control.signer_frontier_commitment),
      )
    |> bytearray.concat(encode_definite_bytes(control.pending_cbor))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
}

pub fn control_no_pending_from_witness(
  witness_cbor: ByteArray,
) -> ResolveInputsControlRawV1 {
  let control = control_raw_from_witness(witness_cbor)
  expect control.pending_cbor == #"00"
  control
}

pub fn control_no_pending_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlRawV1,
) -> Bool {
  and {
    control.pending_cbor == #"00",
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.accumulator) == 32,
    bytearray.length(control.remaining_schedule_hash) == 32,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.cursor > 1 || control.remaining_schedule_hash == control.resolution_schedule_hash,
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.cursor >= 0,
    witness.work_witness_cbor == encode_control_raw(control),
  }
}

pub fn encode_pending_raw(
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  output_proof_cbor: ByteArray,
) -> ByteArray {
  #"85"
    |> bytearray.concat(cbor.serialise(source_kind))
    |> bytearray.concat(encode_definite_bytes(key))
    |> bytearray.concat(encode_definite_bytes(next_schedule_hash))
    |> bytearray.concat(encode_definite_bytes(descriptor_cbor))
    |> bytearray.concat(encode_definite_bytes(output_proof_cbor))
}
