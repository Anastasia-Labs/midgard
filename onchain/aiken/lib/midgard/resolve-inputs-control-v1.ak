use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/compact.{tx_proof_commitment_v1}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_input_cbor}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_raw
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine/witness.{resolution_schedule_node_hash}
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
    tx_proof_commitment_v1(
      pre.source_kind,
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

/// The five-item pending record of a resolve-inputs control, with the nested
/// ledger-output-proof control kept as canonical bytes: the carrier binds it,
/// the stage yield decodes it.
pub type PendingRawV1 {
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  output_proof_cbor: ByteArray,
}

pub fn pending_raw_from_cbor(pending_cbor: ByteArray) -> PendingRawV1 {
  expect pending_cbor != #"00"
  expect Some(data) = cbor.deserialise(pending_cbor)
  expect
      [
        source_kind_data, key_data, next_schedule_hash_data,
        descriptor_cbor_data, output_proof_cbor_data,
      ]
    = builtin.un_list_data(data)
  PendingRawV1 {
    source_kind: builtin.un_i_data(source_kind_data),
    key: builtin.un_b_data(key_data),
    next_schedule_hash: builtin.un_b_data(next_schedule_hash_data),
    descriptor_cbor: builtin.un_b_data(descriptor_cbor_data),
    output_proof_cbor: builtin.un_b_data(output_proof_cbor_data),
  }
}

pub fn encode_pending(pending: PendingRawV1) -> ByteArray {
  encode_pending_raw(
    pending.source_kind,
    pending.key,
    pending.next_schedule_hash,
    pending.descriptor_cbor,
    pending.output_proof_cbor,
  )
}

/// Every clause of `witness.resolve_inputs_control_is_bound` for a control
/// with a pending record, except the three ledger-output-proof scalar
/// equalities and `control_is_well_formed`, which `control_raw_lop_is_pinned`
/// and the selected stage yield discharge respectively.
pub fn control_raw_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlRawV1,
  pending: PendingRawV1,
) -> Bool {
  control_raw_is_bound_with_descriptor(
    pre,
    witness,
    control,
    pending,
    ledger_output_commitment_v1.decode(pending.descriptor_cbor),
  )
}

/// `control_raw_is_bound` with the pending descriptor already decoded, so a
/// dispatcher that also pins the ledger-output-proof scalars decodes the
/// descriptor once instead of twice. `descriptor` MUST be
/// `ledger_output_commitment_v1.decode(pending.descriptor_cbor)` — the wrapper
/// above and the call sites keep that invariant.
pub fn control_raw_is_bound_with_descriptor(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlRawV1,
  pending: PendingRawV1,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> Bool {
  let input = decode_midgard_tx_input_cbor(pending.key)
  and {
    control.pending_cbor == encode_pending(pending),
    tx_proof_commitment_v1(
      pre.source_kind,
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
    control.cursor > 0,
    pending.source_kind == 0 || pending.source_kind == 1,
    resolution_schedule_node_hash(
      pending.source_kind,
      pending.key,
      pending.next_schedule_hash,
    ) == control.remaining_schedule_hash,
    input.output_index == descriptor.output_index,
    witness.work_witness_cbor == encode_control_raw(control),
  }
}

/// The canonical successor work-witness bytes for a step that replaces the
/// pending record's `output_proof_cbor` and changes nothing else. Equals
/// `encode_control_raw(control with pending_cbor = encode_pending(pending with
/// output_proof_cbor = next_control_cbor))` whenever `control_raw_is_bound`
/// holds (the encoding is per-field concatenation: `witness_cbor` ends with
/// `encode_definite_bytes(pending_cbor)` followed by the 34-byte
/// `resolution_schedule_hash` field, and `pending_cbor` ends with
/// `encode_definite_bytes(output_proof_cbor)`), and both tails are re-checked
/// here so the splice fails closed on unbound inputs.
pub fn splice_pending_successor_v1(
  witness_cbor: ByteArray,
  pending_cbor: ByteArray,
  output_proof_cbor: ByteArray,
  next_control_cbor: ByteArray,
) -> ByteArray {
  let old_item = encode_definite_bytes(output_proof_cbor)
  let item_offset = bytearray.length(pending_cbor) - bytearray.length(old_item)
  expect item_offset >= 0
  expect
    builtin.slice_bytearray(
      item_offset,
      bytearray.length(old_item),
      pending_cbor,
    ) == old_item
  let new_pending = builtin.slice_bytearray(0, item_offset, pending_cbor)
    |> bytearray.concat(encode_definite_bytes(next_control_cbor))
  let old_field = encode_definite_bytes(pending_cbor)
  let field_offset = bytearray.length(witness_cbor) - bytearray.length(
    old_field,
  ) - 34
  expect field_offset >= 0
  expect
    builtin.slice_bytearray(
      field_offset,
      bytearray.length(old_field),
      witness_cbor,
    ) == old_field
  builtin.slice_bytearray(0, field_offset, witness_cbor)
    |> bytearray.concat(encode_definite_bytes(new_pending))
    |> bytearray.concat(
        builtin.slice_bytearray(
          field_offset + bytearray.length(old_field),
          34,
          witness_cbor,
        ),
      )
}

/// The three ledger-output-proof scalars the monolith states explicitly:
/// items 2, 3 and 4 of the pinned control equal the descriptor's output
/// index, total length and item commitment. `ledger_output_proof_raw.open`
/// supplies the remaining outer well-formedness (version, stage range,
/// canonical re-encoding).
pub fn control_raw_lop_is_pinned(
  frame: ledger_output_proof_raw.Frame,
  descriptor_cbor: ByteArray,
) -> Bool {
  control_raw_lop_is_pinned_with_descriptor(
    frame,
    ledger_output_commitment_v1.decode(descriptor_cbor),
  )
}

/// `control_raw_lop_is_pinned` with the descriptor already decoded; the same
/// single-decode contract as `control_raw_is_bound_with_descriptor`.
pub fn control_raw_lop_is_pinned_with_descriptor(
  frame: ledger_output_proof_raw.Frame,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> Bool {
  and {
    ledger_output_proof_raw.integer(frame, 2) == descriptor.output_index,
    ledger_output_proof_raw.integer(frame, 3) == descriptor.total_length,
    ledger_output_proof_raw.bytes(frame, 4) == descriptor.item_commitment,
  }
}
