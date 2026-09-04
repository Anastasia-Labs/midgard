use aiken/builtin
use aiken/cbor
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, native_tx_proof_commitment_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1.{
  reject_field_preimage_size, reject_invalid_field_type,
}
use midgard/validation_machine/machine_types.{
  CanonicalDecodeControlV1, CanonicalDecodeItemObservationV1,
  CanonicalDecodeItemProofV1, CanonicalDecodeItemSourceV1, NoAuxiliaryWitness,
  TransactionFieldChunkWitness, TransactionFieldItemWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  canonical_argument_header_size, rejected_successor_is_exact,
  transaction_field_commitment, transaction_field_item_encoded_length,
  transaction_field_preimage_length,
}
use midgard/validation_machine/witness.{
  canonical_decode_control_from_witness, canonical_decode_control_is_bound,
  encode_compact_binding_witness, encode_static_rules_witness,
  encode_transaction_field_scan_witness,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_trace_v1

fn canonical_scan_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  field_index: Int,
  item_index: Int,
  chunk_index: Int,
  item_count: Int,
  encoded_length: Int,
) -> Bool {
  and {
    post.phase == validation_trace_v1.CanonicalDecode,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      pre.program_counter + 1,
      encode_transaction_field_scan_witness(
        compact_cbor,
        witness_set_compact_cbor,
        field_preimage_lengths_cbor,
        context_cbor,
        field_index,
        item_index,
        chunk_index,
        item_count,
        encoded_length,
      ),
    ),
  }
}

fn canonical_scan_field_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  field_index: Int,
) -> Bool {
  if field_index < 8 {
    canonical_scan_successor_is_exact(
      pre,
      post,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
      field_index + 1,
      0,
      0,
      -1,
      0,
    )
  } else {
    let compact_binding_witness_cbor = encode_compact_binding_witness(
      pre.transaction_id,
      pre.transaction_commitment,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
    )
    and {
      post.phase == validation_trace_v1.CompactBinding,
      post.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.CompactBinding,
        pre.program_counter + 1,
        compact_binding_witness_cbor,
      ),
    }
  }
}

fn verify_canonical_decode_empty(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: CanonicalDecodeControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    control.field_preimage_lengths_cbor,
  )
  let expected_field_length = transaction_field_preimage_length(
    field_lengths,
    control.field_index,
  )
  let expected_field_commitment = transaction_field_commitment(
    verified.tx_compact,
    witness_set,
    control.field_index,
  )
  and {
    canonical_decode_control_is_bound(pre, witness, control, verified),
    expected_field_commitment == native_tx_field_access_v1.empty_field_commitment,
    control.item_index == 0,
    control.chunk_index == 0,
    control.item_count == -1,
    control.encoded_length == 0,
    if expected_field_length != 1 {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_field_preimage_size,
      )
    } else {
      canonical_scan_field_successor_is_exact(
        pre,
        witness.claimed_successor,
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.field_index,
      )
    },
  }
}

/// One chunk of one item of the field the `CanonicalDecode` phase is scanning.
///
/// **The item is reached through §8's door and the machine's own walk position,
/// not by a per-item opening.** `control.encoded_length` is the number of
/// preimage bytes before the item this step reads, which is exactly the offset
/// of that item's §5.1 wrapper (§10.2), so a continuing step costs one header
/// decode instead of a re-walk. The first step of a field has no position yet
/// and opens item 0, whose offset is the array header the view already knows.
fn verify_canonical_decode_chunk(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: CanonicalDecodeControlV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let expected_field_commitment = transaction_field_commitment(
    verified.tx_compact,
    witness_set,
    control.field_index,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    control.field_preimage_lengths_cbor,
  )
  let expected_field_length = transaction_field_preimage_length(
    field_lengths,
    control.field_index,
  )
  let first_chunk = and {
    control.item_index == 0,
    control.chunk_index == 0,
    control.item_count == -1,
    control.encoded_length == 0,
  }
  let continuing_chunk = and {
    control.item_count > 0,
    control.item_index < control.item_count,
  }
  let Pair(item, active_item_count) = if first_chunk {
    let opened = field_door.open_machine_field_item(
      door,
      verified,
      witness_set,
      control.field_index,
      0,
      carriage,
    )
    Pair(opened, field_door.machine_field_item_count(opened))
  } else {
    Pair(
      field_door.open_machine_field_item_at(
        door,
        verified,
        witness_set,
        control.field_index,
        control.item_index,
        control.encoded_length,
        carriage,
      ),
      control.item_count,
    )
  }
  let item_length = field_door.machine_field_item_length(item)
  let length_before_item = if first_chunk {
    canonical_argument_header_size(active_item_count)
  } else {
    control.encoded_length
  }
  let chunk_count = field_door.machine_field_item_chunk_count(item)
  let item_finished = control.chunk_index + 1 == chunk_count
  and {
    canonical_decode_control_is_bound(pre, witness, control, verified),
    expected_field_commitment != native_tx_field_access_v1.empty_field_commitment,
    or {
      first_chunk,
      continuing_chunk,
    },
    active_item_count > 0,
    field_index == control.field_index,
    item_index == control.item_index,
    control.chunk_index < chunk_count,
    if !item_finished {
      canonical_scan_successor_is_exact(
        pre,
        witness.claimed_successor,
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.field_index,
        control.item_index,
        control.chunk_index + 1,
        active_item_count,
        length_before_item,
      )
    } else {
      when
        transaction_field_item_encoded_length(control.field_index, item_length)
      is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(item_encoded_length) -> {
          let next_encoded_length = length_before_item + item_encoded_length
          if control.item_index + 1 < active_item_count {
            canonical_scan_successor_is_exact(
              pre,
              witness.claimed_successor,
              control.compact_cbor,
              control.witness_set_compact_cbor,
              control.field_preimage_lengths_cbor,
              control.context_cbor,
              control.field_index,
              control.item_index + 1,
              0,
              active_item_count,
              next_encoded_length,
            )
          } else if next_encoded_length != expected_field_length {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_field_preimage_size,
            )
          } else {
            canonical_scan_field_successor_is_exact(
              pre,
              witness.claimed_successor,
              control.compact_cbor,
              control.witness_set_compact_cbor,
              control.field_preimage_lengths_cbor,
              control.context_cbor,
              control.field_index,
            )
          }
        }
      }
    },
  }
}

fn verify_canonical_decode_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  _control: CanonicalDecodeControlV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  verify_canonical_decode_item_from_source_v1(
    pre,
    witness,
    bind_canonical_decode_item_source_v1(pre, witness),
    door,
    carriage,
  )
}

pub fn bind_canonical_decode_item_source_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> CanonicalDecodeItemSourceV1 {
  let control = canonical_decode_control_from_witness(witness.work_witness_cbor)
  let Pair(expected_field_commitment, expected_field_length) = canonical_item_source_field_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.field_index,
  )
  let exact_witness = encode_transaction_field_scan_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.field_index,
    control.item_index,
    control.chunk_index,
    control.item_count,
    control.encoded_length,
  )
  expect and {
      control.field_index >= 0,
      control.field_index <= 8,
      control.item_index >= 0,
      control.chunk_index >= 0,
      control.item_count >= -1,
      control.encoded_length >= 0,
      native_tx_proof_commitment_v1(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
      ) == pre.transaction_commitment,
      validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
      witness.work_witness_cbor == exact_witness,
    }
  CanonicalDecodeItemSourceV1 {
    expected_field_commitment,
    expected_field_length,
  }
}

pub fn verify_canonical_decode_item_from_source_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source: CanonicalDecodeItemSourceV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  let observation = observe_canonical_decode_item_v1(
    pre,
    witness,
    door,
    carriage,
  )
  and {
    canonical_decode_item_observation_is_consistent(
      witness,
      source,
      observation,
    ),
    verify_canonical_decode_item_successor_v1(
      pre,
      witness,
      source,
      verify_canonical_decode_item_observation_v1(witness, source, observation),
    ),
  }
}

pub fn verify_canonical_decode_item_proof_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source: CanonicalDecodeItemSourceV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> CanonicalDecodeItemProofV1 {
  verify_canonical_decode_item_observation_v1(
    witness,
    source,
    observe_canonical_decode_item_v1(pre, witness, door, carriage),
  )
}

/// Open the item through §8's door and record what the door established.
///
/// This is the one place the door runs for the complete-item path, and it is
/// therefore the one place that needs the consuming transaction. `pre` is here
/// for the transaction id the door authenticates against; under the counted
/// scheme it was not needed, because the prover's opening carried its own
/// (unsatisfiable) claim instead.
pub fn observe_canonical_decode_item_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> CanonicalDecodeItemObservationV1 {
  let control = canonical_decode_control_from_witness(witness.work_witness_cbor)
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let item = field_door.open_machine_field_item(
    door,
    verified,
    witness_set,
    control.field_index,
    control.item_index,
    carriage,
  )
  CanonicalDecodeItemObservationV1 {
    item_count: field_door.machine_field_item_count(item),
    item_length: field_door.machine_field_item_length(item),
  }
}

/// Whether the observation agrees with the control state the step claims to be
/// in.
///
/// **Named for exactly what it does.** It used to be
/// `..._is_authentic` and it used to earn that name: it folded the prover's
/// `ItemProofV1` against the §4 field commitment. Under §4 that fold is
/// unsatisfiable, and what replaces it — the §8.8 door — has already run by the
/// time an observation exists. So this checks the remaining, non-cryptographic
/// half: that the phase is at a legal item position, and that the field's
/// authenticated item count is the one the control carries. Calling it
/// "authentic" after the fold left would have been a false docstring on a
/// predicate that no longer proves anything about bytes.
fn canonical_decode_item_observation_is_consistent(
  witness: ValidationOneStepWitnessV1,
  source: CanonicalDecodeItemSourceV1,
  observation: CanonicalDecodeItemObservationV1,
) -> Bool {
  let control = canonical_decode_control_from_witness(witness.work_witness_cbor)
  let first_item = and {
    control.item_index == 0,
    control.chunk_index == 0,
    control.item_count == -1,
    control.encoded_length == 0,
  }
  let continuing_item = and {
    control.chunk_index == 0,
    control.item_count > 0,
    control.item_index < control.item_count,
    observation.item_count == control.item_count,
  }
  let active_item_count = if first_item {
    observation.item_count
  } else {
    control.item_count
  }
  and {
    source.expected_field_commitment != native_tx_field_access_v1.empty_field_commitment,
    or {
      first_item,
      continuing_item,
    },
    active_item_count > 0,
    observation.item_count == active_item_count,
  }
}

pub fn verify_canonical_decode_item_observation_v1(
  witness: ValidationOneStepWitnessV1,
  source: CanonicalDecodeItemSourceV1,
  observation: CanonicalDecodeItemObservationV1,
) -> CanonicalDecodeItemProofV1 {
  let control = canonical_decode_control_from_witness(witness.work_witness_cbor)
  let first_item = and {
    control.item_index == 0,
    control.chunk_index == 0,
    control.item_count == -1,
    control.encoded_length == 0,
  }
  let active_item_count = if first_item {
    observation.item_count
  } else {
    control.item_count
  }
  let length_before_item = if first_item {
    canonical_argument_header_size(active_item_count)
  } else {
    control.encoded_length
  }
  expect
    canonical_decode_item_observation_is_consistent(
      witness,
      source,
      observation,
    )
  when
    transaction_field_item_encoded_length(
      control.field_index,
      observation.item_length,
    )
  is {
    None ->
      CanonicalDecodeItemProofV1 {
        active_item_count,
        item_encoding_is_valid: False,
        next_encoded_length: 0,
      }
    Some(item_encoded_length) ->
      CanonicalDecodeItemProofV1 {
        active_item_count,
        item_encoding_is_valid: True,
        next_encoded_length: length_before_item + item_encoded_length,
      }
  }
}

pub fn verify_canonical_decode_item_successor_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source: CanonicalDecodeItemSourceV1,
  proof: CanonicalDecodeItemProofV1,
) -> Bool {
  let control = canonical_decode_control_from_witness(witness.work_witness_cbor)
  if !proof.item_encoding_is_valid {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_invalid_field_type,
    )
  } else if control.item_index + 1 < proof.active_item_count {
    canonical_scan_successor_is_exact(
      pre,
      witness.claimed_successor,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.field_index,
      control.item_index + 1,
      0,
      proof.active_item_count,
      proof.next_encoded_length,
    )
  } else if proof.next_encoded_length != source.expected_field_length {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_field_preimage_size,
    )
  } else {
    canonical_scan_field_successor_is_exact(
      pre,
      witness.claimed_successor,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.field_index,
    )
  }
}

fn canonical_item_source_field_v1(
  transaction_id: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  field_index: Int,
) -> Pair<ByteArray, Int> {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  Pair(
    transaction_field_commitment(verified.tx_compact, witness_set, field_index),
    transaction_field_preimage_length(field_lengths, field_index),
  )
}

pub fn verify_canonical_decode_empty_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_canonical_decode_empty(
    pre,
    witness,
    canonical_decode_control_from_witness(witness.work_witness_cbor),
  )
}

pub fn verify_canonical_decode_chunk_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  verify_canonical_decode_chunk(
    pre,
    witness,
    canonical_decode_control_from_witness(witness.work_witness_cbor),
    door,
    field_index,
    item_index,
    carriage,
  )
}

pub fn verify_canonical_decode_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  verify_canonical_decode_item(
    pre,
    witness,
    canonical_decode_control_from_witness(witness.work_witness_cbor),
    door,
    carriage,
  )
}

pub fn verify_canonical_decode(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let control = canonical_decode_control_from_witness(witness.work_witness_cbor)
  when auxiliary is {
    NoAuxiliaryWitness -> verify_canonical_decode_empty(pre, witness, control)
    TransactionFieldChunkWitness { field_index, item_index, carriage } ->
      verify_canonical_decode_chunk(
        pre,
        witness,
        control,
        door,
        field_index,
        item_index,
        carriage,
      )
    TransactionFieldItemWitness { carriage } ->
      verify_canonical_decode_item(pre, witness, control, door, carriage)
    _ -> False
  }
}

pub fn verify_compact_binding(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        transaction_id_data, commitment_data, compact_cbor_data,
        witness_set_compact_cbor_data, field_preimage_lengths_cbor_data,
        context_cbor_data,
      ]
    = builtin.un_list_data(data)
  let transaction_id = builtin.un_b_data(transaction_id_data)
  let commitment = builtin.un_b_data(commitment_data)
  let compact_cbor = builtin.un_b_data(compact_cbor_data)
  let witness_set_compact_cbor = builtin.un_b_data(
    witness_set_compact_cbor_data,
  )
  let field_preimage_lengths_cbor = builtin.un_b_data(
    field_preimage_lengths_cbor_data,
  )
  let context_cbor = builtin.un_b_data(context_cbor_data)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    transaction_id == pre.transaction_id,
    commitment == pre.transaction_commitment,
    commitment == native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ),
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_compact_binding_witness(
      transaction_id,
      commitment,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
    ),
    witness.claimed_successor.phase == validation_trace_v1.StaticLedgerRules,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.StaticLedgerRules,
      pre.program_counter + 1,
      encode_static_rules_witness(
        compact_cbor,
        witness_set_compact_cbor,
        field_preimage_lengths_cbor,
        context_cbor,
      ),
    ),
  }
}
