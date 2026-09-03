use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1.{
  reject_duplicate_input, reject_empty_inputs,
  reject_invalid_validity_interval_format,
}
use midgard/validation_machine/machine_types.{
  InputSetsControlV1, NoAuxiliaryWitness, TransactionFieldChunkWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  max_tx_size_derived_collection_item_count, rejected_successor_is_exact,
  transaction_field_commitment, validity_interval_is_malformed,
}
use midgard/validation_machine/witness.{
  encode_input_sets_scan_witness, encode_signatures_scan_witness,
  input_sets_control_from_witness, input_sets_control_is_bound,
  resolution_schedule_node_hash,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_trace_v1

fn verify_input_sets_empty(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: InputSetsControlV1,
) -> Bool {
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    input_sets_control_is_bound(pre, witness, control, verified),
    control.spend_count == 0,
    control.spend_seen == 0,
    control.reference_seen == 0,
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_empty_inputs,
    ),
  }
}

fn input_sets_item_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: InputSetsControlV1,
  body: NativeTxBodyCompact,
  witness_set: NativeTxWitnessSetCompact,
  spend_count: Int,
  reference_count: Int,
  spend_seen: Int,
  reference_seen: Int,
  key: ByteArray,
  resolution_schedule_hash: ByteArray,
) -> Bool {
  let complete = and {
    spend_count > 0,
    reference_count >= 0,
    spend_seen == spend_count,
    reference_seen == reference_count,
  }
  if complete {
    let malformed_validity_interval = validity_interval_is_malformed(
      body.validity_interval_start,
      body.validity_interval_end,
    )
    if malformed_validity_interval {
      rejected_successor_is_exact(
        pre,
        post,
        reject_invalid_validity_interval_format,
      )
    } else {
      let address_count = if witness_set.addr_tx_wits_hash == native_tx_field_access_v1.empty_field_commitment {
        0
      } else {
        -1
      }
      let required_count = if body.required_signers_hash == native_tx_field_access_v1.empty_field_commitment {
        0
      } else {
        -1
      }
      and {
        post.phase == validation_trace_v1.Signatures,
        post.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.Signatures,
          pre.program_counter + 1,
          encode_signatures_scan_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            resolution_schedule_hash,
            0,
            address_count,
            required_count,
            0,
            0,
            #"",
            #"",
            0,
            [],
            0,
          ),
        ),
      }
    }
  } else {
    and {
      post.phase == validation_trace_v1.InputSets,
      post.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.InputSets,
        pre.program_counter + 1,
        encode_input_sets_scan_witness(
          control.compact_cbor,
          control.witness_set_compact_cbor,
          control.field_preimage_lengths_cbor,
          control.context_cbor,
          spend_count,
          reference_count,
          spend_seen,
          reference_seen,
          key,
          resolution_schedule_hash,
        ),
      ),
    }
  }
}

/// One §5.3 out-ref item of field 0 or field 1.
///
/// The two fields are fixed-stride, so the door reaches the item by arithmetic
/// and `field_index` is the only thing that says which of the two slots the step
/// means — §4 gives the two commitments no domain separation, so this is checked
/// against `{0, 1}` here and pinned to the successor's `source_kind` below.
/// `item_index` is the prover's to choose: this phase scans in **descending
/// out-ref key order**, not index order, which is what `previous_key` enforces.
fn verify_input_sets_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: InputSetsControlV1,
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
  let source_kind = field_index
  let expected_field_commitment = transaction_field_commitment(
    verified.tx_compact,
    witness_set,
    field_index,
  )
  let prior_count = if field_index == 0 {
    control.spend_count
  } else {
    control.reference_count
  }
  let item = field_door.open_machine_field_item(
    door,
    verified,
    witness_set,
    field_index,
    item_index,
    carriage,
  )
  let item_count = field_door.machine_field_item_count(item)
  let active_count = if prior_count == -1 {
    item_count
  } else {
    prior_count
  }
  let prior_seen = if field_index == 0 {
    control.spend_seen
  } else {
    control.reference_seen
  }
  let key = field_door.machine_field_item_bytes(item)
  let input = decode_midgard_tx_input_cbor(key)
  let next_schedule_hash = resolution_schedule_node_hash(
    source_kind,
    key,
    control.resolution_schedule_hash,
  )
  let next_spend_count = if field_index == 0 {
    active_count
  } else {
    control.spend_count
  }
  let next_reference_count = if field_index == 1 {
    active_count
  } else {
    control.reference_count
  }
  let next_spend_seen = if field_index == 0 {
    control.spend_seen + 1
  } else {
    control.spend_seen
  }
  let next_reference_seen = if field_index == 1 {
    control.reference_seen + 1
  } else {
    control.reference_seen
  }
  let duplicate = and {
    bytearray.length(control.previous_key) > 0,
    bytearray.compare(key, control.previous_key) == Equal,
  }
  let correctly_ordered = or {
    control.previous_key == #"",
    bytearray.compare(key, control.previous_key) == Less,
  }
  and {
    input_sets_control_is_bound(pre, witness, control, verified),
    control.spend_count != 0,
    or {
      field_index == 0,
      field_index == 1,
    },
    expected_field_commitment != native_tx_field_access_v1.empty_field_commitment,
    prior_count == -1 || prior_count == item_count,
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    prior_seen < active_count,
    item_count == active_count,
    item_index >= 0,
    item_index < active_count,
    encode_midgard_tx_input(input) == key,
    if duplicate {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_duplicate_input,
      )
    } else {
      and {
        correctly_ordered,
        input_sets_item_successor_is_exact(
          pre,
          witness.claimed_successor,
          control,
          verified.tx_compact.body,
          witness_set,
          next_spend_count,
          next_reference_count,
          next_spend_seen,
          next_reference_seen,
          key,
          next_schedule_hash,
        ),
      }
    },
  }
}

pub fn verify_input_sets_empty_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_input_sets_empty(
    pre,
    witness,
    input_sets_control_from_witness(witness.work_witness_cbor),
  )
}

pub fn verify_input_sets_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  door: field_door.MachineFieldDoorV1,
  field_index: Int,
  item_index: Int,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  verify_input_sets_item(
    pre,
    witness,
    input_sets_control_from_witness(witness.work_witness_cbor),
    door,
    field_index,
    item_index,
    carriage,
  )
}

pub fn verify_input_sets(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let control = input_sets_control_from_witness(witness.work_witness_cbor)
  when auxiliary is {
    NoAuxiliaryWitness -> verify_input_sets_empty(pre, witness, control)
    TransactionFieldChunkWitness { field_index, item_index, carriage } ->
      verify_input_sets_item(
        pre,
        witness,
        control,
        door,
        field_index,
        item_index,
        carriage,
      )
    _ -> False
  }
}
