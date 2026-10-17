use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{verify_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{
  NativeTxBodyCompact, NativeTxWitnessSetCompact,
}
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1.{
  DuplicateInput, EmptyInputs, ValidityIntervalMalformed,
}
use midgard/validation_machine/machine_types.{
  InputSetsControlV1, NoAuxiliaryWitness, TransactionFieldItemWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  max_tx_size_derived_collection_item_count, rejected_successor_is_exact,
  unset_reason_coordinate, validity_interval_is_malformed,
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
  let Pair(verified, _) = verify_tx_proof_source_v1(
    pre.source_kind,
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
    rejected_successor_is_exact(pre, witness.claimed_successor, EmptyInputs),
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
  last_key: ByteArray,
  last_field: Int,
  last_item: Int,
  cached_other_head: ByteArray,
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
      rejected_successor_is_exact(pre, post, ValidityIntervalMalformed)
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
            -1,
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
          last_key,
          last_field,
          last_item,
          cached_other_head,
          resolution_schedule_hash,
        ),
      ),
    }
  }
}

/// One step of the descending merge of fields 0 and 1.
///
/// CanonicalDecode admits each field only in ascending item order, so the
/// merge reads both from the tail: the next item of a field is
/// `count - seen - 1`, which the door derives. The control fixes the field
/// this step opens, so the witness carries only the carriage:
///
///   * before the first item, field 0, then field 1 when the scan cached
///     field 0's head (the seed step below);
///   * after that, the field consumed last while it has items left, else the
///     other one.
///
/// `cached_other_head` is the other field's next item when it has been read
/// and not consumed; it is `#""` when that field has nothing left, or when the
/// field consumed last ran out (the other head is then read again). The step
/// takes the larger of the opened item and the cached one (`#""` is below
/// every item). Two equal heads, or a head equal to `last_key`, are a
/// duplicate; otherwise the larger head is consumed and prepended to the
/// resolution schedule, so the schedule ascends.
fn verify_input_sets_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: InputSetsControlV1,
  door: field_door.MachineFieldDoorV1,
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_tx_proof_source_v1(
    pre.source_kind,
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let spend_count = control.spend_count
  let reference_count = control.reference_count
  let spend_seen = control.spend_seen
  let reference_seen = control.reference_seen
  let last_key = control.last_key
  let cached_other_head = control.cached_other_head
  let field = if last_key == #"" {
    if cached_other_head == #"" {
      0
    } else {
      1
    }
  } else if control.last_field == 0 {
    if spend_seen < spend_count {
      0
    } else {
      1
    }
  } else if reference_seen < reference_count {
    1
  } else {
    0
  }
  let item = field_door.open_machine_fixed_field_tail_item(
    door,
    verified,
    witness_set,
    field,
    if field == 0 {
      spend_seen
    } else {
      reference_seen
    },
    carriage,
  )
  let item_count = field_door.machine_fixed_field_item_count(item)
  let key = field_door.machine_fixed_field_item_bytes(item)
  let prior_count = if field == 0 {
    spend_count
  } else {
    reference_count
  }
  let next_spend_count = if field == 0 {
    item_count
  } else {
    spend_count
  }
  let next_reference_count = if field == 0 {
    reference_count
  } else {
    item_count
  }
  let successor = fn(
    next_spend_seen: Int,
    next_reference_seen: Int,
    next_last_key: ByteArray,
    next_last_field: Int,
    next_last_item: Int,
    next_cached_other_head: ByteArray,
    next_schedule_hash: ByteArray,
  ) {
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
      next_last_key,
      next_last_field,
      next_last_item,
      next_cached_other_head,
      next_schedule_hash,
    )
  }
  let order = bytearray.compare(key, cached_other_head)
  let consumed = if order == Greater {
    key
  } else {
    cached_other_head
  }
  and {
    input_sets_control_is_bound(pre, witness, control, verified),
    spend_count != 0,
    prior_count == -1 || prior_count == item_count,
    item_count <= max_tx_size_derived_collection_item_count,
    encode_midgard_tx_input(decode_midgard_tx_input_cbor(key)) == key,
    if last_key == #"" && cached_other_head == #"" && reference_count != 0 {
      // Seed: hold field 0's head and open field 1's next.
      successor(0, 0, #"", -1, -1, key, control.resolution_schedule_hash)
    } else if order == Equal || consumed == last_key {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        DuplicateInput {
          first_field_index: unset_reason_coordinate,
          first_item_index: unset_reason_coordinate,
          second_field_index: unset_reason_coordinate,
          second_item_index: unset_reason_coordinate,
        },
      )
    } else {
      let consumed_field = if order == Greater {
        field
      } else {
        1 - field
      }
      let next_spend_seen = if consumed_field == 0 {
        spend_seen + 1
      } else {
        spend_seen
      }
      let next_reference_seen = if consumed_field == 0 {
        reference_seen
      } else {
        reference_seen + 1
      }
      let Pair(consumed_count, consumed_seen) = if consumed_field == 0 {
        Pair(next_spend_count, next_spend_seen)
      } else {
        Pair(next_reference_count, next_reference_seen)
      }
      successor(
        next_spend_seen,
        next_reference_seen,
        consumed,
        consumed_field,
        consumed_count - consumed_seen,
        if consumed_seen == consumed_count {
          #""
        } else if order == Greater {
          cached_other_head
        } else {
          key
        },
        resolution_schedule_node_hash(
          consumed_field,
          consumed,
          control.resolution_schedule_hash,
        ),
      )
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
  carriage: native_tx_field_access_v1.FieldCarriageV1,
) -> Bool {
  verify_input_sets_item(
    pre,
    witness,
    input_sets_control_from_witness(witness.work_witness_cbor),
    door,
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
    TransactionFieldItemWitness { carriage } ->
      verify_input_sets_item(pre, witness, control, door, carriage)
    _ -> False
  }
}
