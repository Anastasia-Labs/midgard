use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/types.{VerifiedMidgardNativeTxCompact}
use midgard/native_tx_field_access_v1
use midgard/rejection_reason_v1.{reject_invalid_field_type}
use midgard/validation_machine/machine_types.{
  NoAuxiliaryWitness, PhaseAScriptPreconditionsControlV1,
  TransactionFieldChunkWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  max_tx_size_derived_collection_item_count, rejected_successor_is_exact,
}
use midgard/validation_machine/witness.{
  encode_phase_a_script_preconditions_witness, encode_resolve_inputs_witness,
  initial_resolution_accumulator,
  phase_a_script_preconditions_control_from_witness,
  phase_a_script_preconditions_control_is_bound,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_trace_v1

fn phase_a_script_preconditions_rejection(
  observer_count: Int,
  has_redeemers: Bool,
  contains_non_native_script: Bool,
  script_integrity_hash: ByteArray,
  network_id: Int,
) -> Option<ByteArray> {
  let requires_plutus_evaluation = or {
    contains_non_native_script,
    has_redeemers,
    script_integrity_hash != #"0000000000000000000000000000000000000000000000000000000000000000",
  }
  if !requires_plutus_evaluation {
    None
  } else if script_integrity_hash == #"0000000000000000000000000000000000000000000000000000000000000000" {
    Some(reject_invalid_field_type)
  } else if and {
    observer_count > 0,
    network_id == 255,
  } {
    Some(reject_invalid_field_type)
  } else {
    None
  }
}

fn phase_a_script_preconditions_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: PhaseAScriptPreconditionsControlV1,
) -> Bool {
  and {
    post.phase == validation_trace_v1.PhaseAScriptPreconditions,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      pre.program_counter + 1,
      encode_phase_a_script_preconditions_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        control.signer_count,
        control.signer_frontier_commitment,
        control.contains_non_native_script == 1,
        control.observer_count,
        control.observer_seen,
        control.previous_observer,
      ),
    ),
  }
}

fn phase_a_script_preconditions_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseAScriptPreconditionsControlV1,
  verified_source: VerifiedMidgardNativeTxCompact,
  has_redeemers: Bool,
  observer_count: Int,
) -> Bool {
  when
    phase_a_script_preconditions_rejection(
      observer_count,
      has_redeemers,
      control.contains_non_native_script == 1,
      verified_source.tx_compact.body.script_integrity_hash,
      verified_source.tx_compact.body.network_id,
    )
  is {
    Some(rejection_code) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        rejection_code,
      )
    None -> and {
        witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ResolveInputs,
          pre.program_counter + 1,
          encode_resolve_inputs_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            0,
            initial_resolution_accumulator(),
            control.resolution_schedule_hash,
            control.signer_count,
            control.signer_frontier_commitment,
            None,
            control.resolution_schedule_hash,
          ),
        ),
      }
  }
}

pub fn verify_phase_a_script_preconditions(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let control = phase_a_script_preconditions_control_from_witness(
    witness.work_witness_cbor,
  )
  let Pair(verified_source, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let observer_commitment = verified_source.tx_compact.body.required_observers_hash
  let empty_observer_commitment = native_tx_field_access_v1.empty_field_commitment
  let has_redeemers = witness_set.redeemer_tx_wits_hash != native_tx_field_access_v1.empty_field_commitment
  and {
    verified_source.version == 1,
    phase_a_script_preconditions_control_is_bound(
      pre,
      witness,
      control,
      observer_commitment,
    ),
    if observer_commitment == empty_observer_commitment {
      and {
        auxiliary == NoAuxiliaryWitness,
        phase_a_script_preconditions_finalize(
          pre,
          witness,
          control,
          verified_source,
          has_redeemers,
          0,
        ),
      }
    } else if control.observer_count > 0 && control.observer_seen == control.observer_count {
      and {
        auxiliary == NoAuxiliaryWitness,
        phase_a_script_preconditions_finalize(
          pre,
          witness,
          control,
          verified_source,
          has_redeemers,
          control.observer_count,
        ),
      }
    } else {
      expect TransactionFieldChunkWitness { field_index, item_index, carriage }
        = auxiliary
      let item = field_door.open_machine_field_item(
        door,
        verified_source,
        witness_set,
        3,
        control.observer_seen,
        carriage,
      )
      let item_count = field_door.machine_field_item_count(item)
      let active_count = if control.observer_count == 0 {
        item_count
      } else {
        control.observer_count
      }
      let observer_hash = field_door.machine_field_item_bytes(item)
      and {
        active_count > 0,
        active_count <= max_tx_size_derived_collection_item_count,
        field_index == 3,
        item_index == control.observer_seen,
        item_count == active_count,
        field_door.machine_field_item_length(item) == 28,
        if and {
          control.observer_seen > 0,
          bytearray.compare(control.previous_observer, observer_hash) != Less,
        } {
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        } else {
          phase_a_script_preconditions_successor_is_exact(
            pre,
            witness.claimed_successor,
            PhaseAScriptPreconditionsControlV1 {
              ..control,
              observer_count: active_count,
              observer_seen: control.observer_seen + 1,
              previous_observer: observer_hash,
            },
          )
        },
      }
    },
  }
}
