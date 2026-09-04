use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_witness_set_compact, native_tx_proof_commitment_v1,
  verify_native_tx_compact_cbor_v1,
}
use midgard/rejection_reason_v1.{reject_invalid_field_type}
use midgard/script_language_views_v1
use midgard/validation_machine/machine_types.{
  NoAuxiliaryWitness, ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/native_scripts.{
  script_integrity_control_and_stage,
}
use midgard/validation_machine/shared.{rejected_successor_is_exact}
use midgard/validation_machine/witness.{
  encode_cek_witness_v1, encode_script_integrity_compact_witness_v1,
  encode_script_integrity_finalize_witness_v1,
  encode_script_integrity_witness_set_witness_v1,
  encode_script_integrity_witness_v1, native_resolved_context_is_well_formed,
  native_scripts_control_from_witness,
}
use midgard/validation_trace_v1

pub fn verify_script_integrity_authentication(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let Pair(control, stage) = script_integrity_control_and_stage(
    witness.work_witness_cbor,
  )
  and {
    auxiliary == NoAuxiliaryWitness,
    stage == 0,
    control.execution_cursor == control.execution_count,
    native_resolved_context_is_well_formed(control),
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_script_integrity_witness_v1(control),
    witness.claimed_successor.phase == validation_trace_v1.ScriptIntegrity,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      pre.program_counter + 1,
      encode_script_integrity_compact_witness_v1(control),
    ),
  }
}

pub fn verify_script_integrity_compact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let Pair(control, stage) = script_integrity_control_and_stage(
    witness.work_witness_cbor,
  )
  let verified_source = verify_native_tx_compact_cbor_v1(
    pre.transaction_id,
    control.compact_cbor,
  )
  and {
    auxiliary == NoAuxiliaryWitness,
    stage == 1,
    witness.work_witness_cbor == encode_script_integrity_compact_witness_v1(
      control,
    ),
    witness.claimed_successor.phase == validation_trace_v1.ScriptIntegrity,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      pre.program_counter + 1,
      encode_script_integrity_witness_set_witness_v1(
        control,
        verified_source.tx_compact.body.script_integrity_hash,
        verified_source.tx_compact.witness_set_hash,
      ),
    ),
  }
}

pub fn verify_script_integrity_witness_set(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        native_control_cbor_data, stage_data, script_integrity_hash_data,
        witness_set_hash_data,
      ]
    = builtin.un_list_data(data)
  let control = native_scripts_control_from_witness(
    builtin.un_b_data(native_control_cbor_data),
  )
  let stage = builtin.un_i_data(stage_data)
  let script_integrity_hash = builtin.un_b_data(script_integrity_hash_data)
  let witness_set_hash = builtin.un_b_data(witness_set_hash_data)
  let verified_witness_set = decode_native_tx_witness_set_compact(
    control.witness_set_compact_cbor,
  )
  and {
    auxiliary == NoAuxiliaryWitness,
    stage == 2,
    bytearray.length(script_integrity_hash) == 32,
    bytearray.length(witness_set_hash) == 32,
    blake2b_256(control.witness_set_compact_cbor) == witness_set_hash,
    witness.work_witness_cbor == encode_script_integrity_witness_set_witness_v1(
      control,
      script_integrity_hash,
      witness_set_hash,
    ),
    witness.claimed_successor.phase == validation_trace_v1.ScriptIntegrity,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      pre.program_counter + 1,
      encode_script_integrity_finalize_witness_v1(
        control,
        script_integrity_hash,
        verified_witness_set.redeemer_tx_wits_hash,
      ),
    ),
  }
}

pub fn verify_script_integrity_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        native_control_cbor_data, stage_data, script_integrity_hash_data,
        redeemer_tx_wits_hash_data,
      ]
    = builtin.un_list_data(data)
  let control = native_scripts_control_from_witness(
    builtin.un_b_data(native_control_cbor_data),
  )
  let stage = builtin.un_i_data(stage_data)
  let script_integrity_hash = builtin.un_b_data(script_integrity_hash_data)
  let redeemer_tx_wits_hash = builtin.un_b_data(redeemer_tx_wits_hash_data)
  let expected_hash = script_language_views_v1.expected_script_integrity_hash(
    redeemer_tx_wits_hash,
    control.language_bitmap,
  )
  and {
    auxiliary == NoAuxiliaryWitness,
    stage == 3,
    bytearray.length(script_integrity_hash) == 32,
    bytearray.length(redeemer_tx_wits_hash) == 32,
    witness.work_witness_cbor == encode_script_integrity_finalize_witness_v1(
      control,
      script_integrity_hash,
      redeemer_tx_wits_hash,
    ),
    if script_integrity_hash != expected_hash {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    } else {
      and {
        witness.claimed_successor.phase == validation_trace_v1.Cek,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.Cek,
          pre.program_counter + 1,
          encode_cek_witness_v1(control, #"", 0, 0, 0, #"", 0, 0, #""),
        ),
      }
    },
  }
}

pub fn verify_script_integrity(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let Pair(_, stage) = script_integrity_control_and_stage(
    witness.work_witness_cbor,
  )
  if stage == 0 {
    verify_script_integrity_authentication(pre, witness, auxiliary)
  } else if stage == 1 {
    verify_script_integrity_compact(pre, witness, auxiliary)
  } else if stage == 2 {
    verify_script_integrity_witness_set(pre, witness, auxiliary)
  } else if stage == 3 {
    verify_script_integrity_finalize(pre, witness, auxiliary)
  } else {
    False
  }
}
