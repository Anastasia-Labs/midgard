use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/ledger_output_proof_v1
use midgard/redeemer_item_proof_v1
use midgard/validation_machine/machine_types.{
  RedeemerItemStepWitness, RedeemerScanBeginWitness, ScriptSourcesControlV1,
  SignerSetProofV1, TransactionRedeemerItemBeginWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepEvidenceV1,
  ValidationOneStepWitnessV1,
}
use midgard/validation_machine/script_sources_early_stages.{
  script_sources_stage_one, script_sources_stage_two, script_sources_stage_zero,
}
use midgard/validation_machine/script_sources_late_stages.{
  redeemer_descriptor_matches_current_purpose, script_sources_stage_eight,
  script_sources_stage_eleven, script_sources_stage_nine,
  script_sources_stage_seven,
  script_sources_stage_seven_with_observer_commitment, script_sources_stage_ten,
  script_sources_stage_twelve,
}
use midgard/validation_machine/script_sources_middle_stages.{
  script_sources_output_proof_begin, script_sources_output_proof_finalize,
  script_sources_output_proof_step, script_sources_replay_item,
  script_sources_stage_five, script_sources_stage_four, script_sources_stage_six,
  script_sources_stage_three,
}
use midgard/validation_machine/witness.{
  empty_resolution_schedule_hash, encode_script_sources_witness,
  script_sources_control_from_witness, script_sources_control_is_bound,
  script_sources_stage_ten_control_from_witness,
}
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_trace_v1

pub fn verify_script_sources_stage_three_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let next_control = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    4,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    control.replay_cursor,
    control.replay_accumulator,
    control.replay_remaining_schedule_hash,
    control.spend_index,
    control.purpose_count,
    control.purpose_peaks,
    control.output_cursor,
    control.output_count,
    control.output_peaks,
    control.output_total_count,
    control.receive_scan,
    control.source_total_count,
    control.redeemer_total_count,
    control.observer_scan,
    control.mint_fold,
    control.resolution_schedule_hash,
  )
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 3,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.replay_cursor == control.resolved_input_count,
    control.replay_accumulator == control.resolved_inputs_accumulator,
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_control,
    ),
  }
}

pub fn verify_script_sources_stage_three_replay_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 3,
    control.replay_remaining_schedule_hash != empty_resolution_schedule_hash(),
    script_sources_replay_item(
      pre,
      witness,
      control,
      source_kind,
      key,
      next_schedule_hash,
      value,
    ),
  }
}

/// Stage-specific transition check used only after a prepare validator has
/// authenticated the complete ScriptSources control. Keeping preparation and
/// transition execution in separate computation-thread steps prevents the
/// same common-control work from making the semantic step undeployable.
pub fn verify_prepared_script_sources_stage_three_replay_transition_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    control.stage == 3,
    control.replay_remaining_schedule_hash != empty_resolution_schedule_hash(),
    script_sources_replay_item(
      pre,
      witness,
      control,
      source_kind,
      key,
      next_schedule_hash,
      value,
    ),
  }
}

pub fn verify_script_sources_stage_five_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let next_control = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    6,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    control.replay_cursor,
    control.replay_accumulator,
    control.replay_remaining_schedule_hash,
    control.spend_index,
    control.purpose_count,
    control.purpose_peaks,
    control.output_cursor,
    control.output_count,
    control.output_peaks,
    control.output_total_count,
    control.receive_scan,
    control.source_total_count,
    control.redeemer_total_count,
    control.observer_scan,
    control.mint_fold,
    control.resolution_schedule_hash,
  )
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor == control.output_count,
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_control,
    ),
  }
}

pub fn verify_script_sources_output_proof_begin_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor < control.output_count,
    script_sources_output_proof_begin(
      pre,
      witness,
      control,
      output_index,
      total_length,
      item_commitment,
      siblings,
    ),
  }
}

pub fn verify_script_sources_output_proof_step_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  proof_witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  expect Some(output_proof) = control.output_proof
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor < control.output_count,
    !ledger_output_proof_v1.terminal_is_exact_v1(output_proof),
    script_sources_output_proof_step(
      pre,
      witness,
      control,
      output_proof,
      proof_witness,
    ),
  }
}

pub fn verify_script_sources_output_proof_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  expect Some(output_proof) = control.output_proof
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor < control.output_count,
    ledger_output_proof_v1.terminal_is_exact_v1(output_proof),
    script_sources_output_proof_finalize(
      pre,
      witness,
      control,
      output_proof,
      descriptor_cbor,
      signer_proof,
    ),
  }
}

pub fn verify_script_sources_non_output_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let witness = evidence.transition
  let auxiliary = evidence.auxiliary
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    script_sources_control_is_bound(pre, witness, control),
    control.stage != 5,
    if control.stage == 0 {
      script_sources_stage_zero(pre, witness, auxiliary, control, door)
    } else if control.stage == 1 {
      script_sources_stage_one(pre, witness, auxiliary, control, door)
    } else if control.stage == 2 {
      script_sources_stage_two(pre, witness, auxiliary, control)
    } else if control.stage == 3 {
      script_sources_stage_three(pre, witness, auxiliary, control)
    } else if control.stage == 4 {
      script_sources_stage_four(pre, witness, auxiliary, control, door)
    } else if control.stage == 6 {
      script_sources_stage_six(pre, witness, auxiliary, control, door)
    } else if control.stage == 7 {
      script_sources_stage_seven(pre, witness, auxiliary, control, door)
    } else if control.stage == 8 {
      script_sources_stage_eight(pre, witness, auxiliary, control)
    } else if control.stage == 9 {
      script_sources_stage_nine(pre, witness, auxiliary, control)
    } else if control.stage == 10 {
      script_sources_stage_ten(pre, witness, auxiliary, control)
    } else if control.stage == 11 {
      script_sources_stage_eleven(pre, witness, auxiliary, control)
    } else if control.stage == 12 {
      script_sources_stage_twelve(pre, witness, auxiliary, control)
    } else {
      False
    },
  }
}

fn verify_script_sources_semantic_stage(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  expected_stage: Int,
  verify_stage: fn(
    validation_trace_v1.ValidationMachineStateV1,
    ValidationOneStepWitnessV1,
    ValidationAuxiliaryWitnessV1,
    ScriptSourcesControlV1,
  ) ->
    Bool,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == expected_stage,
    verify_stage(pre, witness, auxiliary, control),
  }
}

/// The same dispatch for the five stages that reach a committed field through
/// §8's door and therefore take one — stages 0, 1, 4, 6 and 7. Kept separate
/// rather than widening the arity of every stage function, so a stage that reads
/// no field cannot be handed a door to ignore.
fn verify_script_sources_semantic_stage_with_door(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
  expected_stage: Int,
  verify_stage: fn(
    validation_trace_v1.ValidationMachineStateV1,
    ValidationOneStepWitnessV1,
    ValidationAuxiliaryWitnessV1,
    ScriptSourcesControlV1,
    field_door.MachineFieldDoorV1,
  ) ->
    Bool,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == expected_stage,
    verify_stage(pre, witness, auxiliary, control, door),
  }
}

pub fn verify_script_sources_stage_zero_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_script_sources_semantic_stage_with_door(
    pre,
    witness,
    auxiliary,
    door,
    0,
    script_sources_stage_zero,
  )
}

pub fn verify_script_sources_stage_one_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_script_sources_semantic_stage_with_door(
    pre,
    witness,
    auxiliary,
    door,
    1,
    script_sources_stage_one,
  )
}

pub fn verify_script_sources_stage_two_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    2,
    script_sources_stage_two,
  )
}

pub fn verify_script_sources_stage_three_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    3,
    script_sources_stage_three,
  )
}

pub fn verify_script_sources_stage_four_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_script_sources_semantic_stage_with_door(
    pre,
    witness,
    auxiliary,
    door,
    4,
    script_sources_stage_four,
  )
}

pub fn verify_script_sources_stage_five_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    5,
    script_sources_stage_five,
  )
}

pub fn verify_script_sources_stage_six_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_script_sources_semantic_stage_with_door(
    pre,
    witness,
    auxiliary,
    door,
    6,
    script_sources_stage_six,
  )
}

pub fn verify_script_sources_stage_seven_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  verify_script_sources_semantic_stage_with_door(
    pre,
    witness,
    auxiliary,
    door,
    7,
    script_sources_stage_seven,
  )
}

pub fn verify_script_sources_stage_eight_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    8,
    script_sources_stage_eight,
  )
}

pub fn verify_script_sources_stage_nine_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    9,
    script_sources_stage_nine,
  )
}

pub fn verify_script_sources_stage_ten_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    10,
    script_sources_stage_ten,
  )
}

pub fn verify_script_sources_stage_eleven_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    11,
    script_sources_stage_eleven,
  )
}

pub fn verify_script_sources_stage_twelve_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    12,
    script_sources_stage_twelve,
  )
}

fn script_sources_stage_one_redeemer_auxiliary_is_family(
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  when auxiliary is {
    TransactionRedeemerItemBeginWitness { .. } -> True
    RedeemerItemStepWitness { redeemer_control, .. } ->
      when redeemer_control is {
        None -> True
        Some(_) -> False
      }
    _ -> False
  }
}

pub fn verify_script_sources_stage_one_redeemer_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  if script_sources_stage_one_redeemer_auxiliary_is_family(auxiliary) {
    verify_script_sources_stage_one_semantics_v1(pre, witness, auxiliary, door)
  } else {
    False
  }
}

fn script_sources_stage_ten_item_matches_current_purpose(
  control: ScriptSourcesControlV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  when auxiliary is {
    RedeemerItemStepWitness {
      redeemer_control,
      control: item_control,
      witness: item_witness,
    } ->
      when redeemer_control is {
        Some(_) -> False
        None ->
          when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
            Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
              control: next,
            }) ->
              if next.stage == redeemer_item_proof_v1.stage_terminal {
                when redeemer_item_proof_v1.descriptor_v1(next) is {
                  Some(descriptor) ->
                    redeemer_descriptor_matches_current_purpose(
                      descriptor,
                      control.discovery,
                    )
                  None -> False
                }
              } else {
                False
              }
            _ -> False
          }
      }
    _ -> False
  }
}

fn script_sources_stage_ten_mismatch_auxiliary_is_family(
  control: ScriptSourcesControlV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  if control.discovery.redeemer_cursor == control.redeemer_count {
    False
  } else if control.discovery.redeemer_item_control_hash == #"" {
    when auxiliary is {
      RedeemerScanBeginWitness { .. } -> True
      _ -> False
    }
  } else {
    when auxiliary is {
      RedeemerItemStepWitness { redeemer_control, .. } ->
        when redeemer_control is {
          None ->
            !script_sources_stage_ten_item_matches_current_purpose(
              control,
              auxiliary,
            )
          Some(_) -> False
        }
      _ -> False
    }
  }
}

pub fn verify_script_sources_stage_ten_match_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = script_sources_stage_ten_control_from_witness(
    witness.work_witness_cbor,
  )
  if and {
    control.discovery.redeemer_cursor < control.redeemer_count,
    control.discovery.redeemer_item_control_hash != #"",
    script_sources_stage_ten_item_matches_current_purpose(control, auxiliary),
  } {
    verify_script_sources_stage_ten_semantics_v1(pre, witness, auxiliary)
  } else {
    False
  }
}

pub fn verify_script_sources_stage_ten_mismatch_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = script_sources_stage_ten_control_from_witness(
    witness.work_witness_cbor,
  )
  if script_sources_stage_ten_mismatch_auxiliary_is_family(control, auxiliary) {
    verify_script_sources_stage_ten_semantics_v1(pre, witness, auxiliary)
  } else {
    False
  }
}

fn script_sources_stage_twelve_redeemer_auxiliary_is_family(
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  when auxiliary is {
    RedeemerScanBeginWitness { .. } -> True
    RedeemerItemStepWitness { redeemer_control, .. } ->
      when redeemer_control is {
        None -> True
        Some(_) -> False
      }
    _ -> False
  }
}

pub fn verify_script_sources_stage_twelve_redeemer_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  if script_sources_stage_twelve_redeemer_auxiliary_is_family(auxiliary) {
    verify_script_sources_stage_twelve_semantics_v1(pre, witness, auxiliary)
  } else {
    False
  }
}

pub fn verify_script_sources(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let Pair(verified_source, dispatch_witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    script_sources_control_is_bound(pre, witness, control),
    if control.stage == 0 {
      script_sources_stage_zero(pre, witness, auxiliary, control, door)
    } else if control.stage == 1 {
      script_sources_stage_one(pre, witness, auxiliary, control, door)
    } else if control.stage == 2 {
      script_sources_stage_two(pre, witness, auxiliary, control)
    } else if control.stage == 3 {
      script_sources_stage_three(pre, witness, auxiliary, control)
    } else if control.stage == 4 {
      script_sources_stage_four(pre, witness, auxiliary, control, door)
    } else if control.stage == 5 {
      script_sources_stage_five(pre, witness, auxiliary, control)
    } else if control.stage == 6 {
      script_sources_stage_six(pre, witness, auxiliary, control, door)
    } else if control.stage == 7 {
      script_sources_stage_seven_with_observer_commitment(
        pre,
        witness,
        auxiliary,
        control,
        verified_source.tx_compact.body.required_observers_hash,
        verified_source,
        dispatch_witness_set,
        door,
      )
    } else if control.stage == 8 {
      script_sources_stage_eight(pre, witness, auxiliary, control)
    } else if control.stage == 9 {
      script_sources_stage_nine(pre, witness, auxiliary, control)
    } else if control.stage == 10 {
      script_sources_stage_ten(pre, witness, auxiliary, control)
    } else if control.stage == 11 {
      script_sources_stage_eleven(pre, witness, auxiliary, control)
    } else if control.stage == 12 {
      script_sources_stage_twelve(pre, witness, auxiliary, control)
    } else {
      False
    },
  }
}
