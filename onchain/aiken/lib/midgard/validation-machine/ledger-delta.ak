use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/ledger_output_commitment_v1
use midgard/mpf_proof_fold_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  LedgerDeltaControlV1, LedgerDeltaOperationProofV1, LedgerDeltaOperationWitness,
  LedgerDeltaOutputWitness, LedgerDeltaPendingMutationV1,
  LedgerDeltaProofFrameWitness, LedgerDeltaReplayWitness, NoAuxiliaryWitness,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  ledger_delta_operation_leaf_hash, max_tx_size_derived_collection_item_count,
  no_rejection_code_hash,
}
use midgard/validation_machine/witness.{
  empty_resolution_schedule_hash, encode_ledger_delta_control_v1,
  encode_ledger_delta_pending_mutation_v1, encode_terminal_acceptance_witness_v1,
  ledger_delta_control_from_witness, ledger_delta_control_is_well_formed,
  ledger_delta_pending_mutation_from_cbor, resolution_schedule_node_hash,
  resolved_input_accumulator_successor,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn ledger_delta_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_control: LedgerDeltaControlV1,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.LedgerDelta,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      pre.program_counter + 1,
      encode_ledger_delta_control_v1(next_control),
    ),
  }
}

fn ledger_delta_operation_proof_is_valid(
  pre: validation_trace_v1.ValidationMachineStateV1,
  control: LedgerDeltaControlV1,
  operation_kind: Int,
  key: ByteArray,
  value: ByteArray,
  proof: LedgerDeltaOperationProofV1,
) -> Bool {
  and {
    proof.operation_count >= 0,
    proof.operation_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      proof.operation_count,
      proof.operation_peaks,
    ),
    validation_merkle_v1.frontier_commitment(
      proof.operation_count,
      proof.operation_peaks,
    ) == pre.ledger_delta_root,
    proof.operation_index == control.operation_count,
    proof.operation_index >= 0,
    proof.operation_index < proof.operation_count,
    validation_merkle_v1.verify_membership(
      proof.operation_count,
      proof.operation_peaks,
      proof.operation_index,
      ledger_delta_operation_leaf_hash(
        operation_kind,
        key,
        value,
        proof.descriptor,
      ),
      proof.operation_siblings,
    ),
  }
}

fn ledger_delta_operation_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  expect
      LedgerDeltaOperationWitness {
        operation_kind,
        key,
        value,
        operation_proof,
      }
    = auxiliary
  let expected_output_key = encode_midgard_tx_input(
    MidgardTxInput {
      tx_id: pre.transaction_id,
      output_index: control.output_cursor,
    },
  )
  and {
    control.pending_mutation_cbor == #"",
    if control.stage == 0 {
      and {
        operation_kind == 0,
        value == #"",
      }
    } else {
      and {
        control.stage == 1,
        operation_kind == 1,
        key == expected_output_key,
      }
    },
    ledger_delta_operation_proof_is_valid(
      pre,
      control,
      operation_kind,
      key,
      value,
      operation_proof,
    ),
    ledger_delta_successor_is_exact(
      pre,
      witness,
      LedgerDeltaControlV1 {
        ..control,
        pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
          LedgerDeltaPendingMutationV1 {
            authorization_stage: 0,
            operation_kind,
            key,
            value,
            descriptor: operation_proof.descriptor,
            fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
              next_frame_index: -1,
              expected_next_cursor: 0,
              including_root: #"",
              excluding_root: #"",
            },
          },
        ),
      },
    ),
  }
}

fn ledger_delta_pending_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  let pending = ledger_delta_pending_mutation_from_cbor(
    control.pending_mutation_cbor,
  )
  expect pending.authorization_stage == 1
  if mpf_proof_fold_v1.fold_is_complete_v1(pending.fold_control) {
    let Pair(expected_current_root, next_root) = if pending.operation_kind == 0 {
      Pair(
        pending.fold_control.including_root,
        pending.fold_control.excluding_root,
      )
    } else {
      Pair(
        pending.fold_control.excluding_root,
        pending.fold_control.including_root,
      )
    }
    let operation_value = if pending.operation_kind == 0 {
      #""
    } else {
      pending.value
    }
    let next_peaks = validation_merkle_v1.append_leaf(
      control.operation_count,
      control.operation_peaks,
      ledger_delta_operation_leaf_hash(
        pending.operation_kind,
        pending.key,
        operation_value,
        pending.descriptor,
      ),
    )
    and {
      auxiliary == NoAuxiliaryWitness,
      control.current_ledger_root == expected_current_root,
      ledger_delta_successor_is_exact(
        pre,
        witness,
        LedgerDeltaControlV1 {
          ..control,
          current_ledger_root: next_root,
          operation_count: control.operation_count + 1,
          operation_peaks: next_peaks,
          pending_mutation_cbor: #"",
        },
      ),
    }
  } else {
    expect LedgerDeltaProofFrameWitness { frame, siblings } = auxiliary
    when
      mpf_proof_fold_v1.fold_proof_frame_v1(
        pending.key,
        pending.descriptor,
        pending.fold_control,
        frame,
        siblings,
      )
    is {
      None -> False
      Some(next_fold_control) ->
        ledger_delta_successor_is_exact(
          pre,
          witness,
          LedgerDeltaControlV1 {
            ..control,
            pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
              LedgerDeltaPendingMutationV1 {
                ..pending,
                fold_control: next_fold_control,
              },
            ),
          },
        )
    }
  }
}

fn ledger_delta_replay_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  expect
      LedgerDeltaReplayWitness { source_kind, key, next_schedule_hash, value }
    = auxiliary
  let descriptor = ledger_output_commitment_v1.decode(value)
  let input = decode_midgard_tx_input_cbor(key)
  if and {
    source_kind == 0 || source_kind == 1,
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
    control.replay_cursor < control.resolved_input_count,
    input.output_index == descriptor.output_index,
  } {
    let next_accumulator = resolved_input_accumulator_successor(
      control.replay_accumulator,
      source_kind,
      key,
      value,
    )
    if source_kind == 1 {
      and {
        control.pending_mutation_cbor == #"",
        ledger_delta_successor_is_exact(
          pre,
          witness,
          LedgerDeltaControlV1 {
            ..control,
            replay_cursor: control.replay_cursor + 1,
            replay_accumulator: next_accumulator,
            replay_remaining_schedule_hash: next_schedule_hash,
          },
        ),
      }
    } else {
      expect control.pending_mutation_cbor != #""
      let pending = ledger_delta_pending_mutation_from_cbor(
        control.pending_mutation_cbor,
      )
      when
        mpf_proof_fold_v1.initial_fold_control_v1(
          key,
          value,
          pending.descriptor,
        )
      is {
        None -> False
        Some(fold_control) -> and {
            pending.authorization_stage == 0,
            pending.operation_kind == 0,
            pending.key == key,
            pending.value == #"",
            ledger_delta_successor_is_exact(
              pre,
              witness,
              LedgerDeltaControlV1 {
                ..control,
                replay_cursor: control.replay_cursor + 1,
                replay_accumulator: next_accumulator,
                replay_remaining_schedule_hash: next_schedule_hash,
                pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
                  LedgerDeltaPendingMutationV1 {
                    ..pending,
                    authorization_stage: 1,
                    value,
                    fold_control,
                  },
                ),
              },
            ),
          }
      }
    }
  } else {
    False
  }
}

fn ledger_delta_stage_one(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  if control.pending_mutation_cbor != #"" {
    let pending = ledger_delta_pending_mutation_from_cbor(
      control.pending_mutation_cbor,
    )
    if pending.authorization_stage == 0 {
      ledger_delta_replay_step(pre, witness, auxiliary, control)
    } else {
      ledger_delta_pending_step(pre, witness, auxiliary, control)
    }
  } else if control.replay_remaining_schedule_hash == empty_resolution_schedule_hash() {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.replay_cursor == control.resolved_input_count,
      control.replay_accumulator == control.resolved_inputs_accumulator,
      ledger_delta_successor_is_exact(
        pre,
        witness,
        LedgerDeltaControlV1 { ..control, stage: 1 },
      ),
    }
  } else {
    ledger_delta_replay_step(pre, witness, auxiliary, control)
  }
}

fn ledger_delta_output_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  expect LedgerDeltaOutputWitness { output_index, descriptor_cbor, siblings } = auxiliary
  let key = encode_midgard_tx_input(
    MidgardTxInput { tx_id: pre.transaction_id, output_index },
  )
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  expect control.pending_mutation_cbor != #""
  let pending = ledger_delta_pending_mutation_from_cbor(
    control.pending_mutation_cbor,
  )
  when
    mpf_proof_fold_v1.initial_fold_control_v1(
      key,
      descriptor_cbor,
      pending.descriptor,
    )
  is {
    None -> False
    Some(fold_control) -> and {
        output_index == control.output_cursor,
        descriptor.output_index == output_index,
        pending.authorization_stage == 0,
        pending.operation_kind == 1,
        pending.key == key,
        pending.value == descriptor_cbor,
        validation_merkle_v1.verify_membership(
          control.output_count,
          control.output_descriptor_peaks,
          output_index,
          script_proof_v1.output_descriptor_leaf_hash(
            output_index,
            descriptor_cbor,
          ),
          siblings,
        ),
        ledger_delta_successor_is_exact(
          pre,
          witness,
          LedgerDeltaControlV1 {
            ..control,
            output_cursor: control.output_cursor + 1,
            pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
              LedgerDeltaPendingMutationV1 {
                ..pending,
                authorization_stage: 1,
                fold_control,
              },
            ),
          },
        ),
      }
  }
}

fn ledger_delta_stage_two(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  if and {
    control.pending_mutation_cbor != #"",
    ledger_delta_pending_mutation_from_cbor(control.pending_mutation_cbor).authorization_stage == 1,
  } {
    ledger_delta_pending_step(pre, witness, auxiliary, control)
  } else if and {
    control.pending_mutation_cbor == #"",
    control.output_cursor == control.output_count,
  } {
    and {
      auxiliary == NoAuxiliaryWitness,
      ledger_delta_successor_is_exact(
        pre,
        witness,
        LedgerDeltaControlV1 { ..control, stage: 2 },
      ),
    }
  } else {
    expect LedgerDeltaOutputWitness { output_index, descriptor_cbor, siblings }
      = auxiliary
    let key = encode_midgard_tx_input(
      MidgardTxInput { tx_id: pre.transaction_id, output_index },
    )
    let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
    expect control.pending_mutation_cbor != #""
    let pending = ledger_delta_pending_mutation_from_cbor(
      control.pending_mutation_cbor,
    )
    when
      mpf_proof_fold_v1.initial_fold_control_v1(
        key,
        descriptor_cbor,
        pending.descriptor,
      )
    is {
      None -> False
      Some(fold_control) -> and {
          output_index == control.output_cursor,
          descriptor.output_index == output_index,
          pending.authorization_stage == 0,
          pending.operation_kind == 1,
          pending.key == key,
          pending.value == descriptor_cbor,
          validation_merkle_v1.verify_membership(
            control.output_count,
            control.output_descriptor_peaks,
            output_index,
            script_proof_v1.output_descriptor_leaf_hash(
              output_index,
              descriptor_cbor,
            ),
            siblings,
          ),
          ledger_delta_successor_is_exact(
            pre,
            witness,
            LedgerDeltaControlV1 {
              ..control,
              output_cursor: control.output_cursor + 1,
              pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
                LedgerDeltaPendingMutationV1 {
                  ..pending,
                  authorization_stage: 1,
                  fold_control,
                },
              ),
            },
          ),
        }
    }
  }
}

fn ledger_delta_stage_three(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    control.replay_cursor == control.resolved_input_count,
    control.replay_accumulator == control.resolved_inputs_accumulator,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.output_cursor == control.output_count,
    control.pending_mutation_cbor == #"",
    validation_merkle_v1.frontier_commitment(
      control.operation_count,
      control.operation_peaks,
    ) == pre.ledger_delta_root,
    witness.claimed_successor.phase == validation_trace_v1.Terminal,
    witness.claimed_successor.verdict == validation_trace_v1.Accepted,
    witness.claimed_successor.rejection_code_hash == no_rejection_code_hash,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      encode_terminal_acceptance_witness_v1(
        control.current_ledger_root,
        control.operation_count,
        control.operation_peaks,
      ),
    ),
  }
}

pub fn verify_ledger_delta(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    control.resolved_input_count >= 0,
    control.resolved_input_count <= max_tx_size_derived_collection_item_count,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    control.output_count >= 0,
    control.output_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_descriptor_peaks,
    ),
    bytearray.length(control.replay_schedule_hash) == 32,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    bytearray.length(control.current_ledger_root) == 32,
    control.replay_cursor >= 0,
    control.replay_cursor <= control.resolved_input_count,
    control.output_cursor >= 0,
    control.output_cursor <= control.output_count,
    control.operation_count >= 0,
    control.operation_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.operation_count,
      control.operation_peaks,
    ),
    control.stage >= 0,
    control.stage <= 2,
    witness.work_witness_cbor == encode_ledger_delta_control_v1(control),
    when auxiliary is {
      LedgerDeltaOperationWitness { .. } ->
        ledger_delta_operation_step(pre, witness, auxiliary, control)
      _ ->
        if control.stage == 0 {
          ledger_delta_stage_one(pre, witness, auxiliary, control)
        } else if control.stage == 1 {
          ledger_delta_stage_two(pre, witness, auxiliary, control)
        } else {
          ledger_delta_stage_three(pre, witness, auxiliary, control)
        }
    },
  }
}

pub fn verify_ledger_delta_replay_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.pending_mutation_cbor == #"",
    control.stage == 0,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.replay_cursor == control.resolved_input_count,
    control.replay_accumulator == control.resolved_inputs_accumulator,
    ledger_delta_successor_is_exact(
      pre,
      witness,
      LedgerDeltaControlV1 { ..control, stage: 1 },
    ),
  }
}

pub fn verify_ledger_delta_output_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.pending_mutation_cbor == #"",
    control.stage == 1,
    control.output_cursor == control.output_count,
    ledger_delta_successor_is_exact(
      pre,
      witness,
      LedgerDeltaControlV1 { ..control, stage: 2 },
    ),
  }
}

pub fn verify_ledger_delta_terminal_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.pending_mutation_cbor == #"",
    control.stage == 2,
    ledger_delta_stage_three(pre, witness, NoAuxiliaryWitness, control),
  }
}

pub fn verify_ledger_delta_operation_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  operation_kind: Int,
  key: ByteArray,
  value: ByteArray,
  operation_proof: LedgerDeltaOperationProofV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.pending_mutation_cbor == #"",
    control.stage == 0 || control.stage == 1,
    ledger_delta_operation_step(
      pre,
      witness,
      LedgerDeltaOperationWitness {
        operation_kind,
        key,
        value,
        operation_proof,
      },
      control,
    ),
  }
}

pub fn verify_ledger_delta_replay_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.stage == 0,
    control.replay_remaining_schedule_hash != empty_resolution_schedule_hash(),
    if source_kind == 0 {
      control.pending_mutation_cbor != #""
    } else {
      control.pending_mutation_cbor == #""
    },
    ledger_delta_replay_step(
      pre,
      witness,
      LedgerDeltaReplayWitness { source_kind, key, next_schedule_hash, value },
      control,
    ),
  }
}

pub fn verify_ledger_delta_output_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  output_index: Int,
  descriptor_cbor: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.stage == 1,
    control.pending_mutation_cbor != #"",
    control.output_cursor < control.output_count,
    ledger_delta_output_step(
      pre,
      witness,
      LedgerDeltaOutputWitness { output_index, descriptor_cbor, siblings },
      control,
    ),
  }
}

pub fn verify_ledger_delta_proof_frame_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  frame: mpf_proof_fold_v1.ProofFrameV1,
  siblings: List<ByteArray>,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  expect control.pending_mutation_cbor != #""
  let pending = ledger_delta_pending_mutation_from_cbor(
    control.pending_mutation_cbor,
  )
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.stage == 0 || control.stage == 1,
    pending.authorization_stage == 1,
    !mpf_proof_fold_v1.fold_is_complete_v1(pending.fold_control),
    ledger_delta_pending_step(
      pre,
      witness,
      LedgerDeltaProofFrameWitness { frame, siblings },
      control,
    ),
  }
}

pub fn verify_ledger_delta_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  expect control.pending_mutation_cbor != #""
  let pending = ledger_delta_pending_mutation_from_cbor(
    control.pending_mutation_cbor,
  )
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.stage == 0 || control.stage == 1,
    pending.authorization_stage == 1,
    mpf_proof_fold_v1.fold_is_complete_v1(pending.fold_control),
    ledger_delta_pending_step(pre, witness, NoAuxiliaryWitness, control),
  }
}
