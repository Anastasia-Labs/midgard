use aiken/merkle_patricia_forestry.{Proof}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_input_cbor}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/ledger_output_v1
use midgard/mpf_proof_v1
use midgard/rejection_reason_v1.{
  reject_input_not_found, reject_invalid_field_type, reject_invalid_output,
  reject_missing_required_witness, reject_native_script_depth,
  reject_native_script_node_count, reject_validity_interval_mismatch,
}
use midgard/validation_machine/machine_types.{
  InputSignerAuthorized, InputSignerMissing, InputSignerProofMalformed,
  LedgerOutputProofFinalizeWitness, LedgerOutputProofStepWitness,
  NoAuxiliaryWitness, NoSignerSetProof, ResolveInputOutputProofV1,
  ResolveInputsControlV1, ScheduledLedgerMembershipWitness,
  ScheduledLedgerNonMembershipWitness, SignerSetProofV1,
  ValidationAuxiliaryWitnessV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/shared.{
  decode_validation_context, input_signer_authorization,
  rejected_successor_is_exact, validity_interval_contains_slot,
}
use midgard/validation_machine/witness.{
  empty_mint_fold_control, empty_observer_purpose_scan_control,
  empty_receive_purpose_scan_control, empty_resolution_schedule_hash,
  encode_resolve_inputs_witness, encode_script_sources_witness,
  initial_resolution_accumulator, resolution_schedule_node_hash,
  resolve_inputs_control_from_witness, resolve_inputs_control_is_bound,
  resolved_input_accumulator_successor,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

fn resolve_inputs_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_control: ResolveInputsControlV1,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      pre.program_counter + 1,
      encode_resolve_inputs_witness(
        next_control.compact_cbor,
        next_control.witness_set_compact_cbor,
        next_control.field_preimage_lengths_cbor,
        next_control.context_cbor,
        next_control.cursor,
        next_control.accumulator,
        next_control.remaining_schedule_hash,
        next_control.signer_count,
        next_control.signer_frontier_commitment,
        next_control.pending,
        next_control.resolution_schedule_hash,
      ),
    ),
  }
}

fn resolve_membership_begin_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  proof: Proof,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  let input = decode_midgard_tx_input_cbor(key)
  and {
    control.pending == None,
    signer_proof == NoSignerSetProof,
    source_kind == 0 || source_kind == 1,
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.remaining_schedule_hash,
    input.output_index == descriptor.output_index,
    descriptor.total_length > 0,
    mpf_proof_v1.has(pre.prior_ledger_root, key, descriptor_cbor, proof),
    resolve_inputs_successor_is_exact(
      pre,
      witness,
      ResolveInputsControlV1 {
        ..control,
        pending: Some(
          ResolveInputOutputProofV1 {
            source_kind,
            key,
            next_schedule_hash,
            descriptor_cbor,
            output_proof: ledger_output_proof_v1.initial_control_v1(
              descriptor.output_index,
              descriptor.total_length,
              descriptor.item_commitment,
            ),
          },
        ),
      },
    ),
  }
}

fn resolve_membership_proof_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  pending: ResolveInputOutputProofV1,
  proof_witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
) -> Bool {
  when ledger_output_proof_v1.step_v1(pending.output_proof, proof_witness) is {
    None -> False
    Some(result) ->
      when result is {
        ledger_output_proof_v1.LedgerOutputProofAdvanced {
          control: next_output_proof,
        } ->
          resolve_inputs_successor_is_exact(
            pre,
            witness,
            ResolveInputsControlV1 {
              ..control,
              pending: Some(
                ResolveInputOutputProofV1 {
                  ..pending,
                  output_proof: next_output_proof,
                },
              ),
            },
          )
        ledger_output_proof_v1.LedgerOutputProofInvalidOutput ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_output,
          )
        ledger_output_proof_v1.LedgerOutputProofInvalidReferenceScript ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        ledger_output_proof_v1.LedgerOutputProofNativeScriptNodeLimit ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_node_count,
          )
        ledger_output_proof_v1.LedgerOutputProofNativeScriptDepthLimit ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_depth,
          )
      }
  }
}

fn resolve_membership_proof_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  pending: ResolveInputOutputProofV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  if and {
    descriptor_cbor == pending.descriptor_cbor,
    ledger_output_proof_v1.descriptor_is_exact_v1(
      pending.output_proof,
      descriptor,
    ),
  } {
    expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
      descriptor.address,
    )
    when
      input_signer_authorization(
        pending.source_kind,
        address,
        control.signer_count,
        control.signer_frontier_commitment,
        signer_proof,
      )
    is {
      InputSignerMissing ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_missing_required_witness,
        )
      InputSignerProofMalformed -> False
      InputSignerAuthorized ->
        resolve_inputs_successor_is_exact(
          pre,
          witness,
          ResolveInputsControlV1 {
            ..control,
            cursor: control.cursor + 1,
            accumulator: resolved_input_accumulator_successor(
              control.accumulator,
              pending.source_kind,
              pending.key,
              descriptor_cbor,
            ),
            remaining_schedule_hash: pending.next_schedule_hash,
            pending: None,
          },
        )
    }
  } else {
    False
  }
}

fn resolve_non_membership_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  proof: Proof,
) -> Bool {
  and {
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.remaining_schedule_hash,
    mpf_proof_v1.does_not_have(pre.prior_ledger_root, key, proof),
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_input_not_found,
    ),
  }
}

fn resolve_lookup_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ResolveInputsControlV1,
) -> Bool {
  when control.pending is {
    Some(pending) ->
      when auxiliary is {
        LedgerOutputProofStepWitness { witness: proof_witness } ->
          resolve_membership_proof_step(
            pre,
            witness,
            control,
            pending,
            proof_witness,
          )
        LedgerOutputProofFinalizeWitness { descriptor_cbor, signer_proof } ->
          resolve_membership_proof_finalize(
            pre,
            witness,
            control,
            pending,
            descriptor_cbor,
            signer_proof,
          )
        _ -> False
      }
    None ->
      when auxiliary is {
        ScheduledLedgerMembershipWitness {
          source_kind,
          key,
          next_schedule_hash,
          value,
          proof,
          signer_proof,
        } ->
          resolve_membership_begin_step(
            pre,
            witness,
            control,
            source_kind,
            key,
            next_schedule_hash,
            value,
            proof,
            signer_proof,
          )
        ScheduledLedgerNonMembershipWitness {
          source_kind,
          key,
          next_schedule_hash,
          proof,
        } ->
          resolve_non_membership_step(
            pre,
            witness,
            control,
            source_kind,
            key,
            next_schedule_hash,
            proof,
          )
        _ -> False
      }
  }
}

pub fn verify_resolve_inputs(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let context = decode_validation_context(control.context_cbor)
  and {
    verified.version == 1,
    resolve_inputs_control_is_bound(pre, witness, control),
    if control.cursor == 0 {
      and {
        auxiliary == NoAuxiliaryWitness,
        if validity_interval_contains_slot(
          verified.tx_compact.body,
          context.block_slot,
        ) {
          let next_control = encode_resolve_inputs_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            1,
            control.accumulator,
            control.remaining_schedule_hash,
            control.signer_count,
            control.signer_frontier_commitment,
            None,
            control.resolution_schedule_hash,
          )
          and {
            witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
            witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
              validation_trace_v1.ResolveInputs,
              pre.program_counter + 1,
              next_control,
            ),
          }
        } else {
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_validity_interval_mismatch,
          )
        },
      }
    } else if and {
      control.pending == None,
      control.remaining_schedule_hash == empty_resolution_schedule_hash(),
    } {
      and {
        auxiliary == NoAuxiliaryWitness,
        witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ScriptSources,
          pre.program_counter + 1,
          encode_script_sources_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            control.cursor - 1,
            control.accumulator,
            control.signer_count,
            control.signer_frontier_commitment,
            validation_merkle_v1.empty_frontier(),
            0,
            0,
            validation_merkle_v1.empty_frontier(),
            0,
            validation_merkle_v1.empty_frontier(),
            0,
            initial_resolution_accumulator(),
            empty_resolution_schedule_hash(),
            0,
            0,
            validation_merkle_v1.empty_frontier(),
            0,
            0,
            validation_merkle_v1.empty_frontier(),
            0,
            empty_receive_purpose_scan_control(),
            0,
            0,
            empty_observer_purpose_scan_control(),
            empty_mint_fold_control(),
            control.resolution_schedule_hash,
          ),
        ),
      }
    } else {
      resolve_lookup_step(pre, witness, auxiliary, control)
    },
  }
}

pub fn verify_resolve_inputs_initial_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let context = decode_validation_context(control.context_cbor)
  and {
    verified.version == 1,
    resolve_inputs_control_is_bound(pre, witness, control),
    control.cursor == 0,
    control.pending == None,
    if validity_interval_contains_slot(
      verified.tx_compact.body,
      context.block_slot,
    ) {
      let next_control = encode_resolve_inputs_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        1,
        control.accumulator,
        control.remaining_schedule_hash,
        control.signer_count,
        control.signer_frontier_commitment,
        None,
        control.resolution_schedule_hash,
      )
      and {
        witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ResolveInputs,
          pre.program_counter + 1,
          next_control,
        ),
      }
    } else {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_validity_interval_mismatch,
      )
    },
  }
}

pub fn verify_resolve_inputs_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  and {
    resolve_inputs_control_is_bound(pre, witness, control),
    control.cursor > 0,
    control.pending == None,
    control.remaining_schedule_hash == empty_resolution_schedule_hash(),
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      encode_script_sources_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.cursor - 1,
        control.accumulator,
        control.signer_count,
        control.signer_frontier_commitment,
        validation_merkle_v1.empty_frontier(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        initial_resolution_accumulator(),
        empty_resolution_schedule_hash(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        empty_receive_purpose_scan_control(),
        0,
        0,
        empty_observer_purpose_scan_control(),
        empty_mint_fold_control(),
        control.resolution_schedule_hash,
      ),
    ),
  }
}

pub fn verify_resolve_inputs_membership_begin_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
  proof: Proof,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  and {
    resolve_inputs_control_is_bound(pre, witness, control),
    control.cursor > 0,
    control.pending == None,
    control.remaining_schedule_hash != empty_resolution_schedule_hash(),
    resolve_membership_begin_step(
      pre,
      witness,
      control,
      source_kind,
      key,
      next_schedule_hash,
      value,
      proof,
      signer_proof,
    ),
  }
}

pub fn verify_resolve_inputs_membership_step_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  proof_witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  when control.pending is {
    None -> False
    Some(pending) -> and {
        resolve_inputs_control_is_bound(pre, witness, control),
        resolve_membership_proof_step(
          pre,
          witness,
          control,
          pending,
          proof_witness,
        ),
      }
  }
}

pub fn verify_resolve_inputs_membership_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  when control.pending is {
    None -> False
    Some(pending) -> and {
        resolve_inputs_control_is_bound(pre, witness, control),
        resolve_membership_proof_finalize(
          pre,
          witness,
          control,
          pending,
          descriptor_cbor,
          signer_proof,
        ),
      }
  }
}

pub fn verify_resolve_inputs_non_membership_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  proof: Proof,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  and {
    resolve_inputs_control_is_bound(pre, witness, control),
    control.cursor > 0,
    control.pending == None,
    control.remaining_schedule_hash != empty_resolution_schedule_hash(),
    resolve_non_membership_step(
      pre,
      witness,
      control,
      source_kind,
      key,
      next_schedule_hash,
      proof,
    ),
  }
}
