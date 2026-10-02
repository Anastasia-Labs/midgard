use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/ledger_output_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/resolve_inputs as vm_resolve_inputs
use midgard/validation_machine/signatures as vm_signatures
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{h_a, validation_context_cbor}
use midgard/validation_machine_tests/fixtures_phase_a.{
  authenticated_output_material, empty_v1_transaction,
  resolve_inputs_step_fixture, signatures_scan_witness, signatures_step_fixture,
  signer_frontier_for_tx, singleton_root, submitted_source,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// Builds the Signatures->PhaseANativeScripts handoff, claiming a successor
// whose stage-0 phase-A native scan control carries the supplied `result`.
pub fn signatures_handoff_with_claimed_result(result: Int) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
  }
  let Pair(pre, _work_witness_cbor) = signatures_step_fixture(tx, h_a)
  let proof_source = submitted_source(tx)
  let signer_frontier = validation_merkle_v1.build_frontier([])
  let handoff_witness_cbor = signatures_scan_witness(
    tx,
    2,
    0,
    0,
    0,
    0,
    #"",
    #"",
    signer_frontier,
    0,
  )
  let handoff_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 6,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      6,
      handoff_witness_cbor,
    ),
  }
  let next_work_witness_cbor = vm_witness.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    0,
    0,
    0,
    0,
    0,
    #"",
    0,
    #"",
    0,
    0,
    result,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..handoff_pre,
    phase: validation_trace_v1.PhaseANativeScripts,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      7,
      next_work_witness_cbor,
    ),
  }
  vm_signatures.verify_signatures_handoff_semantics_v1(
    handoff_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: handoff_witness_cbor,
      claimed_successor: post,
    },
  )
}

pub fn resolve_inputs_authenticated_membership_step(step_to_verify: Int) -> Bool {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let output_cbor = #"a200581d70aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa018200a0"
  let material = authenticated_output_material(output_cbor)
  let descriptor_cbor = material.descriptor_cbor
  let root = singleton_root(key, descriptor_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    root,
    1,
    accumulator,
    8,
  )
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let empty_schedule_hash = vm_witness.empty_resolution_schedule_hash()
  let signer_frontier_commitment = validation_merkle_v1.frontier_commitment(
    signer_frontier.count,
    signer_frontier.peaks,
  )
  let pending_initial = vm_machine_types.ResolveInputOutputProofV1 {
    source_kind: 0,
    key,
    next_schedule_hash: empty_schedule_hash,
    descriptor_cbor,
    output_proof: material.initial,
  }
  let begin_post_cbor = vm_witness.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    accumulator,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    signer_frontier_commitment,
    Some(pending_initial),
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let begin_post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 9,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      9,
      begin_post_cbor,
    ),
  }
  let pending_first_successor = vm_machine_types.ResolveInputOutputProofV1 {
    ..pending_initial,
    output_proof: material.first_successor,
  }
  let step_post_cbor = vm_witness.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    accumulator,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    signer_frontier_commitment,
    Some(pending_first_successor),
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let step_post = validation_trace_v1.ValidationMachineStateV1 {
    ..begin_post,
    program_counter: 10,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      10,
      step_post_cbor,
    ),
  }
  let pending_terminal = vm_machine_types.ResolveInputOutputProofV1 {
    ..pending_initial,
    output_proof: material.facts_terminal,
  }
  let finalize_pre_cbor = vm_witness.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    accumulator,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    signer_frontier_commitment,
    Some(pending_terminal),
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let finalize_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..step_post,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      10,
      finalize_pre_cbor,
    ),
  }
  let next_accumulator = vm_witness.resolved_input_accumulator_successor(
    accumulator,
    0,
    key,
    descriptor_cbor,
  )
  let finalize_post_cbor = vm_witness.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    2,
    next_accumulator,
    empty_schedule_hash,
    signer_frontier.count,
    signer_frontier_commitment,
    None,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  let finalize_post = validation_trace_v1.ValidationMachineStateV1 {
    ..finalize_pre,
    program_counter: 11,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      11,
      finalize_post_cbor,
    ),
  }
  if step_to_verify == 0 {
    vm_resolve_inputs.verify_resolve_inputs_membership_begin_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: begin_post,
      },
      0,
      key,
      empty_schedule_hash,
      descriptor_cbor,
      [],
      vm_machine_types.NoSignerSetProof,
    )
  } else if step_to_verify == 1 {
    vm_resolve_inputs.verify_resolve_inputs_membership_step_semantics_v1(
      begin_post,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: begin_post_cbor,
        claimed_successor: step_post,
      },
      ledger_output_proof_v1.LedgerOutputProofChunks {
        chunk_proof: material.chunk_proof,
        next_chunk_proof: None,
      },
    )
  } else {
    and {
      step_to_verify == 2,
      vm_resolve_inputs.verify_resolve_inputs_membership_finalize_semantics_v1(
        finalize_pre,
        vm_machine_types.ValidationOneStepWitnessV1 {
          work_witness_cbor: finalize_pre_cbor,
          claimed_successor: finalize_post,
        },
        vm_machine_types.NoSignerSetProof,
      ),
    }
  }
}
