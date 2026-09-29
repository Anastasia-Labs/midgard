use aiken/builtin
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet, MidgardVersionedScript,
  PlutusV3Script, native_tx_version_v1,
}
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/script_sources_early_stages as vm_script_sources_early_stages
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{h_b, validation_context_cbor}
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, forced_pending_state, inline_field_door,
  signer_frontier_for_tx, singleton_field_item_chunk_proof, submitted_source,
  v1_program_envelope,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub fn stage_zero_script_sources_control(
  tx: MidgardTransaction,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  source_total_count: Int,
  pending_source_cbor: ByteArray,
) -> vm_machine_types.ScriptSourcesControlV1 {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  vm_machine_types.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: vm_witness.initial_resolution_accumulator(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    stage: 0,
    source_count: source_frontier.count,
    source_peaks: source_frontier.peaks,
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    replay_cursor: 0,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_cursor: 0,
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_total_count: 0,
    receive_scan: vm_witness.empty_receive_purpose_scan_control(),
    source_total_count,
    redeemer_total_count: 0,
    observer_scan: vm_witness.empty_observer_purpose_scan_control(),
    discovery: vm_witness.empty_script_discovery_control(),
    output_proof: None,
    pending_source_cbor,
    mint_fold: vm_witness.empty_mint_fold_control(),
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

pub fn stage_zero_script_sources_work_witness(
  control: vm_machine_types.ScriptSourcesControlV1,
) -> ByteArray {
  if control.pending_source_cbor == #"" {
    vm_witness.encode_script_sources_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolved_input_count,
      control.resolved_inputs_accumulator,
      control.signer_count,
      control.signer_frontier_commitment,
      control.resolved_item_peaks,
      control.stage,
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
  } else {
    vm_witness.encode_script_sources_pending_source_witness(
      control,
      control.pending_source_cbor,
    )
  }
}

pub fn stage_zero_script_sources_state(
  tx: MidgardTransaction,
  control: vm_machine_types.ScriptSourcesControlV1,
  program_counter: Int,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.ScriptSources,
    program_counter,
    work_witness_cbor,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
}

pub fn stage_zero_hash_block_checks_binding(wrong_commitment: Bool) -> Bool {
  let base = empty_v1_transaction()
  let payload = v1_program_envelope()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: payload,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let item_cbor = encode_midgard_versioned_script(script)
  let chunk_proof = singleton_field_item_chunk_proof(6, item_cbor)
  let hash_control = blake2b_224_trace_v1.initial_control_v1(
    bytearray.length(payload) + 1,
  )
  let pending = vm_machine_types.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment: bounded_item_v1.from_bytes(6, 0, item_cbor),
    hash_control,
  }
  let pending_cbor = vm_witness.encode_inline_source_hash_control_v1(pending)
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    1,
    pending_cbor,
  )
  let block = #"03" |> bytearray.concat(payload)
  expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
    hash_control,
    Some(block),
  )
  let next_pending_cbor = vm_witness.encode_inline_source_hash_control_v1(
    vm_machine_types.InlineSourceHashControlV1 {
      ..pending,
      hash_control: next_hash_control,
    },
  )
  let successor_control = vm_machine_types.ScriptSourcesControlV1 {
    ..control,
    pending_source_cbor: next_pending_cbor,
  }
  let pre = stage_zero_script_sources_state(tx, control, 60)
  let post = stage_zero_script_sources_state(tx, successor_control, 61)
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_zero_script_sources_work_witness(control),
    claimed_successor: post,
  }
  and {
    vm_script_sources.verify_script_sources_stage_zero_semantics_v1(
      pre,
      transition,
      vm_machine_types.ScriptSourceHashBlockWitness {
        chunk_proof,
        next_chunk_proof: None,
      },
      inline_field_door(),
    ),
    vm_script_sources_early_stages.verify_script_sources_stage_zero_hash_block_semantics_v1(
      pre,
      transition,
      chunk_proof,
      None,
    ),
    if wrong_commitment {
      vm_script_sources_early_stages.verify_script_sources_stage_zero_hash_block_semantics_v1(
        validation_trace_v1.ValidationMachineStateV1 {
          ..pre,
          transaction_commitment: h_b,
        },
        transition,
        chunk_proof,
        None,
      )
    } else {
      True
    },
  }
}

pub fn crossing_inline_source_hash_fixture() -> Pair<
  vm_machine_types.InlineSourceHashControlV1,
  Pair<bounded_item_v1.ChunkProofV1, bounded_item_v1.ChunkProofV1>,
> {
  let payload = builtin.replicate_byte(4_200, 171)
  let item_cbor = encode_midgard_versioned_script(
    MidgardVersionedScript { language: PlutusV3Script, script_bytes: payload },
  )
  let first_chunk = builtin.slice_bytearray(
    0,
    bounded_item_v1.chunk_bytes,
    item_cbor,
  )
  let second_chunk = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes,
    bytearray.length(item_cbor) - bounded_item_v1.chunk_bytes,
    item_cbor,
  )
  let first_hash = bounded_item_v1.hash_chunk(6, 0, 0, first_chunk)
  let second_hash = bounded_item_v1.hash_chunk(6, 0, 1, second_chunk)
  let frontier = validation_merkle_v1.build_frontier([first_hash, second_hash])
  let item_commitment = bounded_item_v1.commitment(
    6,
    0,
    bytearray.length(item_cbor),
    frontier.peaks,
  )
  let pending = vm_machine_types.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment,
    hash_control: blake2b_224_trace_v1.Blake2b224TraceControlV1 {
      version: 1,
      stage: blake2b_224_trace_v1.stage_ready,
      cursor: 3_968,
      total_length: bytearray.length(payload) + 1,
      chaining_value: builtin.replicate_byte(64, 17),
      active_block: #"",
      active_block_length: 0,
      working_value: #"",
      round: 0,
    },
  }
  Pair(
    pending,
    Pair(
      bounded_item_v1.ChunkProofV1 {
        version: 1,
        field_index: 6,
        item_index: 0,
        total_length: bytearray.length(item_cbor),
        chunk_index: 0,
        chunk: first_chunk,
        frontier: frontier.peaks,
        siblings: [second_hash],
      },
      bounded_item_v1.ChunkProofV1 {
        version: 1,
        field_index: 6,
        item_index: 0,
        total_length: bytearray.length(item_cbor),
        chunk_index: 1,
        chunk: second_chunk,
        frontier: frontier.peaks,
        siblings: [first_hash],
      },
    ),
  )
}

// ====================================================================// VM-STAGE-DIFFERENTIAL -- per-stage route agreement controls.
//
// Every ScriptSources stage is implemented twice.
//
//   * once as the narrow sub-case entry point a deployed validator calls,
//     `verify_script_sources_stage_<n>_<case>_semantics_v1` (for example
//     `validators/fraud-proofs/validation-trace/script-sources-stage-eight-finish-semantic-v1.ak`
//     calls `verify_script_sources_stage_eight_finish_semantics_v1`);
//   * once inside the generic dispatcher `script_sources_stage_<n>`, reached
//     through `verify_script_sources_stage_<n>_semantics_v1`,
//     `verify_script_sources_non_output_semantics_v1` and
//     `verify_one_step_evidence`.
//
// Two of the four confirmed production defects on this branch came from that
// single structural cause: the stage-7 deployed successor checks demanded the
// Plutus `serialiseData` successor encoding while the evidence path's
// `script_sources_control_successor_is_exact` demanded the canonical
// definite-length encoding, so the deployed stage-7 path was unsatisfiable for
// every transaction while the evidence path worked.
//
// The controls below pin the two routes together on ONE shared fixture per
// stage, for an accepting transition AND for a mutated transition. Agreement
// on rejection matters as much as agreement on acceptance: a deployed route
// that is *wider* than the canonical rule is a soundness break of the dispute
// game (a step `verify_one_step` rejects can be won on-chain), while a
// deployed route that is narrower is a liveness break. A happy-path-only guard
// would not have caught the mutated-successor half of defect 4.
//
// Each test is deliberately built from the smallest fixture that reaches the
// stage -- this module is under aiken#1389, so every extra fixture costs
// compile minutes.
// ====================================================================
/// The deployed narrow route and the generic route must return the same
/// verdict, and that verdict must be `expected`. Both arguments are evaluated
/// before the comparison, so neither route can hide behind `and` short
/// circuiting.
pub fn stage_routes_agree(deployed: Bool, generic: Bool, expected: Bool) -> Bool {
  and {
    deployed == generic,
    deployed == expected,
  }
}
