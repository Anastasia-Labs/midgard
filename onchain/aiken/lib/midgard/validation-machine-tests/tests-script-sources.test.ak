use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_redeemer_witness, encode_midgard_tx_input,
  encode_midgard_tx_output, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_mint_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardExecutionUnits, MidgardRedeemerWitness,
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, MidgardTxOutput, MidgardValue, MidgardVersionedScript,
  NativeCardanoScript, PlutusV3Script, PubKeyCredential, ScriptCredential,
  SpendRedeemer,
}
use midgard/native_tx_field_access_v1
use midgard/redeemer_item_proof_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/script_sources_early_stages as vm_script_sources_early_stages
use midgard/validation_machine/script_sources_late_stages as vm_script_sources_late_stages
use midgard/validation_machine/script_sources_middle_semantics as vm_middle_semantics
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{h_28_a, h_28_b, h_a}
use midgard/validation_machine_tests/fixtures_parity.{
  stage_twelve_finish_fixture, verify_descriptor_ten_match_parity,
  verify_descriptor_ten_mismatch_parity,
  verify_descriptor_twelve_redeemer_parity,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  authenticated_output_material, empty_v1_transaction, exact_rejection_post,
  first_leaf_power_of_two_siblings, indexed_inline_source_leaves,
  indexed_spend_purpose_leaves, inline_field_carriage, inline_field_door,
  native_scripts_step_fixture, one_step_evidence, script_discovery_step_fixture,
  script_sources_active_output_proof_fixture, script_sources_mint_fold_fixture,
  script_sources_output_step_fixture,
  script_sources_output_step_fixture_with_resolved_items,
  script_sources_pending_redeemer_item_fixture,
  script_sources_receive_scan_step_fixture, script_sources_step_fixture,
  script_sources_step_fixture_with_resolved_items,
  singleton_field_item_chunk_proof, submitted_source, v1_program_envelope,
}
use midgard/validation_machine_tests/fixtures_script_identity.{
  stage_zero_script_sources_control, stage_zero_script_sources_state,
  stage_zero_script_sources_work_witness,
}
use midgard/validation_machine_tests/fixtures_script_sources.{
  authenticated_output_proof_step, canonical_observer_purpose_step,
  canonical_output_commitment_output, canonical_output_commitment_output_cbor,
  canonical_output_commitment_step,
  canonical_output_commitment_step_with_successor, crossing_mint_fold_step,
  maximum_reference_input_schedule_terminal_vector,
  maximum_spend_input_schedule_terminal_vector, protected_receive_purpose_step,
  replay_output_descriptor_fixture, script_sources_rejects_wrong_output_network,
  script_sources_state_for_work_witness, serialise_data_reencoding,
  verify_maximum_input_schedule_terminal_vector,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

test script_sources_commits_inline_reference_script_candidates() {
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
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.source_leaf_hash(0, #"00", script)],
  )
  let item_cbor = encode_midgard_versioned_script(script)
  let carriage = inline_field_carriage([item_cbor])
  let pending = vm_machine_types.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment: bounded_item_v1.from_bytes(6, 0, item_cbor),
    hash_control: blake2b_224_trace_v1.initial_control_v1(
      bytearray.length(payload) + 1,
    ),
  }
  let initial_control = stage_zero_script_sources_control(
    tx,
    empty_frontier,
    0,
    #"",
  )
  let pending_control = vm_machine_types.ScriptSourcesControlV1 {
    ..initial_control,
    source_total_count: 1,
    pending_source_cbor: vm_witness.encode_inline_source_hash_control_v1(
      pending,
    ),
  }
  let committed_control = stage_zero_script_sources_control(
    tx,
    source_frontier,
    1,
    #"",
  )
  let final_control = vm_machine_types.ScriptSourcesControlV1 {
    ..committed_control,
    stage: 1,
  }
  let pre = stage_zero_script_sources_state(tx, initial_control, 9)
  let pending_post = stage_zero_script_sources_state(tx, pending_control, 10)
  let committed_pre = stage_zero_script_sources_state(tx, committed_control, 10)
  let final_post = stage_zero_script_sources_state(tx, final_control, 11)
  and {
    vm_script_sources_early_stages.verify_script_sources_stage_zero_begin_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: stage_zero_script_sources_work_witness(
          initial_control,
        ),
        claimed_successor: pending_post,
      },
      inline_field_door(),
      6,
      0,
      carriage,
    ),
    vm_script_sources_early_stages.verify_script_sources_stage_zero_finish_semantics_v1(
      committed_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: stage_zero_script_sources_work_witness(
          committed_control,
        ),
        claimed_successor: final_post,
      },
    ),
  }
}

test script_sources_commits_unique_supported_redeemers() {
  let base = empty_v1_transaction()
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"00",
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let redeemer_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.redeemer_leaf_hash(0, redeemer)],
  )
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let carriage = inline_field_carriage([redeemer_cbor])
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  // Since `320ed869` the stage-1 redeemer scan no longer commits the redeemer
  // leaf in a single step. `TransactionRedeemerItemBeginWitness` only opens the
  // item: it records `redeemer_total_count` and a pending
  // `redeemer_item_proof_v1` control hash (validation-machine-v1.ak:8580-8615).
  // A chain of `RedeemerItemStepWitness` steps then advances that control, and
  // only the terminal step appends the redeemer leaf and clears the pending
  // hash (:8637-8658). This test covers the open step, the first chained step,
  // and the stage-1 -> stage-2 finish from a completed scan.
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(begin_post, begin_work_witness_cbor) = script_sources_pending_redeemer_item_fixture(
    tx,
    empty_frontier,
    1,
    redeemer_item_proof_v1.hash_control_v1(item_control),
    11,
  )
  let Pair(header_post, _) = script_sources_pending_redeemer_item_fixture(
    tx,
    empty_frontier,
    1,
    redeemer_item_proof_v1.hash_control_v1(tail_control),
    12,
  )
  // A stage-1 state after the scan completed: the redeemer leaf is committed
  // and no item proof is pending.
  let Pair(item_post, item_work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    empty_frontier,
    redeemer_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  let Pair(final_post, _) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    2,
    empty_frontier,
    redeemer_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    12,
  )
  let begin_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: begin_post,
  }
  let header_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: begin_work_witness_cbor,
    claimed_successor: header_post,
  }
  let final_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: item_work_witness_cbor,
    claimed_successor: final_post,
  }
  and {
    vm_script_sources_early_stages.verify_script_sources_stage_one_finish_raw_semantics_v1(
      item_post,
      final_transition,
    ),
    vm_one_step.verify_one_step_evidence(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: begin_transition,
        auxiliary: vm_machine_types.TransactionRedeemerItemBeginWitness {
          carriage,
        },
      },
      inline_field_door(),
    ),
    vm_one_step.verify_one_step_evidence(
      begin_post,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: header_transition,
        auxiliary: vm_machine_types.RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: header_witness,
        },
      },
      inline_field_door(),
    ),
    vm_one_step.verify_one_step_evidence(
      item_post,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: final_transition,
        auxiliary: vm_machine_types.NoAuxiliaryWitness,
      },
      inline_field_door(),
    ),
  }
}

test script_sources_stage_one_begin_matches_canonical_transition() {
  let base = empty_v1_transaction()
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"00",
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let redeemer_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.redeemer_leaf_hash(0, redeemer)],
  )
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let carriage = inline_field_carriage([redeemer_cbor])
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(begin_post, begin_work_witness_cbor) = script_sources_pending_redeemer_item_fixture(
    tx,
    empty_frontier,
    1,
    redeemer_item_proof_v1.hash_control_v1(item_control),
    11,
  )
  let Pair(header_post, _) = script_sources_pending_redeemer_item_fixture(
    tx,
    empty_frontier,
    1,
    redeemer_item_proof_v1.hash_control_v1(tail_control),
    12,
  )
  let Pair(item_post, item_work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    empty_frontier,
    redeemer_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  let Pair(final_post, _) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    2,
    empty_frontier,
    redeemer_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    12,
  )
  let begin_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: begin_post,
  }
  let header_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: begin_work_witness_cbor,
    claimed_successor: header_post,
  }
  let final_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: item_work_witness_cbor,
    claimed_successor: final_post,
  }
  let begin_auxiliary = vm_machine_types.TransactionRedeemerItemBeginWitness {
    carriage,
  }
  let header_auxiliary = vm_machine_types.RedeemerItemStepWitness {
    redeemer_control: None,
    control: item_control,
    witness: header_witness,
  }
  and {
    vm_script_sources.verify_script_sources_stage_one_semantics_v1(
      pre,
      begin_transition,
      begin_auxiliary,
      inline_field_door(),
    ),
    vm_script_sources.verify_script_sources_stage_one_semantics_v1(
      begin_post,
      header_transition,
      header_auxiliary,
      inline_field_door(),
    ),
    vm_script_sources.verify_script_sources_stage_one_semantics_v1(
      item_post,
      final_transition,
      vm_machine_types.NoAuxiliaryWitness,
      inline_field_door(),
    ),
    vm_script_sources_early_stages.verify_script_sources_stage_one_begin_raw_semantics(
      pre,
      begin_transition,
      carriage,
      inline_field_door(),
    ),
  }
}

test script_sources_stage_one_accepts_more_than_sixteen_sources() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: list.repeat(script, 17),
    },
  }
  let source_frontier = validation_merkle_v1.build_frontier(
    indexed_inline_source_leaves(script, 0, 17),
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(post, _) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    2,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    vm_script_sources_early_stages.verify_script_sources_stage_one_finish_raw_semantics_v1(
      pre,
      transition,
    ),
    vm_one_step.verify_one_step_evidence(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition,
        auxiliary: vm_machine_types.NoAuxiliaryWitness,
      },
      inline_field_door(),
    ),
  }
}

test script_sources_binds_resolved_input_replay_schedule() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let accumulator = vm_witness.initial_resolution_accumulator()
  let schedule = vm_witness.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    1,
    accumulator,
    2,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  let Pair(post, _) = script_sources_step_fixture(
    tx,
    1,
    accumulator,
    3,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    schedule,
    0,
    empty_frontier,
    12,
  )
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
  )
}

// The TypeScript producer exhaustively derives these terminal controls from
// the checked 434-spend Cardano boundary fixture. This Aiken argument contains
// only the final bounded replay item plus logarithmic frontier peaks.
test maximum_spend_input_schedule_terminal_matches_typescript() {
  verify_maximum_input_schedule_terminal_vector(
    maximum_spend_input_schedule_terminal_vector(),
  )
}

// The mixed checked boundary contains one spend and 433 reference inputs.
test maximum_reference_input_schedule_terminal_matches_typescript() {
  verify_maximum_input_schedule_terminal_vector(
    maximum_reference_input_schedule_terminal_vector(),
  )
}

test script_sources_replay_appends_reference_scripts() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: Some(script),
  }
  let value = replay_output_descriptor_fixture(
    0,
    encode_midgard_tx_output(output),
    #"70" |> bytearray.concat(h_28_a),
    Some(script),
  )
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, reference_inputs: [input] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.source_leaf_hash(1, key, script)],
  )
  let resolved_item_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.resolved_context_item_leaf_hash(1, 0, key, value)],
  )
  let accumulator = vm_witness.initial_resolution_accumulator()
  let next_accumulator = vm_witness.resolved_input_accumulator_successor(
    accumulator,
    1,
    key,
    value,
  )
  let schedule = vm_witness.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    1,
    next_accumulator,
    3,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    schedule,
    0,
    empty_frontier,
    12,
  )
  let Pair(post, _) = script_sources_step_fixture_with_resolved_items(
    tx,
    1,
    next_accumulator,
    3,
    source_frontier,
    empty_frontier,
    1,
    resolved_item_frontier,
    next_accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    13,
  )
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.ResolvedInputReplayWitness {
        source_kind: 1,
        key,
        next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
        value,
      },
    ),
    inline_field_door(),
  )
}

test script_sources_replay_appends_script_credential_spend_purposes() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let value = replay_output_descriptor_fixture(
    0,
    encode_midgard_tx_output(output),
    #"70" |> bytearray.concat(h_28_a),
    None,
  )
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, key)],
  )
  let resolved_item_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.resolved_context_item_leaf_hash(0, 0, key, value)],
  )
  let accumulator = vm_witness.initial_resolution_accumulator()
  let next_accumulator = vm_witness.resolved_input_accumulator_successor(
    accumulator,
    0,
    key,
    value,
  )
  let schedule = vm_witness.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    1,
    next_accumulator,
    3,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    schedule,
    0,
    empty_frontier,
    12,
  )
  let Pair(post, _) = script_sources_step_fixture_with_resolved_items(
    tx,
    1,
    next_accumulator,
    3,
    empty_frontier,
    empty_frontier,
    1,
    resolved_item_frontier,
    next_accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    1,
    purpose_frontier,
    13,
  )
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.ResolvedInputReplayWitness {
        source_kind: 0,
        key,
        next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
        value,
      },
    ),
    inline_field_door(),
  )
}

test script_sources_commits_an_authenticated_output_item() {
  canonical_output_commitment_step(0)
}

/// Supersedes `script_sources_rejects_a_substituted_complete_output_item`:
/// stage 4 does not reveal item bytes, so substitution attacks what the fold
/// accumulates.
///
/// The forgery is in the **claimed successor's** output leaf rather than in an
/// `ItemProofV1.item_commitment`, because the commitment the fold appends is
/// derived from door-authenticated bytes and is not a prover field any more
/// (see `canonical_output_commitment_step_with_successor`). The claim refuted is
/// unchanged and is `C21-STAGE4` Option A's: the leaf appended at stage 4 must
/// be `output_item_leaf_hash` over the *authenticated* item's `bounded_item_v1`
/// commitment, and nothing else.
test script_sources_rejects_a_forged_output_item_commitment() {
  !canonical_output_commitment_step_with_successor(
    0,
    script_proof_v1.output_item_leaf_hash(
      0,
      #"00000000000000000000000000000000000000000000000000000000000000ff",
    ),
    1,
  )
}

/// Was `script_sources_rejects_a_forged_output_item_length`, renamed for the
/// quantity it now forges (#592).
///
/// The old row forged `ItemProofV1.item_length`, whose only effect was on the
/// `bounded_collection_v1.verify_item` membership leaf — the conjunct the flat
/// reversion retired. The item's length is derived from authenticated bytes
/// now, so the surviving count claim a prover controls is the fold's
/// `output_total_count`, forged to 2 against a one-output field with the appended
/// leaf held honest.
///
/// **Which conjunct refutes it, measured rather than assumed.** The forgery lands
/// in the *claimed successor*, and this is the fold's opening step, so the pre
/// state's `output_total_count` is still the `0` sentinel and
/// `active_total_count` is therefore the field's own authenticated count — 1.
/// `item_count == active_total_count` is `1 == 1` and passes; neutralizing it to
/// `True` leaves this row green. The falsifier is
/// `script_sources_control_successor_is_exact`, which requires the successor to
/// carry `output_total_count: active_total_count` and so refuses the claimed 2.
/// That is the right refutation for a forged successor, and it is the whole of
/// this row's claim.
///
/// The named count binding gets its own row below, because "the step may not
/// commit a count the field denies" is a separate claim from "the successor must
/// be exact", and neutralizing either one leaves the other's row red.
test script_sources_rejects_a_forged_output_total_count() {
  !canonical_output_commitment_step_with_successor(
    0,
    script_proof_v1.output_leaf_hash(
      0,
      canonical_output_commitment_output_cbor(),
    ),
    2,
  )
}

/// `item_count == active_total_count` (validation-machine-v1.ak:9213) as the
/// **sole** falsifier — the conjunct the row above names and does not exercise.
///
/// The fold is entered with a carried `output_total_count` of 2 already in the
/// pre state's control, so `active_total_count` is that carried 2 rather than the
/// field's count, and the honest successor for such a control is one that carries
/// 2 forward. Successor exactness therefore **passes**: the successor built here
/// is exactly `{..control, output_count + 1, output_peaks appended honestly,
/// output_total_count: 2}`. What fails is the field itself disagreeing — field 2
/// authenticates one item, so a step claiming a two-output fold is refused. This
/// is the attack the count binding exists for: a carried count that the disputed
/// transaction's own field-2 preimage does not support, which is how a fold's
/// completion test would be reached without visiting every output.
test script_sources_rejects_an_output_total_count_the_field_denies() {
  let output = canonical_output_commitment_output()
  let output_cbor = canonical_output_commitment_output_cbor()
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    4,
    empty_frontier,
    empty_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    2,
    vm_witness.empty_receive_purpose_scan_control(),
    vm_witness.empty_observer_purpose_scan_control(),
    14,
  )
  let Pair(successor, _) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    4,
    empty_frontier,
    empty_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    output_frontier,
    2,
    vm_witness.empty_receive_purpose_scan_control(),
    vm_witness.empty_observer_purpose_scan_control(),
    15,
  )
  !vm_script_sources.verify_script_sources_stage_four_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: successor,
    },
    vm_machine_types.TransactionRedeemerItemBeginWitness {
      carriage: inline_field_carriage([output_cbor]),
    },
    inline_field_door(),
  )
}

test script_sources_finishes_authenticated_output_commitment() {
  canonical_output_commitment_step(1)
}

test script_sources_begins_an_authenticated_output_proof() {
  authenticated_output_proof_step(0)
}

test script_sources_advances_one_bounded_output_proof_step() {
  authenticated_output_proof_step(1)
}

test script_sources_output_proof_cannot_use_non_output_resolver() {
  authenticated_output_proof_step(2)
}

test script_sources_collects_protected_receive_source() {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let receive_source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let output_descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, material.descriptor_cbor)],
  )
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_active_output_proof_fixture(
    tx,
    0,
    output_frontier,
    vm_witness.empty_receive_purpose_scan_control(),
    material.facts_terminal,
    22,
  )
  let Pair(post, _) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    5,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    1,
    output_frontier,
    1,
    vm_machine_types.ReceivePurposeScanControlV1 {
      source_count: receive_source_frontier.count,
      source_peaks: receive_source_frontier.peaks,
      receive_count: 0,
      previous_hash: #"",
      candidate_hash: #"",
      descriptor_peaks: output_descriptor_frontier.peaks,
    },
    vm_witness.empty_observer_purpose_scan_control(),
    23,
  )
  vm_script_sources.verify_script_sources_output_proof_finalize_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    material.descriptor_cbor,
    vm_machine_types.NoSignerSetProof,
  )
}

test script_sources_selects_canonical_protected_receive_purpose() {
  protected_receive_purpose_step(0)
}

test script_sources_appends_protected_receive_purpose() {
  protected_receive_purpose_step(1)
}

test script_sources_rescans_receive_sources_for_distinctness() {
  protected_receive_purpose_step(2)
}

test script_sources_finishes_protected_receive_purpose_scan() {
  protected_receive_purpose_step(3)
}

test script_sources_rejects_foreign_raw_network_nibble_2() {
  script_sources_rejects_wrong_output_network(2, False)
}

test script_sources_rejects_protected_foreign_raw_network_nibble_15() {
  // The Midgard protection bit makes raw 15 logical protected-network 7.
  script_sources_rejects_wrong_output_network(7, True)
}

test script_sources_rejects_an_unsigned_protected_pubkey_output() {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let Pair(pre, work_witness_cbor) = script_sources_active_output_proof_fixture(
    tx,
    0,
    output_frontier,
    vm_witness.empty_receive_purpose_scan_control(),
    material.facts_terminal,
    15,
  )
  let rejection_code = #"455f4d495353494e475f52455155495245445f5749544e455353"
  // An exact rejection also clears the ledger delta to the empty frontier
  // commitment; `exact_rejection_post` is the canonical successor shape.
  let terminal = exact_rejection_post(pre, rejection_code)
  vm_script_sources.verify_script_sources_output_proof_finalize_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: terminal,
    },
    material.descriptor_cbor,
    vm_machine_types.EmptySignerSetProof { peaks: [] },
  )
}

/// The mint fold's three steps, under §8's field-access door (#592).
///
/// **This vector's begin step was a tripwire and is now a positive assertion.**
/// Under the counted scheme `script_sources_begin_mint_policy` authenticated its
/// policy item with `bounded_collection_v1.verify_item(mint_hash, ...)` while
/// `mint_hash` had already become §4's *flat* `blake2b_256` over field 5's §5.1
/// enveloped preimage — a hash with no leaves, which
/// `bounded-collection-v1.ak`'s own docstring says `verify_item` cannot be
/// satisfied against. No fixture could make the begin step true, so this test
/// asserted `!begin_step` beside the reason, with a note that the row "turns red
/// the moment stage six is rebound onto the flat door".
///
/// #592 is that rebind, so the note is honoured here: the begin step now carries
/// a §8.1 tier-1 carriage over the field-5 preimage, the door hashes it once
/// against `mint_hash`, and the policy item's length and `bounded_item_v1`
/// commitment are derived from those authenticated bytes into the fold control.
/// All three steps are asserted **positively**.
///
/// The asset and finish steps are unchanged: they authenticate against
/// `fold.item_commitment`, which the fold control carries and which is a genuine
/// Merkle root over the item's chunks, so their openings were always
/// satisfiable. What moved is only the provenance of that root — derived at the
/// begin step from door-authenticated bytes instead of asserted by an
/// `ItemProofV1`.
test script_sources_streams_mint_and_burn_into_authenticated_frontiers() {
  // §5.6 — the enveloped list of per-policy items, not the retired raw map.
  let mint_cbor = native_tx_field_access_v1.encode_field_preimage(
    [
      #"82581c"
        |> bytearray.concat(h_28_a)
        |> bytearray.concat(#"a1410124"),
    ],
  )
  let policy_item_cbor = #"82581c"
    |> bytearray.concat(h_28_a)
    |> bytearray.concat(#"a1410124")
  let mint = decode_midgard_tx_mint_preimage_cbor(mint_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let mint_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(1, 0, h_28_a, h_28_a)],
  )
  let mint_asset_frontier = validation_merkle_v1.build_frontier(
    [vm_shared.mint_asset_leaf_hash(h_28_a, #"01", -5)],
  )
  let carriage = inline_field_carriage([policy_item_cbor])
  let chunk_proof = singleton_field_item_chunk_proof(5, policy_item_cbor)
  let initial_fold = vm_witness.empty_mint_fold_control()
  let active_fold = vm_machine_types.MintFoldControlV1 {
    policy_count: 1,
    policy_cursor: 0,
    previous_policy: #"",
    active_policy: h_28_a,
    item_length: bytearray.length(policy_item_cbor),
    item_commitment: bounded_item_v1.from_bytes(5, 0, policy_item_cbor),
    item_cursor: 32,
    assets_remaining: 1,
    policy_asset_cursor: 0,
    previous_asset: #"",
    asset_count: 0,
    asset_peaks: empty_frontier.peaks,
  }
  let completed_fold = vm_machine_types.MintFoldControlV1 {
    policy_count: 1,
    policy_cursor: 1,
    previous_policy: h_28_a,
    active_policy: #"",
    item_length: 0,
    item_commitment: #"",
    item_cursor: 0,
    assets_remaining: 0,
    policy_asset_cursor: 0,
    previous_asset: #"",
    asset_count: 1,
    asset_peaks: mint_asset_frontier.peaks,
  }
  let Pair(begin_pre, begin_work_witness_cbor) = script_sources_mint_fold_fixture(
    tx,
    empty_frontier,
    initial_fold,
    6,
    17,
  )
  let Pair(begin_post, _) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    active_fold,
    6,
    18,
  )
  let begin_step = vm_script_sources.verify_script_sources_stage_six_semantics_v1(
    begin_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: begin_work_witness_cbor,
      claimed_successor: begin_post,
    },
    vm_machine_types.TransactionFieldChunkWitness {
      field_index: 5,
      item_index: 0,
      carriage,
    },
    inline_field_door(),
  )
  let Pair(asset_pre, asset_work_witness_cbor) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    active_fold,
    6,
    18,
  )
  let Pair(asset_post, _) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    completed_fold,
    6,
    19,
  )
  let asset_step = vm_script_sources.verify_script_sources_stage_six_semantics_v1(
    asset_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: asset_work_witness_cbor,
      claimed_successor: asset_post,
    },
    vm_machine_types.MintFoldAssetWitness {
      chunk_proof,
      next_chunk_proof: None,
    },
    inline_field_door(),
  )
  let Pair(finish_pre, finish_work_witness_cbor) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    completed_fold,
    6,
    19,
  )
  let Pair(finish_post, _) = script_sources_mint_fold_fixture(
    tx,
    mint_purpose_frontier,
    completed_fold,
    7,
    20,
  )
  let finish_step = vm_script_sources.verify_script_sources_stage_six_semantics_v1(
    finish_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: finish_work_witness_cbor,
      claimed_successor: finish_post,
    },
    vm_machine_types.NoAuxiliaryWitness,
    inline_field_door(),
  )
  and {
    // The rebound call site, asserted positively — see this test's note for what
    // it used to assert and why.
    begin_step,
    // …and the tripwire's own reason, kept as a live statement rather than a
    // claim about history: field 5's commitment is the flat `blake2b_256` over
    // its §5.1 preimage, and the door reaches the policy item by authenticating
    // exactly those bytes. A carriage over anything else does not open.
    native_tx_field_access_v1.field_commitment(mint_cbor) == midgard_transaction_to_compact(
      tx,
    ).body.mint_hash,
    asset_step,
    finish_step,
    vm_middle_semantics.stage_six_begin(
      begin_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: begin_work_witness_cbor,
        claimed_successor: begin_post,
      },
      inline_field_door(),
      5,
      0,
      carriage,
    ),
    vm_middle_semantics.stage_six_asset(
      asset_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: asset_work_witness_cbor,
        claimed_successor: asset_post,
      },
      chunk_proof,
      None,
    ),
    vm_middle_semantics.stage_six_finish(
      finish_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: finish_work_witness_cbor,
        claimed_successor: finish_post,
      },
    ),
  }
}

test script_sources_mint_fold_authenticates_a_chunk_boundary() {
  crossing_mint_fold_step(False)
}

test script_sources_mint_fold_fails_closed_when_boundary_data_is_withheld() fail {
  crossing_mint_fold_step(True)
}

test script_sources_appends_canonical_observer_purposes() {
  canonical_observer_purpose_step(0)
}

test script_sources_finishes_canonical_observer_purposes() {
  canonical_observer_purpose_step(1)
}

test script_sources_stage_seven_finish_binds_canonical_successor_encoding() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    18,
  )
  let Pair(post, post_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    vm_witness.empty_script_discovery_control(),
    19,
  )
  // Same successor state, but committed in the `serialiseData` encoding.
  let reencoded_post = script_sources_state_for_work_witness(
    tx,
    serialise_data_reencoding(post_witness_cbor),
    19,
  )
  // Semantically wrong successor: stage 9 instead of the mandated stage 8.
  let Pair(mutated_post, _) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    vm_witness.empty_script_discovery_control(),
    19,
  )
  let canonical_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    // The canonical encoding really is distinct from the `serialiseData` form.
    bytearray.take(post_witness_cbor, 1) == #"98",
    bytearray.take(serialise_data_reencoding(post_witness_cbor), 1) == #"9f",
    // Positive: both proof paths accept the canonical successor.
    vm_script_sources_late_stages.verify_script_sources_stage_seven_finish_semantics_v1(
      pre,
      canonical_transition,
    ),
    vm_one_step.verify_one_step_evidence(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: canonical_transition,
        auxiliary: vm_machine_types.NoAuxiliaryWitness,
      },
      inline_field_door(),
    ),
    // Control: the `serialiseData` successor is still rejected.
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_finish_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: reencoded_post,
      },
    ),
    !vm_one_step.verify_one_step_evidence(
      pre,
      one_step_evidence(
        work_witness_cbor,
        reencoded_post,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      inline_field_door(),
    ),
    // Control: a mutated successor stage is still rejected.
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_finish_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: mutated_post,
      },
    ),
  }
}

test script_sources_stage_seven_observer_binds_canonical_successor_encoding() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: [h_28_a] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let observer_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a)],
  )
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    18,
  )
  let Pair(after_observer, after_observer_witness_cbor) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    observer_purpose_frontier,
    0,
    empty_frontier,
    0,
    vm_witness.empty_receive_purpose_scan_control(),
    vm_machine_types.ObserverPurposeScanControlV1 {
      total_count: 1,
      seen: 1,
      previous_hash: h_28_a,
    },
    19,
  )
  // Same successor state, but committed in the `serialiseData` encoding.
  let reencoded_after_observer = script_sources_state_for_work_witness(
    tx,
    serialise_data_reencoding(after_observer_witness_cbor),
    19,
  )
  // Semantically wrong successor: the observer purpose was never appended.
  let Pair(mutated_after_observer, _) = script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    empty_frontier,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    empty_frontier,
    0,
    vm_witness.empty_receive_purpose_scan_control(),
    vm_machine_types.ObserverPurposeScanControlV1 {
      total_count: 1,
      seen: 1,
      previous_hash: h_28_a,
    },
    19,
  )
  let carriage = inline_field_carriage([h_28_a])
  let honest_witness = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: after_observer,
  }
  let forged_witness = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_after_observer,
  }
  and {
    vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_item_facts_v1(
      pre,
      honest_witness,
      inline_field_door(),
      3,
      0,
      carriage,
      h_28_a,
      1,
    ),
    vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_bound_semantics_v1(
      pre,
      honest_witness,
      h_28_a,
      1,
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_item_facts_v1(
      pre,
      honest_witness,
      inline_field_door(),
      3,
      0,
      carriage,
      h_28_b,
      1,
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_item_facts_v1(
      pre,
      honest_witness,
      inline_field_door(),
      3,
      0,
      carriage,
      h_28_a,
      2,
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_bound_semantics_v1(
      pre,
      forged_witness,
      h_28_a,
      1,
    ),
    vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_observer,
      },
      inline_field_door(),
      3,
      0,
      carriage,
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: reencoded_after_observer,
      },
      inline_field_door(),
      3,
      0,
      carriage,
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: mutated_after_observer,
      },
      inline_field_door(),
      3,
      0,
      carriage,
    ),
  }
}

test script_sources_stage_seven_receive_binds_canonical_successor_encoding() {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: ScriptCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 0, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let material = authenticated_output_material(output_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, outputs: [output] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let output_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_leaf_hash(0, output_cbor)],
  )
  let receive_source_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let output_descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, material.descriptor_cbor)],
  )
  let empty_receive_scan = vm_machine_types.ReceivePurposeScanControlV1 {
    source_count: receive_source_frontier.count,
    source_peaks: receive_source_frontier.peaks,
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: #"",
    descriptor_peaks: output_descriptor_frontier.peaks,
  }
  let selected_receive_scan = vm_machine_types.ReceivePurposeScanControlV1 {
    ..empty_receive_scan,
    candidate_hash: h_28_a,
  }
  let Pair(pre, work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    empty_receive_scan,
    15,
  )
  let Pair(after_candidate, after_candidate_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    1,
    output_frontier,
    selected_receive_scan,
    16,
  )
  // Same successor state, but committed in the `serialiseData` encoding.
  let reencoded_after_candidate = script_sources_state_for_work_witness(
    tx,
    serialise_data_reencoding(after_candidate_witness_cbor),
    16,
  )
  // Semantically wrong successor: the output cursor did not advance.
  let Pair(mutated_after_candidate, _) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    selected_receive_scan,
    16,
  )
  and {
    vm_script_sources_late_stages.verify_script_sources_stage_seven_receive_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_candidate,
      },
      3,
      0,
      h_28_a,
      h_28_a,
      [],
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_receive_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: reencoded_after_candidate,
      },
      3,
      0,
      h_28_a,
      h_28_a,
      [],
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_seven_receive_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: mutated_after_candidate,
      },
      3,
      0,
      h_28_a,
      h_28_a,
      [],
    ),
  }
}

test script_sources_stage_one_finish_raw_binds_canonical_successor_encoding() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let source_frontier = validation_merkle_v1.build_frontier(
    indexed_inline_source_leaves(script, 0, 1),
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    1,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    10,
  )
  let Pair(post, post_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    2,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  // Same successor state, but committed in the `serialiseData` encoding.
  let reencoded_post = script_sources_state_for_work_witness(
    tx,
    serialise_data_reencoding(post_witness_cbor),
    11,
  )
  // Semantically wrong successor: stage 3 instead of the mandated stage 2.
  let Pair(mutated_post, _) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    3,
    source_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    11,
  )
  and {
    // The witness carries a bytestring past the 64-byte `serialiseData`
    // chunking threshold, which is what made the rebuilt prefix unmatchable.
    bytearray.length(submitted_source(tx).compact_cbor) > 64,
    vm_script_sources_early_stages.verify_script_sources_stage_one_finish_raw_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
    ),
    !vm_script_sources_early_stages.verify_script_sources_stage_one_finish_raw_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: reencoded_post,
      },
    ),
    !vm_script_sources_early_stages.verify_script_sources_stage_one_finish_raw_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: mutated_post,
      },
    ),
  }
}

test script_sources_prepares_more_than_sixteen_purposes_for_discovery() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    indexed_spend_purpose_leaves(0, 17),
  )
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    7,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    purpose_frontier,
    0,
    empty_frontier,
    18,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    vm_witness.empty_script_discovery_control(),
    19,
  )
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
  )
}

test script_sources_discovers_more_than_sixteen_purposes() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let first_peak_leaves = indexed_spend_purpose_leaves(0, 16)
  let purpose_frontier = validation_merkle_v1.build_frontier(
    indexed_spend_purpose_leaves(0, 17),
  )
  let empty_discovery = vm_witness.empty_script_discovery_control()
  let selected_purpose = vm_machine_types.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: h_28_a,
    current_subject: cbor.serialise(0),
  }
  let Pair(stage_eight, stage_eight_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    empty_discovery,
    19,
  )
  let Pair(stage_nine, _) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    selected_purpose,
    20,
  )
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_eight_cbor,
    claimed_successor: stage_nine,
  }
  let siblings = first_leaf_power_of_two_siblings(first_peak_leaves)
  and {
    vm_script_sources_late_stages.verify_script_sources_stage_eight_purpose_semantics_v1(
      stage_eight,
      transition,
      0,
      0,
      h_28_a,
      cbor.serialise(0),
      siblings,
    ),
    vm_one_step.verify_one_step_evidence(
      stage_eight,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition,
        auxiliary: vm_machine_types.ScriptPurposeScanWitness {
          purpose_kind: 0,
          purpose_index: 0,
          script_hash: h_28_a,
          subject: cbor.serialise(0),
          siblings,
        },
      },
      inline_field_door(),
    ),
  }
}

test script_sources_stage_eight_finishes_empty_discovery_exactly() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let empty_discovery = vm_witness.empty_script_discovery_control()
  let Pair(stage_eight, stage_eight_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    19,
  )
  let Pair(stage_eleven, _) = script_discovery_step_fixture(
    tx,
    11,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    20,
  )
  vm_script_sources_late_stages.verify_script_sources_stage_eight_finish_semantics_v1(
    stage_eight,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: stage_eight_cbor,
      claimed_successor: stage_eleven,
    },
  )
}

test script_sources_discovers_an_exact_non_native_execution() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
      redeemer_tx_wits: [redeemer],
    },
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
    0,
    redeemer_item_commitment,
  )
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    #"0102",
  )
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let empty_discovery = vm_witness.empty_script_discovery_control()
  let selected_purpose = vm_machine_types.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: #"0102",
  }
  let selected_source = vm_machine_types.ScriptDiscoveryControlV1 {
    ..selected_purpose,
    source_cursor: 1,
    matched_source_index: 0,
    matched_language_tag: 3,
    matched_source_leaf: source_leaf,
    used_inline_bitmap: 1,
  }
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let tail_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenTail,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  let selected_item = vm_machine_types.ScriptDiscoveryControlV1 {
    ..selected_source,
    redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      item_control,
    ),
  }
  let selected_tail = vm_machine_types.ScriptDiscoveryControlV1 {
    ..selected_source,
    redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      tail_control,
    ),
  }
  let execution_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.execution_leaf_hash(
        3,
        purpose_leaf,
        source_leaf,
        redeemer_leaf,
      ),
    ],
  )
  let completed = vm_machine_types.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    purpose_cursor: 1,
    used_inline_bitmap: 1,
    used_redeemer_bitmap: 1,
    execution_count: execution_frontier.count,
    execution_peaks: execution_frontier.peaks,
  }
  let Pair(stage_eight, stage_eight_cbor) = script_discovery_step_fixture(
    tx,
    8,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    empty_discovery,
    20,
  )
  let Pair(stage_nine, stage_nine_cbor) = script_discovery_step_fixture(
    tx,
    9,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_purpose,
    21,
  )
  let Pair(stage_ten, stage_ten_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_source,
    22,
  )
  let Pair(stage_ten_item, stage_ten_item_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_item,
    23,
  )
  let Pair(stage_ten_tail, stage_ten_tail_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_tail,
    24,
  )
  let Pair(stage_eight_done, _) = script_discovery_step_fixture(
    tx,
    8,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    completed,
    25,
  )
  let stage_ten_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_cbor,
    claimed_successor: stage_ten_item,
  }
  and {
    vm_script_sources_late_stages.verify_script_sources_stage_eight_purpose_semantics_v1(
      stage_eight,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: stage_eight_cbor,
        claimed_successor: stage_nine,
      },
      0,
      0,
      script_hash,
      #"0102",
      [],
    ),
    vm_script_sources_late_stages.verify_script_sources_stage_nine_effectful_match_semantics_v1(
      stage_nine,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: stage_nine_cbor,
        claimed_successor: stage_ten,
      },
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    vm_one_step.verify_one_step_evidence(
      stage_ten,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: stage_ten_transition,
        auxiliary: vm_machine_types.RedeemerScanBeginWitness {
          item_index: 0,
          item_count: 1,
          total_length: bytearray.length(redeemer_cbor),
          item_commitment: redeemer_item_commitment,
          siblings: [],
        },
      },
      inline_field_door(),
    ),
    vm_one_step.verify_one_step_evidence(
      stage_ten_item,
      one_step_evidence(
        stage_ten_item_cbor,
        stage_ten_tail,
        vm_machine_types.RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: header_witness,
        },
      ),
      inline_field_door(),
    ),
    vm_one_step.verify_one_step_evidence(
      stage_ten_tail,
      one_step_evidence(
        stage_ten_tail_cbor,
        stage_eight_done,
        vm_machine_types.RedeemerItemStepWitness {
          redeemer_control: None,
          control: tail_control,
          witness: tail_witness,
        },
      ),
      inline_field_door(),
    ),
  }
}

test script_sources_stage_ten_redeemer_family_guards() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
      redeemer_tx_wits: [redeemer],
    },
  }
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
    0,
    redeemer_item_commitment,
  )
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    #"0102",
  )
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let empty_discovery = vm_witness.empty_script_discovery_control()
  let selected_purpose = vm_machine_types.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: #"0102",
  }
  let selected_source = vm_machine_types.ScriptDiscoveryControlV1 {
    ..selected_purpose,
    source_cursor: 1,
    matched_source_index: 0,
    matched_language_tag: 3,
    matched_source_leaf: source_leaf,
    used_inline_bitmap: 1,
  }
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let tail_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenTail,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  let selected_item = vm_machine_types.ScriptDiscoveryControlV1 {
    ..selected_source,
    redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      item_control,
    ),
  }
  let selected_tail = vm_machine_types.ScriptDiscoveryControlV1 {
    ..selected_source,
    redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
      tail_control,
    ),
  }
  let execution_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.execution_leaf_hash(
        3,
        purpose_leaf,
        source_leaf,
        redeemer_leaf,
      ),
    ],
  )
  let completed = vm_machine_types.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    purpose_cursor: 1,
    used_inline_bitmap: 1,
    used_redeemer_bitmap: 1,
    execution_count: execution_frontier.count,
    execution_peaks: execution_frontier.peaks,
  }
  let Pair(stage_ten, stage_ten_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_source,
    22,
  )
  let Pair(stage_ten_item, stage_ten_item_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_item,
    23,
  )
  let Pair(stage_ten_tail, stage_ten_tail_cbor) = script_discovery_step_fixture(
    tx,
    10,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    selected_tail,
    24,
  )
  let Pair(stage_eight_done, _) = script_discovery_step_fixture(
    tx,
    8,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    completed,
    25,
  )
  let stage_ten_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_cbor,
    claimed_successor: stage_ten_item,
  }
  let stage_ten_item_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_item_cbor,
    claimed_successor: stage_ten_tail,
  }
  let stage_ten_tail_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_ten_tail_cbor,
    claimed_successor: stage_eight_done,
  }
  let scan_begin_auxiliary = vm_machine_types.RedeemerScanBeginWitness {
    item_index: 0,
    item_count: 1,
    total_length: bytearray.length(redeemer_cbor),
    item_commitment: redeemer_item_commitment,
    siblings: [],
  }
  let header_auxiliary = vm_machine_types.RedeemerItemStepWitness {
    redeemer_control: None,
    control: item_control,
    witness: header_witness,
  }
  let tail_auxiliary = vm_machine_types.RedeemerItemStepWitness {
    redeemer_control: None,
    control: tail_control,
    witness: tail_witness,
  }
  and {
    vm_script_sources.verify_script_sources_stage_ten_semantics_v1(
      stage_ten,
      stage_ten_transition,
      scan_begin_auxiliary,
    ),
    verify_descriptor_ten_mismatch_parity(
      stage_ten,
      stage_ten_transition,
      scan_begin_auxiliary,
    ),
    !verify_descriptor_ten_match_parity(
      stage_ten,
      stage_ten_transition,
      scan_begin_auxiliary,
    ),
    vm_script_sources.verify_script_sources_stage_ten_semantics_v1(
      stage_ten_item,
      stage_ten_item_transition,
      header_auxiliary,
    ),
    verify_descriptor_ten_mismatch_parity(
      stage_ten_item,
      stage_ten_item_transition,
      header_auxiliary,
    ),
    !verify_descriptor_ten_match_parity(
      stage_ten_item,
      stage_ten_item_transition,
      header_auxiliary,
    ),
    vm_script_sources.verify_script_sources_stage_ten_semantics_v1(
      stage_ten_tail,
      stage_ten_tail_transition,
      tail_auxiliary,
    ),
    verify_descriptor_ten_match_parity(
      stage_ten_tail,
      stage_ten_tail_transition,
      tail_auxiliary,
    ),
    !verify_descriptor_ten_mismatch_parity(
      stage_ten_tail,
      stage_ten_tail_transition,
      tail_auxiliary,
    ),
  }
}

test script_sources_stage_ten_proves_mismatch_and_missing_redeemer_exactly() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let singleton_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 1, h_28_a, #"0102")],
  )
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    source_cursor: 1,
    current_purpose_kind: 0,
    current_purpose_index: 1,
    current_script_hash: h_28_a,
    current_subject: #"0102",
    matched_source_index: 0,
    matched_language_tag: 3,
    matched_source_leaf: redeemer_leaf,
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    10,
    singleton_frontier,
    singleton_frontier,
    purpose_frontier,
    discovery,
    30,
  )
  let Pair(after_mismatch, after_mismatch_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    10,
    singleton_frontier,
    singleton_frontier,
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..discovery,
      redeemer_cursor: 1,
    },
    31,
  )
  and {
    pre.program_counter == 30,
    work_witness_cbor != #"",
    after_mismatch.program_counter == 31,
    after_mismatch_work_witness_cbor != #"",
  }
}

test script_sources_stage_nine_accepts_an_exact_mismatch() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, #"0102")],
  )
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: h_28_a,
    current_subject: #"0102",
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    9,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    discovery,
    24,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    empty_v1_transaction(),
    9,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 { ..discovery, source_cursor: 1 },
    25,
  )
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    vm_script_sources_late_stages.verify_script_sources_stage_nine_mismatch_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_nine_effectful_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_nine_missing_semantics_v1(
      pre,
      transition,
    ),
  }
}

test script_sources_stage_nine_accepts_an_exact_effectful_match() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: #"0102",
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    9,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    discovery,
    26,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    empty_v1_transaction(),
    10,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..discovery,
      source_cursor: 1,
      matched_source_index: 0,
      matched_language_tag: 3,
      matched_source_leaf: source_leaf,
      used_inline_bitmap: 1,
    },
    27,
  )
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    vm_script_sources_late_stages.verify_script_sources_stage_nine_effectful_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_nine_mismatch_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_nine_native_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
  }
}

test script_sources_stage_nine_accepts_an_exact_native_match() {
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"820400",
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    #"0102",
  )
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.execution_leaf_hash(0, purpose_leaf, source_leaf, #"")],
  )
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: #"0102",
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    9,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    discovery,
    28,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    empty_v1_transaction(),
    8,
    source_frontier,
    validation_merkle_v1.build_frontier([]),
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..vm_witness.empty_script_discovery_control(),
      purpose_cursor: 1,
      used_inline_bitmap: 1,
      execution_count: execution_frontier.count,
      execution_peaks: execution_frontier.peaks,
    },
    29,
  )
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    vm_script_sources_late_stages.verify_script_sources_stage_nine_native_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_nine_mismatch_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      0,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    !vm_script_sources_late_stages.verify_script_sources_stage_nine_effectful_match_semantics_v1(
      pre,
      transition,
      0,
      0,
      #"00",
      0,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
  }
}

test script_sources_stage_eleven_audits_inline_sources_exactly() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let execution_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    purpose_cursor: 1,
    used_inline_bitmap: 1,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    11,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    30,
  )
  let Pair(after_source, after_source_work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    11,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 { ..discovery, source_cursor: 1 },
    31,
  )
  let Pair(after_finish, after_finish_work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    12,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 { ..discovery, source_cursor: 1 },
    32,
  )
  let Pair(native_scripts, _) = native_scripts_step_fixture(
    empty_v1_transaction(),
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    33,
  )
  and {
    vm_script_sources_late_stages.verify_script_sources_stage_eleven_source_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_source,
      },
      0,
      0,
      #"00",
      3,
      script_hash,
      bytearray.length(script_cbor),
      script_item_commitment,
      [],
    ),
    vm_script_sources_late_stages.verify_script_sources_stage_eleven_finish_semantics_v1(
      after_source,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: after_source_work_witness_cbor,
        claimed_successor: after_finish,
      },
    ),
    vm_script_sources_late_stages.verify_script_sources_stage_twelve_finish_semantics_v1(
      after_finish,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: after_finish_work_witness_cbor,
        claimed_successor: native_scripts,
      },
    ),
  }
}

test script_sources_stage_eleven_rejects_an_unused_inline_source_exactly() {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    empty_v1_transaction(),
    11,
    source_frontier,
    empty_frontier,
    empty_frontier,
    vm_witness.empty_script_discovery_control(),
    30,
  )
  vm_script_sources_late_stages.verify_script_sources_stage_eleven_source_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(
        pre,
        #"455f494e56414c49445f4649454c445f54595045",
      ),
    },
    0,
    0,
    #"00",
    3,
    script_hash,
    bytearray.length(script_cbor),
    script_item_commitment,
    [],
  )
}

test script_sources_stage_twelve_audits_redeemers_exactly() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, #"0102")
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.execution_leaf_hash(
        3,
        purpose_leaf,
        redeemer_leaf,
        redeemer_leaf,
      ),
    ],
  )
  let used_discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    purpose_cursor: 1,
    used_redeemer_bitmap: 1,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(used_pre, used_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    empty_frontier,
    redeemer_frontier,
    purpose_frontier,
    used_discovery,
    40,
  )
  let Pair(used_post, _) = script_discovery_step_fixture(
    tx,
    12,
    empty_frontier,
    redeemer_frontier,
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..used_discovery,
      redeemer_cursor: 1,
    },
    41,
  )
  let Pair(unused_pre, unused_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    empty_frontier,
    redeemer_frontier,
    empty_frontier,
    vm_witness.empty_script_discovery_control(),
    40,
  )
  and {
    used_pre.program_counter == 40,
    used_work_witness_cbor != #"",
    used_post.program_counter == 41,
    unused_pre.program_counter == 40,
    unused_work_witness_cbor != #"",
  }
}

test script_sources_stage_twelve_redeemer_family_guard() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"d87980",
    execution_units: MidgardExecutionUnits { memory: 3, steps: 4 },
  }
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
      redeemer_tx_wits: [redeemer],
    },
  }
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let redeemer_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(8, 0, redeemer_cbor)
  let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
    0,
    redeemer_item_commitment,
  )
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    purpose_cursor: 1,
    source_cursor: 1,
    used_inline_bitmap: 1,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
  }
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(redeemer_cbor),
    redeemer_item_commitment,
    -1,
    -1,
  )
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(8, redeemer_cbor)
  let header_witness = redeemer_item_proof_v1.RedeemerItemProofWitnessV1 {
    action: redeemer_item_proof_v1.RedeemerItemOpenHeader,
    chunk_proof: Some(redeemer_chunk_proof),
    next_chunk_proof: None,
  }
  expect
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
        control: tail_control,
      })
    = redeemer_item_proof_v1.step_v1(item_control, header_witness)
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    empty_discovery,
    32,
  )
  let Pair(begin_post, begin_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..empty_discovery,
      redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
        item_control,
      ),
    },
    33,
  )
  let Pair(item_post, _) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..empty_discovery,
      redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
        tail_control,
      ),
    },
    34,
  )
  let begin_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: begin_post,
  }
  let item_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: begin_work_witness_cbor,
    claimed_successor: item_post,
  }
  let Pair(finish_pre, Pair(finish_work_witness_cbor, Pair(finish_post, _))) = stage_twelve_finish_fixture(
    #"",
  )
  let finish_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: finish_work_witness_cbor,
    claimed_successor: finish_post,
  }
  and {
    verify_descriptor_twelve_redeemer_parity(
      pre,
      begin_transition,
      vm_machine_types.RedeemerScanBeginWitness {
        item_index: 0,
        item_count: 1,
        total_length: bytearray.length(redeemer_cbor),
        item_commitment: redeemer_item_commitment,
        siblings: [],
      },
    ),
    verify_descriptor_twelve_redeemer_parity(
      begin_post,
      item_transition,
      vm_machine_types.RedeemerItemStepWitness {
        redeemer_control: None,
        control: item_control,
        witness: header_witness,
      },
    ),
    !verify_descriptor_twelve_redeemer_parity(
      finish_pre,
      finish_transition,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  }
}

test script_sources_proves_a_missing_script_source_exactly() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let script_hash = h_28_a
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, script_hash, script_hash)],
  )
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    current_purpose_kind: 3,
    current_purpose_index: 0,
    current_script_hash: script_hash,
    current_subject: script_hash,
  }
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    24,
  )
  let rejection_code = #"455f4d495353494e475f52455155495245445f5749544e455353"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 25,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      25,
      vm_shared.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  vm_script_sources_late_stages.verify_script_sources_stage_nine_missing_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: terminal,
    },
  )
}
