use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_constant_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_witness_set_compact, encode_native_tx_body_compact,
  native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_redeemer_witness, encode_midgard_tx_input,
  encode_midgard_tx_output, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_byte_list_preimage_cbor,
  decode_midgard_tx_mint_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardExecutionUnits, MidgardRedeemerWitness,
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, MidgardTxOutput, MidgardValue, MidgardVersionedScript,
  NativeCardanoScript, PlutusV3Script, PubKeyCredential, RewardRedeemer,
  SpendRedeemer, native_tx_version_v1,
}
use midgard/native_tx_field_access_v1
use midgard/redeemer_item_proof_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_machine/cek as vm_cek
use midgard/validation_machine/ledger_delta as vm_ledger_delta
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/native_scripts as vm_native_scripts
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/phase_a_native_scripts as vm_phase_a_native_scripts
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_28_b, h_a, h_b, h_c, maximum_observer_terminal_compact_cbor,
  maximum_observer_terminal_field_preimage_cbor,
  maximum_observer_terminal_field_preimage_lengths_cbor,
  maximum_observer_terminal_transaction_commitment,
  maximum_observer_terminal_transaction_id,
  maximum_observer_terminal_witness_set_compact_cbor, validation_context_cbor,
  zero,
}
use midgard/validation_machine_tests/fixtures_cek.{
  cek_active_core_step_fixture, exact_output_descriptor_fixture,
  maximum_mint_field_terminal_fixture_v1,
  maximum_observer_field_terminal_fixture_v1,
  maximum_output_field_terminal_fixture_v1,
  maximum_program_envelope_selection_is_exact,
  maximum_reference_input_field_terminal_fixture_v1,
  maximum_required_signer_field_terminal_fixture_v1,
  maximum_spend_input_field_terminal_fixture_v1,
  verify_maximum_field_terminal_fixture_v1,
  verify_maximum_mint_field_terminal_fixture_v1,
}
use midgard/validation_machine_tests/fixtures_parity.{
  verify_bounded_context_parity,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  cek_context_native_control_fixture, cek_state_fixture,
  empty_cek_context_control_fixture, empty_mpf_proof_descriptor,
  empty_native_control_fixture, empty_v1_transaction, exact_rejection_post,
  forced_pending_state, inline_field_carriage, inline_field_door,
  maximum_mint_last_asset_siblings, maximum_mint_preimage_cbor,
  native_scripts_step_fixture, next_phase_a_native_state,
  non_empty_claimed_delta_root, observer_field_carriage, observer_hash_at,
  observer_hashes_from, one_step_evidence, singleton_field_item_chunk_proof,
  submitted_commitment, submitted_source, v1_program_envelope,
  verify_cek_context_source_transition_fixture,
  verify_cek_context_transition_fixture,
}
use midgard/validation_machine_tests/fixtures_script_sources.{
  late_phase_a_native_scan_witness, maximum_mint_asset_leaves,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

test native_scripts_stream_an_authenticated_execution_and_return_on_l1() {
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    // TimelockStart(0)
    script_bytes: #"820400",
  }
  let base = empty_v1_transaction()
  let valid_tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, validity_interval_start: 0 },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let chunk_proof = singleton_field_item_chunk_proof(6, script_cbor)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    0,
    script_hash,
    bytearray.length(script_cbor),
    bounded_item_v1.from_bytes(6, 0, script_cbor),
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_frontier = validation_merkle_v1.build_frontier([])
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
  let Pair(pre, work_witness_cbor) = native_scripts_step_fixture(
    valid_tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    30,
  )
  let Pair(post, _) = native_scripts_step_fixture(
    valid_tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    1,
    0,
    34,
  )
  let payload_offset = bytearray.length(script_cbor) - bytearray.length(
    script.script_bytes,
  )
  let entry_work_witness_cbor = late_phase_a_native_scan_witness(
    valid_tx,
    work_witness_cbor,
    1,
    bytearray.length(script_cbor),
    bounded_item_v1.from_bytes(6, 0, script_cbor),
    payload_offset,
    0,
    -1,
  )
  let entry_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.PhaseANativeScripts,
    program_counter: 31,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      31,
      entry_work_witness_cbor,
    ),
  }
  let payload_work_witness_cbor = late_phase_a_native_scan_witness(
    valid_tx,
    work_witness_cbor,
    7,
    bytearray.length(script_cbor),
    bounded_item_v1.from_bytes(6, 0, script_cbor),
    payload_offset + 2,
    1,
    -1,
  )
  let payload_pre = next_phase_a_native_state(
    entry_pre,
    payload_work_witness_cbor,
  )
  let finalize_work_witness_cbor = late_phase_a_native_scan_witness(
    valid_tx,
    work_witness_cbor,
    2,
    bytearray.length(script_cbor),
    bounded_item_v1.from_bytes(6, 0, script_cbor),
    bytearray.length(script_cbor),
    1,
    1,
  )
  let finalize_pre = next_phase_a_native_state(
    payload_pre,
    finalize_work_witness_cbor,
  )
  let native_entry = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: entry_pre,
  }
  let native_descriptor = vm_machine_types.NativeExecutionDescriptorWitness {
    execution_index: 0,
    language_tag: 0,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash,
    subject: #"0102",
    purpose_siblings: [],
    source_index: 0,
    origin_kind: 0,
    source_key: #"00",
    script_total_length: bytearray.length(script_cbor),
    script_item_commitment: bounded_item_v1.from_bytes(6, 0, script_cbor),
    source_siblings: [],
    redeemer_leaf: #"",
    execution_siblings: [],
    first_chunk_proof: Some(chunk_proof),
    signer_peaks: [],
  }
  and {
    vm_native_scripts.verify_native_scripts_native_descriptor(
      pre,
      native_entry,
      native_descriptor,
    ),
    !vm_native_scripts.verify_native_scripts_native_descriptor(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        ..native_entry,
        claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
          ..entry_pre,
          work_root: zero,
        },
      },
      native_descriptor,
    ),
    !vm_native_scripts.verify_native_scripts_effectful_descriptor(
      pre,
      native_entry,
      native_descriptor,
    ),
    vm_native_scripts.verify_native_scripts_native_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: entry_pre,
      },
      0,
      0,
      0,
      script_hash,
      #"0102",
      [],
      0,
      0,
      #"00",
      bytearray.length(script_cbor),
      bounded_item_v1.from_bytes(6, 0, script_cbor),
      [],
      #"",
      [],
      chunk_proof,
      [],
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_token_head_semantics_v1(
      entry_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: entry_work_witness_cbor,
        claimed_successor: payload_pre,
      },
      chunk_proof,
      None,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_timelock_payload_semantics_v1(
      payload_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: payload_work_witness_cbor,
        claimed_successor: finalize_pre,
      },
      chunk_proof,
      None,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_advance_semantics_v1(
      finalize_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: finalize_work_witness_cbor,
        claimed_successor: post,
      },
    ),
  }
}

test script_integrity_is_recomputed_before_cek() {
  let empty_integrity_hash = #"01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"
  let base = empty_v1_transaction()
  let valid_tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      script_integrity_hash: empty_integrity_hash,
    },
  }
  let valid_control = empty_native_control_fixture(valid_tx)
  let valid_source = submitted_source(valid_tx)
  let valid_compact = midgard_transaction_to_compact(valid_tx)
  let valid_witness_set = decode_native_tx_witness_set_compact(
    valid_source.witness_set_compact_cbor,
  )
  let authentication_witness_cbor = vm_witness.encode_script_integrity_witness_v1(
    valid_control,
  )
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(valid_compact.body),
  )
  let authentication_pre = forced_pending_state(
    tx_id,
    valid_tx,
    validation_context_cbor,
    validation_trace_v1.ScriptIntegrity,
    34,
    authentication_witness_cbor,
    // Re-based off `frontier_commitment(0, [])`: this fixture backs rejection
    // proofs, so it must carry the realistic non-empty claimed delta.
    non_empty_claimed_delta_root(),
  )
  let compact_witness_cbor = vm_witness.encode_script_integrity_compact_witness_v1(
    valid_control,
  )
  let compact_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..authentication_pre,
    program_counter: 35,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      35,
      compact_witness_cbor,
    ),
  }
  let witness_set_witness_cbor = vm_witness.encode_script_integrity_witness_set_witness_v1(
    valid_control,
    valid_compact.body.script_integrity_hash,
    valid_compact.witness_set_hash,
  )
  let witness_set_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..compact_pre,
    program_counter: 36,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      36,
      witness_set_witness_cbor,
    ),
  }
  let finalize_witness_cbor = vm_witness.encode_script_integrity_finalize_witness_v1(
    valid_control,
    valid_compact.body.script_integrity_hash,
    valid_witness_set.redeemer_tx_wits_hash,
  )
  let finalize_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..witness_set_pre,
    program_counter: 37,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      37,
      finalize_witness_cbor,
    ),
  }
  let valid_post = validation_trace_v1.ValidationMachineStateV1 {
    ..finalize_pre,
    phase: validation_trace_v1.Cek,
    program_counter: 38,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      38,
      vm_witness.encode_cek_witness_v1(
        valid_control,
        #"",
        0,
        0,
        0,
        #"",
        0,
        0,
        #"",
      ),
    ),
  }
  let authentication_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: authentication_witness_cbor,
    claimed_successor: compact_pre,
  }
  let compact_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: compact_witness_cbor,
    claimed_successor: witness_set_pre,
  }
  let witness_set_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: witness_set_witness_cbor,
    claimed_successor: finalize_pre,
  }
  let finalize_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: finalize_witness_cbor,
    claimed_successor: valid_post,
  }
  let invalid_tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, script_integrity_hash: zero },
  }
  let invalid_control = empty_native_control_fixture(invalid_tx)
  let invalid_source = submitted_source(invalid_tx)
  let invalid_witness_set = decode_native_tx_witness_set_compact(
    invalid_source.witness_set_compact_cbor,
  )
  let invalid_compact = midgard_transaction_to_compact(invalid_tx)
  let invalid_tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(invalid_compact.body),
  )
  let invalid_finalize_witness_cbor = vm_witness.encode_script_integrity_finalize_witness_v1(
    invalid_control,
    invalid_compact.body.script_integrity_hash,
    invalid_witness_set.redeemer_tx_wits_hash,
  )
  let invalid_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..authentication_pre,
    transaction_id: invalid_tx_id,
    transaction_commitment: submitted_commitment(invalid_tx),
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      40,
      invalid_finalize_witness_cbor,
    ),
  }
  let rejection_code = #"455f494e56414c49445f4649454c445f54595045"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..invalid_pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      41,
      vm_shared.encode_terminal_rejection_witness(
        rejection_code,
        invalid_pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  and {
    vm_one_step.verify_script_integrity_authentication_semantics_v1(
      authentication_pre,
      authentication_transition,
    ),
    vm_one_step.verify_script_integrity_compact_semantics_v1(
      compact_pre,
      compact_transition,
    ),
    vm_one_step.verify_script_integrity_witness_set_semantics_v1(
      witness_set_pre,
      witness_set_transition,
    ),
    vm_one_step.verify_script_integrity_finalize_semantics_v1(
      finalize_pre,
      finalize_transition,
    ),
    vm_one_step.verify_script_integrity_finalize_semantics_v1(
      invalid_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: invalid_finalize_witness_cbor,
        claimed_successor: terminal,
      },
    ),
  }
}

test resolved_input_accumulator_vector_is_stable() {
  let initial = vm_witness.initial_resolution_accumulator()
  and {
    initial == #"07eb401e2f7e5de17444414ec48a5d9dca455dea72f4675cc2b08bf5b4e39979",
    vm_witness.resolved_input_accumulator_successor(
      initial,
      0,
      #"010203",
      #"040506",
    ) == #"97e2dbdabf1ac8b5046e02f46c8d081ade2d81296b174bf77b9b8c69bd59c9c0",
  }
}

test mint_asset_leaf_vector_is_stable() {
  vm_shared.mint_asset_leaf_hash(
    #"11111111111111111111111111111111111111111111111111111111",
    #"abcd",
    -7,
  ) == #"4813bd9aad26eea82fa41280aefd50c848041a2a6cf27be416e1873c2876a479"
}

test ledger_delta_operation_frontier_vectors_are_stable() {
  let proof_descriptor = empty_mpf_proof_descriptor()
  let deletion = vm_shared.ledger_delta_operation_leaf_hash(
    0,
    #"010203",
    #"",
    proof_descriptor,
  )
  let insertion = vm_shared.ledger_delta_operation_leaf_hash(
    1,
    #"0405",
    #"060708",
    proof_descriptor,
  )
  let frontier = validation_merkle_v1.build_frontier([deletion, insertion])
  and {
    deletion == #"d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589",
    insertion == #"f8bc7029f5f58f0436ebdf6cbbb85bd9adac05d5f6dc1b9238c8166a517aa8db",
    validation_merkle_v1.frontier_commitment(frontier.count, frontier.peaks) == #"b6d017c71f3fc974f620b22764385bf9ad56ee5627009e57dbeb9418e486dcb2",
  }
}

test ledger_delta_terminal_step_binds_more_than_128_operations() {
  // This is the off-chain-built frontier for 129 identical authenticated
  // deletions. Supplying the compact peaks measures only the actual terminal
  // resolver; constructing 129 leaves inside the test would charge fixture
  // setup to the transition.
  let operation_frontier = validation_merkle_v1.BuiltFrontier {
    count: 129,
    peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"d70952a4347195627444cfbb1874f6857de1ad78f095460b76fc826cd267a589",
      },
      validation_merkle_v1.FrontierPeak {
        height: 7,
        hash: #"c4029f40ad899fb153acbf60401c4b0187fc7ce03060d3cbeae09ea32de11cc9",
      },
    ],
  }
  let control = vm_machine_types.LedgerDeltaControlV1 {
    resolved_input_count: 0,
    resolved_inputs_accumulator: vm_witness.initial_resolution_accumulator(),
    output_count: 0,
    output_descriptor_peaks: [],
    stage: 2,
    replay_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    current_ledger_root: h_b,
    output_cursor: 0,
    operation_count: operation_frontier.count,
    operation_peaks: operation_frontier.peaks,
    pending_mutation_cbor: #"",
  }
  let work_witness_cbor = vm_witness.encode_ledger_delta_control_v1(control)
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: h_a,
    transaction_commitment: h_b,
    validation_context_hash: h_c,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.LedgerDelta,
    program_counter: 46,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      46,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_merkle_v1.frontier_commitment(
      operation_frontier.count,
      operation_frontier.peaks,
    ),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 47,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      47,
      vm_witness.encode_terminal_acceptance_witness_v1(
        h_b,
        operation_frontier.count,
        operation_frontier.peaks,
      ),
    ),
    verdict: validation_trace_v1.Accepted,
  }
  vm_ledger_delta.verify_ledger_delta_terminal_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
  )
}

test cek_execution_selection_authenticates_program_and_context_subject() {
  maximum_program_envelope_selection_is_exact()
}

test cek_effectful_selection_rejects_a_51_byte_program_envelope() {
  let oversized = builtin.replicate_byte(
    cek_proof_v1.max_program_envelope_cbor_bytes + 1,
    0,
  )
  and {
    bytearray.length(oversized) == 51,
    cek_proof_v1.inspect_program_envelope_v1(oversized) == None,
  }
}

// Expected-failure inversion keeps the payload byte opaque. The two exact
// pieces stay within Aiken's 8,192-byte `replicate_byte` optimizer bound while
// preserving the 16,379-byte Cardano maximum on the single generated case.
test maximum_native_then_effectful_cek_selection_is_bounded(
  fill_byte via fuzz.byte(),
) fail once {
  let tx = empty_v1_transaction()
  let native_script_bytes = builtin.replicate_byte(8_192, fill_byte)
    |> bytearray.concat(builtin.replicate_byte(8_187, fill_byte))
  let native_script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: native_script_bytes,
  }
  let native_script_cbor = encode_midgard_versioned_script(native_script)
  let native_commitment = bounded_item_v1.from_bytes(6, 0, native_script_cbor)
  let chunk_0 = builtin.slice_bytearray(
    0,
    bounded_item_v1.chunk_bytes,
    native_script_cbor,
  )
  let chunk_1 = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes,
    bounded_item_v1.chunk_bytes,
    native_script_cbor,
  )
  let chunk_2 = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes * 2,
    bounded_item_v1.chunk_bytes,
    native_script_cbor,
  )
  let chunk_3 = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes * 3,
    bounded_item_v1.chunk_bytes,
    native_script_cbor,
  )
  let chunk_4 = builtin.slice_bytearray(
    bounded_item_v1.chunk_bytes * 4,
    4,
    native_script_cbor,
  )
  let hash_0 = bounded_item_v1.hash_chunk(6, 0, 0, chunk_0)
  let hash_1 = bounded_item_v1.hash_chunk(6, 0, 1, chunk_1)
  let hash_2 = bounded_item_v1.hash_chunk(6, 0, 2, chunk_2)
  let hash_3 = bounded_item_v1.hash_chunk(6, 0, 3, chunk_3)
  let hash_4 = bounded_item_v1.hash_chunk(6, 0, 4, chunk_4)
  let chunk_frontier = validation_merkle_v1.build_frontier(
    [hash_0, hash_1, hash_2, hash_3, hash_4],
  )
  let first_chunk_proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 6,
    item_index: 0,
    total_length: bytearray.length(native_script_cbor),
    chunk_index: 0,
    chunk: chunk_0,
    frontier: chunk_frontier.peaks,
    siblings: [hash_1, validation_merkle_v1.hash_branch(hash_2, hash_3)],
  }
  let effectful_script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let effectful_script_cbor = encode_midgard_versioned_script(effectful_script)
  let native_hash = script_proof_v1.versioned_script_hash(native_script)
  let effectful_hash = script_proof_v1.versioned_script_hash(effectful_script)
  let native_source = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    0,
    native_hash,
    bytearray.length(native_script_cbor),
    native_commitment,
  )
  let effectful_source = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"01",
    3,
    effectful_hash,
    bytearray.length(effectful_script_cbor),
    bounded_item_v1.from_bytes(6, 1, effectful_script_cbor),
  )
  let native_purpose = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    native_hash,
    #"00",
  )
  let effectful_purpose = script_proof_v1.purpose_leaf_hash(
    0,
    1,
    effectful_hash,
    #"01",
  )
  let native_execution = script_proof_v1.execution_leaf_hash(
    0,
    native_purpose,
    native_source,
    #"",
  )
  let effectful_execution = script_proof_v1.execution_leaf_hash(
    3,
    effectful_purpose,
    effectful_source,
    h_c,
  )
  let source_frontier = validation_merkle_v1.build_frontier(
    [native_source, effectful_source],
  )
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [native_purpose, effectful_purpose],
  )
  let execution_frontier = validation_merkle_v1.build_frontier(
    [native_execution, effectful_execution],
  )
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    source_count: 2,
    source_peaks: source_frontier.peaks,
    purpose_count: 2,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 2,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 2,
    language_bitmap: 1,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(tx, control, #"", 0, 43)
  let Pair(post, _) = cek_state_fixture(tx, control, #"", 1, 44)
  let evidence = one_step_evidence(
    work_witness_cbor,
    post,
    vm_machine_types.NativeExecutionScanWitness {
      execution_index: 0,
      language_tag: 0,
      purpose_kind: 0,
      purpose_index: 0,
      script_hash: native_hash,
      subject: #"00",
      purpose_siblings: [effectful_purpose],
      source_index: 0,
      origin_kind: 0,
      source_key: #"00",
      script_total_length: bytearray.length(native_script_cbor),
      script_item_commitment: native_commitment,
      source_siblings: [effectful_source],
      redeemer_leaf: #"",
      execution_siblings: [effectful_execution],
      first_chunk_proof,
    },
  )
  !and {
    bytearray.length(native_script_bytes) == 16_379,
    bytearray.length(native_script_cbor) == 16_384,
    bytearray.length(first_chunk_proof.chunk) == bounded_item_v1.chunk_bytes,
    bytearray.length(cbor.serialise(evidence)) < 16_384,
    vm_cek.verify_cek_execution_selection(
      pre,
      evidence.transition,
      evidence.auxiliary,
      control,
      0,
      0,
      0,
    ),
  }
}

test cross_language_cek_context_control_vectors() {
  let context = vm_machine_types.CekContextControlV1 {
    stage: 0,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: h_b,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_b,
    subject: #"00",
    redeemer_leaf: h_c,
    redeemer_context_control_hash: #"",
    execution_memory_limit: 0,
    execution_cpu_limit: 0,
    reference_items: cek_data_v1.empty_data_list_summary_v1(),
    spend_items: cek_data_v1.empty_data_list_summary_v1(),
    output_items: cek_data_v1.empty_data_list_summary_v1(),
    signer_items: cek_data_v1.empty_data_list_summary_v1(),
    observer_count: 0,
    observer_items: cek_data_v1.empty_data_pair_summary_v1(),
    previous_observer: #"",
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_cursor: 0,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
    mint_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
  }
  and {
    vm_witness.encode_cek_context_control_v1(context) == #"981900035820aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa5820bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb0000581cbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb41005820cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc4000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa560000008458208c446a903f125939fd6e036b313c52340c9ac0539e6730f08e95eaec9052fa5600000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000040834000000040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba200000083400000",
    vm_witness.hash_cek_redeemer_context_control_v1(
      vm_witness.initial_cek_redeemer_context_control_v1(0),
    ) == #"16d11fb592d3cad09ef73bd20c31a8d35a0f502c4eeeddf00ab3e8d037bb6a3f",
  }
}

test plutus_v3_receive_selection_rejects_with_an_exact_noop() {
  let tx = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let first_chunk_proof = singleton_field_item_chunk_proof(6, script_cbor)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    3,
    script_hash,
    bytearray.length(script_cbor),
    script_item_commitment,
  )
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    3,
    0,
    script_hash,
    script_hash,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    3,
    purpose_leaf,
    source_leaf,
    h_c,
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([execution_leaf])
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    source_count: 1,
    source_peaks: source_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(tx, control, #"", 0, 40)
  let rejection_code = #"455f504c555455535f5343524950545f494e56414c4944"
  // Canonical exact-rejection successor: it carries the pre-state's non-empty
  // claimed delta forward. See the note on
  // `static_rules_prove_a_network_mismatch_is_an_exact_no_op`.
  let post = exact_rejection_post(pre, rejection_code)
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NativeExecutionScanWitness {
        execution_index: 0,
        language_tag: 3,
        purpose_kind: 3,
        purpose_index: 0,
        script_hash,
        subject: script_hash,
        purpose_siblings: [],
        source_index: 0,
        origin_kind: 0,
        source_key: #"00",
        script_total_length: bytearray.length(script_cbor),
        script_item_commitment,
        source_siblings: [],
        redeemer_leaf: h_c,
        execution_siblings: [],
        first_chunk_proof,
      },
    ),
    inline_field_door(),
  )
}

test cek_redeemer_selection_binds_the_data_scan_initial_state() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: #"00",
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
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
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count: 1,
    redeemer_peaks: redeemer_frontier.peaks,
  }
  // Stage-0 redeemer selection is a *chain*, not a single step
  // (`verify_cek_redeemer_selection`, validation-machine-v1.ak:14967).
  // `RedeemerScanBeginWitness` only opens a bounded `mode_descriptor`
  // `redeemer_item_proof_v1` control and parks its hash in
  // `redeemer_context_control_hash` while the stage stays 0 (:15015-15020);
  // the stage only advances to 1 -- binding the *initial* redeemer-context
  // (data-scan) control and the authenticated execution-unit limits -- once a
  // `RedeemerItemStepWitness` drives that item control to
  // `stage_terminal` (:15053-15074). The whole chain is exercised here.
  let redeemer_item_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(
    8,
    0,
    redeemer_item_cbor,
  )
  let redeemer_chunk_proof = singleton_field_item_chunk_proof(
    8,
    redeemer_item_cbor,
  )
  expect Some(redeemer_tag) = vm_shared.redeemer_tag_for_purpose_kind_v1(0)
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_descriptor,
    0,
    1,
    bytearray.length(redeemer_item_cbor),
    redeemer_item_commitment,
    redeemer_tag,
    0,
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
  let initial_context = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(0),
    redeemer_leaf,
    execution_memory_limit: 0,
    execution_cpu_limit: 0,
  }
  let opened_context = vm_machine_types.CekContextControlV1 {
    ..initial_context,
    redeemer_context_control_hash: redeemer_item_proof_v1.hash_control_v1(
      item_control,
    ),
  }
  let tail_context = vm_machine_types.CekContextControlV1 {
    ..initial_context,
    redeemer_context_control_hash: redeemer_item_proof_v1.hash_control_v1(
      tail_control,
    ),
  }
  let redeemer_context = vm_witness.initial_cek_redeemer_context_control_v1(
    native_control.purpose_count,
  )
  let selected_context = vm_machine_types.CekContextControlV1 {
    ..initial_context,
    stage: 1,
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      redeemer_context,
    ),
    execution_memory_limit: redeemer.execution_units.memory,
    execution_cpu_limit: redeemer.execution_units.steps,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(initial_context),
    0,
    41,
  )
  let Pair(opened, opened_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(opened_context),
    0,
    42,
  )
  let Pair(tail, tail_work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(tail_context),
    0,
    43,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(selected_context),
    0,
    44,
  )
  and {
    vm_one_step.verify_one_step_evidence(
      pre,
      one_step_evidence(
        work_witness_cbor,
        opened,
        vm_machine_types.RedeemerScanBeginWitness {
          item_index: 0,
          item_count: 1,
          total_length: bytearray.length(redeemer_item_cbor),
          item_commitment: redeemer_item_commitment,
          siblings: [],
        },
      ),
      inline_field_door(),
    ),
    vm_one_step.verify_one_step_evidence(
      opened,
      one_step_evidence(
        opened_witness_cbor,
        tail,
        vm_machine_types.RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: header_witness,
        },
      ),
      inline_field_door(),
    ),
    vm_one_step.verify_one_step_evidence(
      tail,
      one_step_evidence(
        tail_work_witness_cbor,
        post,
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

test cek_context_spend_item_uses_authenticated_descriptor_frontier() {
  let tx = empty_v1_transaction()
  let input = MidgardTxInput { tx_id: h_b, output_index: 2 }
  let key = encode_midgard_tx_input(input)
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 1, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let value = encode_midgard_tx_output(output)
  let descriptor_cbor = exact_output_descriptor_fixture(2, value)
  let substituted_key = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_b, output_index: 3 },
  )
  let resolved_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    0,
    0,
    key,
    descriptor_cbor,
  )
  let resolved_frontier = validation_merkle_v1.build_frontier([resolved_leaf])
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, key)
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([h_a])
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolved_input_count: 1,
    resolved_inputs_accumulator: vm_witness.resolved_input_accumulator_successor(
      vm_witness.initial_resolution_accumulator(),
      0,
      key,
      descriptor_cbor,
    ),
    spend_input_count: 1,
    resolved_item_peaks: resolved_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let context_control = vm_machine_types.CekContextControlV1 {
    stage: 2,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: h_b,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: key,
    redeemer_leaf: h_c,
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      vm_witness.initial_cek_redeemer_context_control_v1(control.purpose_count),
    ),
    execution_memory_limit: 10,
    execution_cpu_limit: 20,
    reference_items: cek_data_v1.empty_data_list_summary_v1(),
    spend_items: cek_data_v1.empty_data_list_summary_v1(),
    output_items: cek_data_v1.empty_data_list_summary_v1(),
    signer_items: cek_data_v1.empty_data_list_summary_v1(),
    observer_count: 0,
    observer_items: cek_data_v1.empty_data_pair_summary_v1(),
    previous_observer: #"",
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_cursor: 0,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
    mint_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
  }
  expect Some(next_spend_items) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
    1,
    resolved_frontier.peaks,
    1,
    0,
    0,
    key,
    descriptor_cbor,
    [],
    False,
    context_control.spend_items,
  )
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    41,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(
      vm_machine_types.CekContextControlV1 {
        ..context_control,
        spend_items: next_spend_items,
      },
    ),
    0,
    42,
  )
  let evidence = one_step_evidence(
    work_witness_cbor,
    post,
    vm_machine_types.CekResolvedContextItemWitness {
      source_kind: 0,
      item_index: 0,
      key,
      descriptor_cbor,
      siblings: [],
    },
  )
  and {
    vm_cek.verify_cek_spend_context_step(
      pre,
      evidence.transition,
      evidence.auxiliary,
      control,
      context_control,
      0,
      0,
      0,
    ),
    script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
      1,
      resolved_frontier.peaks,
      1,
      0,
      0,
      substituted_key,
      descriptor_cbor,
      [],
      False,
      context_control.spend_items,
    ) == None,
  }
}

test cek_context_reference_item_uses_authenticated_descriptor_frontier() {
  let tx = empty_v1_transaction()
  let input = MidgardTxInput { tx_id: h_b, output_index: 2 }
  let key = encode_midgard_tx_input(input)
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: True,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 1, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let descriptor_cbor = exact_output_descriptor_fixture(
    2,
    encode_midgard_tx_output(output),
  )
  let resolved_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    1,
    0,
    key,
    descriptor_cbor,
  )
  let resolved_frontier = validation_merkle_v1.build_frontier([resolved_leaf])
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolved_input_count: 1,
    resolved_inputs_accumulator: vm_witness.resolved_input_accumulator_successor(
      vm_witness.initial_resolution_accumulator(),
      1,
      key,
      descriptor_cbor,
    ),
    spend_input_count: 0,
    resolved_item_peaks: resolved_frontier.peaks,
    execution_cursor: 0,
  }
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(1),
    language_tag: 128,
  }
  expect Some(next_reference_items) = script_context_v1.prepend_resolved_descriptor_tx_in_info_v1(
    1,
    resolved_frontier.peaks,
    0,
    1,
    0,
    key,
    descriptor_cbor,
    [],
    True,
    context_control.reference_items,
  )
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    reference_items: next_reference_items,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    42,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    43,
  )
  vm_cek.verify_cek_reference_context_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    vm_machine_types.CekResolvedContextItemWitness {
      source_kind: 1,
      item_index: 0,
      key,
      descriptor_cbor,
      siblings: [],
    },
    control,
    context_control,
    0,
    0,
    0,
  )
}

test cek_context_output_item_uses_authenticated_descriptor_frontier() {
  let tx = empty_v1_transaction()
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 1, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let descriptor_cbor = exact_output_descriptor_fixture(0, output_cbor)
  let descriptor_leaf = script_proof_v1.output_descriptor_leaf_hash(
    0,
    descriptor_cbor,
  )
  let descriptor_frontier = validation_merkle_v1.build_frontier(
    [descriptor_leaf],
  )
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    output_count: 1,
    output_descriptor_peaks: descriptor_frontier.peaks,
  }
  let context_control = empty_cek_context_control_fixture(3)
  expect Some(next_output_items) = script_context_v1.prepend_output_descriptor_v1(
    1,
    descriptor_frontier.peaks,
    0,
    descriptor_cbor,
    [],
    False,
    context_control.output_items,
  )
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    output_items: next_output_items,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    43,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    44,
  )
  let evidence = one_step_evidence(
    work_witness_cbor,
    post,
    vm_machine_types.CekOutputContextItemWitness {
      output_index: 0,
      descriptor_cbor,
      siblings: [],
    },
  )
  and {
    vm_cek.verify_cek_output_context_step(
      pre,
      evidence.transition,
      evidence.auxiliary,
      control,
      context_control,
      0,
      0,
      0,
    ),
    script_context_v1.prepend_output_descriptor_v1(
      1,
      descriptor_frontier.peaks,
      0,
      exact_output_descriptor_fixture(
        0,
        encode_midgard_tx_output(
          MidgardTxOutput {
            ..output,
            value: MidgardValue { lovelace: 2, assets: [] },
          },
        ),
      ),
      [],
      False,
      context_control.output_items,
    ) == None,
  }
}

test maximum_spend_input_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_spend_input_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test maximum_reference_input_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_reference_input_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test maximum_observer_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_observer_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test maximum_output_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_output_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test maximum_required_signer_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_required_signer_field_terminal_fixture_v1()),
) {
  verify_maximum_field_terminal_fixture_v1(fixture)
}

test cek_context_signer_item_uses_authenticated_signer_frontier() {
  let tx = empty_v1_transaction()
  let signer_hash = h_28_b
  let signer_leaf = script_proof_v1.signer_leaf_hash(signer_hash)
  let signer_frontier = validation_merkle_v1.build_frontier([signer_leaf])
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    signer_count: 1,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
  }
  let context_control = empty_cek_context_control_fixture(4)
  expect Some(next_signer_items) = script_context_v1.prepend_signer_v1(
    1,
    control.signer_frontier_commitment,
    signer_frontier.peaks,
    0,
    signer_hash,
    [],
    context_control.signer_items,
  )
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    43,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(
      vm_machine_types.CekContextControlV1 {
        ..context_control,
        signer_items: next_signer_items,
      },
    ),
    0,
    44,
  )
  verify_bounded_context_parity(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.CekSignerContextItemWitness {
        peaks: signer_frontier.peaks,
        signer_index: 0,
        signer_hash,
        siblings: [],
      },
    ),
    inline_field_door(),
  )
}

test cek_context_observer_cardano_maximum_224_first_item_and_terminal_agree() {
  let base = empty_v1_transaction()
  // The same signed transaction the field-3 terminal fixture pins, through the
  // same generated constants — one exact source for both selectors, rather than
  // two hand-mirrored copies that could drift apart (#590).
  let transaction_id = maximum_observer_terminal_transaction_id
  let transaction_commitment = maximum_observer_terminal_transaction_commitment
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(base),
    compact_cbor: maximum_observer_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_observer_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_observer_terminal_field_preimage_lengths_cbor,
  }
  let context_control = empty_cek_context_control_fixture(5)
  let observers_carriage = native_tx_field_access_v1.Inline {
    preimage: maximum_observer_terminal_field_preimage_cbor,
  }
  // The CEK observer list is built back to front, so the first item step reads
  // the *last* observer. Derived from the authenticated preimage rather than
  // pinned: under §8 the item's bytes are not a prover claim any more.
  expect Some(observer_hash) = list.last(
    decode_midgard_tx_byte_list_preimage_cbor(
      maximum_observer_terminal_field_preimage_cbor,
    ),
  )
  let first_items = vm_shared.prepend_cek_observer_item_v1(
    observer_hash,
    False,
    context_control.observer_items,
  )
  let first_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    observer_count: 224,
    observer_items: first_items,
    previous_observer: observer_hash,
  }
  let mismatched_count_control = vm_machine_types.CekContextControlV1 {
    ..first_control,
    observer_count: 225,
  }
  let complete_items = cek_data_v1.DataSequenceSummaryV1 {
    root: #"354faff8c56e5c2db595d2b0682336623a4de555e8426762630783d26921ab5d",
    length: 224,
    payload_cbor_length: 7840,
    memory: 9184,
  }
  let complete_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    observer_count: 224,
    observer_items: complete_items,
    previous_observer: #"008b91b576da13635f7150f248cd133c23ac0a697f1ccae85617ea85",
  }
  let observer_summary = cek_data_v1.DataSummaryV1 {
    root: #"61eff6b0a693f2da6f3fd45ae1b3d402937e3ab0de94da2fcbd5ed67ab61a726",
    cbor_length: 7842,
    memory: 9188,
  }
  let final_control = vm_machine_types.CekContextControlV1 {
    ..complete_control,
    stage: 6,
    observer_summary,
  }
  and {
    observer_summary == vm_shared.finalize_cek_observer_items_v1(
      complete_items,
      False,
    ),
    verify_cek_context_source_transition_fixture(
      transaction_id,
      transaction_commitment,
      native_control,
      context_control,
      first_control,
      vm_machine_types.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 223,
        carriage: observers_carriage,
      },
      45,
    ),
    verify_cek_context_source_transition_fixture(
      transaction_id,
      transaction_commitment,
      native_control,
      complete_control,
      final_control,
      vm_machine_types.NoAuxiliaryWitness,
      46,
    ),
    !verify_cek_context_source_transition_fixture(
      transaction_id,
      transaction_commitment,
      native_control,
      context_control,
      mismatched_count_control,
      // The count is no longer a prover field: it is §5.2's authenticated header
      // of the very bytes the door hashed. What a prover can still claim wrongly
      // is the successor's `observer_count`, and `mismatched_count_control` does
      // exactly that against the honest carriage — the same 225-against-224
      // overstatement the retired `ItemProofV1.item_count: 225` expressed.
      vm_machine_types.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 223,
        carriage: observers_carriage,
      },
      47,
    ),
  }
}

test cek_context_observer_empty_field_has_exact_no_auxiliary_path() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let context_control = empty_cek_context_control_fixture(5)
  let observer_summary = vm_shared.finalize_cek_observer_items_v1(
    context_control.observer_items,
    False,
  )
  let successor_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    stage: 6,
    observer_summary,
  }
  let carriage = inline_field_carriage([h_28_a])
  and {
    verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      successor_control,
      vm_machine_types.NoAuxiliaryWitness,
      48,
    ),
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      successor_control,
      // An empty field-3 slot admits no item step at all: the auxiliary must be
      // `NoAuxiliaryWitness`, so any carriage — even one that is a well-formed
      // §5.1 preimage — is refused before the door is consulted.
      vm_machine_types.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 0,
        carriage,
      },
      49,
    ),
  }
}

/// The `CekContext` observer step's negative space, after §8's door left it
/// with only three prover-controlled inputs.
///
/// **What retired here, and what it became.** Eight rows used to perturb a
/// counted `ItemProofV1`: its `item_index`, its `field_index`, its `item_count`,
/// its `item_length`, and the paired `ChunkProofV1.total_length`. Under §8 the
/// item's bytes, its length and the field's §5.2 count are all *derived* from a
/// preimage the door hashed once against `required_observers_hash`, so five of
/// those five fields no longer exist to forge — a prover that changes any of them
/// has changed the preimage, and the door refuses it by aborting (§7.3), which is
/// fail-closed rather than a `False` a `!verify` row could observe.
///
/// The two that survive as evidence are the pair the machine still checks
/// against its own cursor (`field_index == 3`, `item_index == expected_index`),
/// and the count/length claims move to where they are now decidable: the
/// **claimed successor**. `observer_count` is claimed there, and understating it
/// is exactly the attack the retired `item_count` perturbation stood for. Each
/// row below is one falsifier, measured.
test cek_context_observer_rejects_malformed_proofs_order_and_successors() {
  let base = empty_v1_transaction()
  let observers = observer_hashes_from(0, 2)
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: observers },
  }
  let native_control = cek_context_native_control_fixture(tx)
  let context_control = empty_cek_context_control_fixture(5)
  let observers_carriage = observer_field_carriage(2)
  let first_items = vm_shared.prepend_cek_observer_item_v1(
    observer_hash_at(1),
    False,
    context_control.observer_items,
  )
  let first_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    observer_count: 2,
    observer_items: first_items,
    previous_observer: observer_hash_at(1),
  }
  let malformed_successor = vm_machine_types.CekContextControlV1 {
    ..first_control,
    previous_observer: observer_hash_at(0),
  }
  // Replaces the retired `ItemProofV1.item_count: 1` perturbation: the field's
  // authenticated §5.2 count is 2, and a successor claiming 1 is the same
  // understatement, now made where a prover can actually make it.
  let understated_count_successor = vm_machine_types.CekContextControlV1 {
    ..first_control,
    observer_count: 1,
  }
  let next_items = vm_shared.prepend_cek_observer_item_v1(
    observer_hash_at(0),
    False,
    first_items,
  )
  let wrong_order_control = vm_machine_types.CekContextControlV1 {
    ..first_control,
    previous_observer: observer_hash_at(0),
  }
  let complete_control = vm_machine_types.CekContextControlV1 {
    ..wrong_order_control,
    observer_items: next_items,
  }
  let exact_summary = vm_shared.finalize_cek_observer_items_v1(
    next_items,
    False,
  )
  let wrong_summary = cek_data_v1.DataSummaryV1 { ..exact_summary, root: h_a }
  let wrong_summary_control = vm_machine_types.CekContextControlV1 {
    ..complete_control,
    stage: 6,
    observer_summary: wrong_summary,
  }
  // The list is built back to front, so the first item step must read index 1.
  let honest_auxiliary = vm_machine_types.TransactionFieldChunkWitness {
    field_index: 3,
    item_index: 1,
    carriage: observers_carriage,
  }
  and {
    // The honest first step.
    verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      honest_auxiliary,
      50,
    ),
    // Descending-order violation in the claimed successor.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      malformed_successor,
      honest_auxiliary,
      51,
    ),
    // An `item_index` that is not the one the accumulated list derives.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      vm_machine_types.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 0,
        carriage: observers_carriage,
      },
      52,
    ),
    // A successor understating the field's authenticated item count.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      understated_count_successor,
      honest_auxiliary,
      53,
    ),
    // Evidence naming a different field slot. §4 removed domain separation, so
    // this is refused by the step's own `field_index == 3` and by nothing else.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      context_control,
      first_control,
      vm_machine_types.TransactionFieldChunkWitness {
        field_index: 2,
        item_index: 1,
        carriage: observers_carriage,
      },
      54,
    ),
    // The second step taken out of order: `previous_observer` is already the
    // lesser hash, so nothing remains that can precede it.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      wrong_order_control,
      complete_control,
      vm_machine_types.TransactionFieldChunkWitness {
        field_index: 3,
        item_index: 0,
        carriage: observers_carriage,
      },
      57,
    ),
    // And the finalization summary must be the exact fold of what was read.
    !verify_cek_context_transition_fixture(
      tx,
      native_control,
      complete_control,
      wrong_summary_control,
      vm_machine_types.NoAuxiliaryWitness,
      58,
    ),
  }
}

test cek_context_observer_complete_summary_relation_is_exact() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let empty_items = cek_data_v1.empty_data_pair_summary_v1()
  let exact_summary = vm_shared.finalize_cek_observer_items_v1(
    empty_items,
    False,
  )
  let malformed_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary: cek_data_v1.DataSummaryV1 { ..exact_summary, root: h_a },
  }
  let successor_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(9),
    observer_summary: exact_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(malformed_control),
    0,
    59,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(successor_control),
    0,
    60,
  )
  !vm_one_step.verify_cek_one_step_v1(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
  )
}

test cek_context_mint_is_authenticated_and_folded_incrementally() {
  // §5.6 — the enveloped list of per-policy items, not the retired raw map.
  let mint_cbor = native_tx_field_access_v1.encode_field_preimage(
    [
      #"82581c"
        |> bytearray.concat(h_28_a)
        |> bytearray.concat(#"a1410124"),
    ],
  )
  let mint = decode_midgard_tx_mint_preimage_cbor(mint_cbor)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let mint_leaf = vm_shared.mint_asset_leaf_hash(h_28_a, #"01", -5)
  let mint_frontier = validation_merkle_v1.build_frontier([mint_leaf])
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let init_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary,
  }
  let semantic_control = vm_machine_types.CekContextControlV1 {
    ..init_control,
    stage: 8,
  }
  let Pair(init_pre, init_work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(init_control),
    0,
    47,
  )
  let Pair(init_post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(semantic_control),
    0,
    48,
  )
  let init_step = vm_one_step.verify_cek_one_step_v1(
    init_pre,
    one_step_evidence(
      init_work_witness_cbor,
      init_post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
  )
  let asset_items = cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(#"01") |> cek_data_v1.semantic_data_summary_v1,
    builtin.i_data(-5) |> cek_data_v1.semantic_data_summary_v1,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let item_control = vm_machine_types.CekContextControlV1 {
    ..semantic_control,
    mint_cursor: 1,
    current_mint_policy: h_28_a,
    current_mint_assets: asset_items,
  }
  let Pair(item_pre, item_work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(semantic_control),
    0,
    48,
  )
  let Pair(item_post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(item_control),
    0,
    49,
  )
  let item_step = vm_one_step.verify_cek_one_step_v1(
    item_pre,
    one_step_evidence(
      item_work_witness_cbor,
      item_post,
      vm_machine_types.CekMintContextItemWitness {
        previous: None,
        mint_index: 0,
        policy_id: h_28_a,
        asset_name: #"01",
        quantity: -5,
        siblings: [],
      },
    ),
    inline_field_door(),
  )
  let policy_items = cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(h_28_a) |> cek_data_v1.semantic_data_summary_v1,
    cek_data_v1.map_data_summary_v1(asset_items),
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let final_control = vm_machine_types.CekContextControlV1 {
    ..item_control,
    stage: 9,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: policy_items,
    mint_summary: cek_data_v1.map_data_summary_v1(policy_items),
  }
  let Pair(final_pre, final_work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(item_control),
    0,
    49,
  )
  let Pair(final_post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(final_control),
    0,
    50,
  )
  let final_step = vm_one_step.verify_cek_one_step_v1(
    final_pre,
    one_step_evidence(
      final_work_witness_cbor,
      final_post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
  )
  and {
    init_step,
    item_step,
    final_step,
  }
}

test cek_context_redeemer_scalar_scan_is_content_addressed() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: cbor.serialise(builtin.i_data(0)),
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let subject = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_b, output_index: 0 },
  )
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, subject)
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count: 1,
    redeemer_peaks: redeemer_frontier.peaks,
    purpose_peaks: purpose_frontier.peaks,
  }
  let raw = cbor.serialise(builtin.i_data(0))
  let scan = cek_data_scan_v1.initial_control_v1(
    blake2b_256(raw),
    bytearray.length(raw),
  )
  expect Some(completed_scan) = cek_data_scan_v1.reveal_leaf_control_step_v1(
    scan,
    raw,
    None,
    bytearray.length(raw),
  )
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let mint_summary = cek_data_v1.map_data_summary_v1(
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  expect Some(purpose_summary) = script_context_v1.script_purpose_summary_v1(
    0,
    h_28_a,
    subject,
    False,
  )
  let active_redeemers = vm_machine_types.CekRedeemerContextControlV1 {
    ..vm_witness.initial_cek_redeemer_context_control_v1(1),
    active_scan_hash: cek_data_scan_v1.hash_control_v1(scan),
    active_redeemer_leaf: redeemer_leaf,
    active_purpose: purpose_summary,
    purpose_bound: 0,
  }
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(9),
    subject,
    redeemer_leaf,
    observer_summary,
    mint_summary,
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      active_redeemers,
    ),
  }
  let map_items = cek_data_v1.prepend_data_pair_summary_v1(
    purpose_summary,
    completed_scan.result,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let completed_redeemers = vm_machine_types.CekRedeemerContextControlV1 {
    cursor: 1,
    map_items,
    active_scan_hash: #"",
    active_redeemer_leaf: #"",
    active_purpose: cek_data_scan_v1.empty_result_v1(),
    current_redeemer: completed_scan.result,
    purpose_bound: 0,
  }
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    stage: 10,
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      completed_redeemers,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    52,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    53,
  )
  and {
    pre.program_counter == 52,
    work_witness_cbor != #"",
    post.program_counter == 53,
    vm_witness.hash_cek_redeemer_context_control_v1(active_redeemers) != #"",
  }
}

test cek_context_redeemer_map_selection_authenticates_purpose_and_redeemer() {
  let redeemer = MidgardRedeemerWitness {
    purpose: SpendRedeemer,
    index: 0,
    redeemer_cbor: cbor.serialise(builtin.i_data(0)),
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let subject = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_b, output_index: 0 },
  )
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, subject)
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count: 1,
    redeemer_peaks: redeemer_frontier.peaks,
    purpose_peaks: purpose_frontier.peaks,
    execution_peaks: validation_merkle_v1.build_frontier(
      [script_proof_v1.execution_leaf_hash(3, purpose_leaf, h_a, redeemer_leaf)],
    ).peaks,
  }
  let initial_redeemers = vm_witness.initial_cek_redeemer_context_control_v1(
    native_control.purpose_count,
  )
  // The active redeemer scan is a bounded `redeemer_item_proof_v1` control, not
  // the superseded free-standing `cek_data_scan_v1` control:
  // `verify_cek_redeemer_data_step` seeds `active_scan_hash` with
  // `redeemer_item_proof_v1.hash_control_v1` of a fresh `mode_data` item control
  // (validation-machine-v1.ak:15818-15839) and the follow-on
  // `RedeemerItemStepWitness` branch re-derives exactly that hash (:15868).
  // `validation-machine-v1.ak` no longer references `cek_data_scan_v1` at all.
  let redeemer_item_cbor = encode_midgard_redeemer_witness(redeemer)
  let redeemer_item_commitment = bounded_item_v1.from_bytes(
    8,
    0,
    redeemer_item_cbor,
  )
  expect Some(redeemer_tag) = vm_shared.redeemer_tag_for_purpose_kind_v1(0)
  let item_control = redeemer_item_proof_v1.initial_control_v1(
    redeemer_item_proof_v1.mode_data,
    0,
    1,
    bytearray.length(redeemer_item_cbor),
    redeemer_item_commitment,
    redeemer_tag,
    0,
  )
  expect Some(purpose_summary) = script_context_v1.script_purpose_summary_v1(
    0,
    h_28_a,
    subject,
    False,
  )
  let active_redeemers = vm_machine_types.CekRedeemerContextControlV1 {
    ..initial_redeemers,
    active_scan_hash: redeemer_item_proof_v1.hash_control_v1(item_control),
    active_redeemer_leaf: redeemer_leaf,
    active_purpose: purpose_summary,
    purpose_bound: 0,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(9),
    subject,
    redeemer_leaf,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      initial_redeemers,
    ),
  }
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      active_redeemers,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    53,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    54,
  )
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.CekRedeemerContextSelectWitness {
        control: initial_redeemers,
        item_index: 0,
        total_length: bytearray.length(redeemer_item_cbor),
        item_commitment: redeemer_item_commitment,
        purpose_kind: 0,
        purpose_index: 0,
        script_hash: h_28_a,
        subject,
        execution_language_tag: 3,
        source_leaf: h_a,
        execution_siblings: [],
        item_frontier_root: validation_merkle_v1.frontier_commitment(
          1,
          validation_merkle_v1.build_frontier(
            [bounded_item_v1.hash_chunk(8, 0, 0, redeemer_item_cbor)],
          ).peaks,
        ),
      },
    ),
    inline_field_door(),
  )
}

fn cek_context_finalize_with_redeemer_count(redeemer_count: Int) -> Bool {
  let redeemer = MidgardRedeemerWitness {
    purpose: RewardRedeemer,
    index: 0,
    redeemer_cbor: cbor.serialise(builtin.i_data(0)),
    execution_units: MidgardExecutionUnits { memory: 10, steps: 20 },
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: [h_28_a] },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      redeemer_tx_wits: [redeemer],
    },
  }
  let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(0, redeemer)
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a)
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    redeemer_count,
    redeemer_peaks: validation_merkle_v1.build_frontier(
      list.repeat(redeemer_leaf, redeemer_count),
    ).peaks,
    purpose_peaks: validation_merkle_v1.build_frontier([purpose_leaf]).peaks,
  }
  let redeemer_summary = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(purpose_summary) = script_context_v1.script_purpose_summary_v1(
    2,
    h_28_a,
    h_28_a,
    False,
  )
  let redeemer_items = cek_data_v1.prepend_data_pair_summary_v1(
    purpose_summary,
    redeemer_summary,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let redeemer_control = vm_machine_types.CekRedeemerContextControlV1 {
    cursor: 1,
    map_items: redeemer_items,
    active_scan_hash: #"",
    active_redeemer_leaf: #"",
    active_purpose: cek_data_scan_v1.empty_result_v1(),
    current_redeemer: redeemer_summary,
    purpose_bound: 0,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [h_28_a],
    False,
  )
  // Stage 10 is gated by `mint_context_is_final` (validation-machine-v1.ak:13631),
  // which requires `observer_context_is_complete` (:13602): the replayed
  // `observer_items`/`observer_count`/`previous_observer` must be exactly the
  // state that produced `observer_summary`. This transaction carries one
  // required observer, so the observer scan must be shown as completed over
  // that one observer -- leaving the pristine (count 0, empty items) fixture
  // in place while overriding `observer_summary` with a one-observer summary
  // is not a reachable control.
  let observer_items = vm_shared.prepend_cek_observer_item_v1(
    h_28_a,
    False,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(10),
    purpose_kind: 2,
    script_hash: h_28_a,
    subject: h_28_a,
    redeemer_leaf,
    observer_count: 1,
    observer_items,
    previous_observer: h_28_a,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      redeemer_control,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    54,
  )
  expect Some(script_info) = script_context_v1.cardano_script_info_summary_v1(
    2,
    h_28_a,
    h_28_a,
    None,
  )
  let parts = vm_machine_types.CekContextPartsControlV1 {
    redeemer_items,
    redeemer: redeemer_summary,
    script_info,
  }
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    stage: 11,
    redeemer_context_control_hash: vm_witness.hash_cek_context_parts_control_v1(
      parts,
    ),
  }
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    55,
  )
  and {
    observer_summary == vm_shared.finalize_cek_observer_items_v1(
      observer_items,
      False,
    ),
    vm_one_step.verify_one_step_evidence(
      pre,
      one_step_evidence(
        work_witness_cbor,
        post,
        vm_machine_types.CekContextFinalizeWitness { redeemer_control },
      ),
      inline_field_door(),
    ),
  }
}

test cek_context_finalize_seeds_the_exact_semantic_context_application() {
  cek_context_finalize_with_redeemer_count(1)
}

// Completion needs every redeemer folded: with one of two folded the
// finalize step is refused.
test cek_context_finalize_refuses_an_unfolded_redeemer() fail {
  cek_context_finalize_with_redeemer_count(2)
}

test cek_spend_finalize_authenticates_descriptor_datum_summary() {
  let tx = empty_v1_transaction()
  let input = MidgardTxInput { tx_id: h_b, output_index: 2 }
  let subject = encode_midgard_tx_input(input)
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 1, assets: [] },
    datum_cbor: Some(cbor.serialise(builtin.i_data(7))),
    script_ref: None,
  }
  let output_cbor = encode_midgard_tx_output(output)
  let descriptor_cbor = exact_output_descriptor_fixture(2, output_cbor)
  let resolved_leaf = script_proof_v1.resolved_context_item_leaf_hash(
    0,
    0,
    subject,
    descriptor_cbor,
  )
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    resolved_input_count: 1,
    spend_input_count: 1,
    resolved_item_peaks: validation_merkle_v1.build_frontier([resolved_leaf]).peaks,
  }
  let redeemer_summary = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  let redeemer_control = vm_machine_types.CekRedeemerContextControlV1 {
    ..vm_witness.initial_cek_redeemer_context_control_v1(
      native_control.purpose_count,
    ),
    current_redeemer: redeemer_summary,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(10),
    purpose_kind: 0,
    purpose_index: 0,
    subject,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      redeemer_control,
    ),
  }
  expect Some(script_info) = script_context_v1.cardano_spend_script_info_from_descriptor_v1(
    subject,
    descriptor_cbor,
  )
  let parts = vm_machine_types.CekContextPartsControlV1 {
    redeemer_items: redeemer_control.map_items,
    redeemer: redeemer_summary,
    script_info,
  }
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    stage: 11,
    redeemer_context_control_hash: vm_witness.hash_cek_context_parts_control_v1(
      parts,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    56,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    57,
  )
  let evidence = one_step_evidence(
    work_witness_cbor,
    post,
    vm_machine_types.CekContextFinalizeSpendWitness {
      redeemer_control,
      item_index: 0,
      key: subject,
      descriptor_cbor,
      siblings: [],
    },
  )
  and {
    vm_cek.verify_cek_context_finalize(
      pre,
      evidence.transition,
      evidence.auxiliary,
      native_control,
      context_control,
      0,
      0,
      0,
    ),
    !validation_merkle_v1.verify_membership(
      native_control.resolved_input_count,
      native_control.resolved_item_peaks,
      0,
      script_proof_v1.resolved_context_item_leaf_hash(
        0,
        0,
        subject,
        exact_output_descriptor_fixture(
          2,
          encode_midgard_tx_output(
            MidgardTxOutput {
              ..output,
              datum_cbor: Some(cbor.serialise(builtin.i_data(8))),
            },
          ),
        ),
      ),
      [],
    ),
  }
}

test cek_context_seed_fits_one_step() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let empty_items = cek_data_v1.empty_data_list_summary_v1()
  let empty_pairs = cek_data_v1.empty_data_pair_summary_v1()
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let tx_info = script_context_v1.tx_info_summary_v1(
    False,
    empty_items,
    empty_items,
    empty_items,
    0,
    -1,
    -1,
    observer_summary,
    empty_items,
    cek_data_v1.map_data_summary_v1(empty_pairs),
    empty_pairs,
    native_tx_id_for_version(
      native_tx_version_v1,
      encode_native_tx_body_compact(midgard_transaction_to_compact(tx).body),
    ),
  )
  let redeemer = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(script_info) = script_context_v1.script_purpose_summary_v1(
    1,
    h_28_a,
    #"",
    False,
  )
  let final_context = vm_machine_types.CekFinalContextControlV1 {
    tx_info,
    redeemer,
    script_info,
  }
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(13),
    purpose_kind: 1,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(empty_pairs),
    redeemer_context_control_hash: vm_witness.hash_cek_final_context_control_v1(
      final_context,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    55,
  )
  let context_summary = script_context_v1.script_context_summary_v1(
    tx_info,
    redeemer,
    script_info,
  )
  let context_value = cek_constant_v1.semantic_data_constant_root_v1(
    context_summary,
  )
  let initial_state = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: 0,
    // The seeded context constant is the runtime-only tag-10 term
    // (`cek_proof_v1.hash_context_constant_term_v1`), not the ordinary tag-4
    // constant a source program may contain. `cek_context_execution_successor_is_exact`
    // (validation-machine-v1.ak:15953) deliberately uses the tag-10 form so a
    // canonical source program can never forge the authenticated script
    // context as one of its own constants.
    focus_root: cek_proof_v1.hash_application_term_v1(
      context_control.program_term_root,
      cek_proof_v1.hash_context_constant_term_v1(context_value),
    ),
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: cek_proof_v1.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  let post_work = vm_witness.encode_cek_witness_v1(
    native_control,
    #"",
    0,
    0,
    0,
    cek_machine_v1.hash_state_v1(initial_state),
    context_control.execution_cpu_limit,
    context_control.execution_memory_limit,
    context_control.program_envelope_hash,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 56,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      56,
      post_work,
    ),
  }
  verify_bounded_context_parity(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.CekContextSeedWitness { control: final_context },
    ),
    inline_field_door(),
  )
}

test cek_context_assemble_fits_one_step() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let empty_items = cek_data_v1.empty_data_list_summary_v1()
  let empty_pairs = cek_data_v1.empty_data_pair_summary_v1()
  let redeemer = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(script_info) = script_context_v1.script_purpose_summary_v1(
    1,
    h_28_a,
    #"",
    False,
  )
  let parts = vm_machine_types.CekContextPartsControlV1 {
    redeemer_items: empty_pairs,
    redeemer,
    script_info,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(11),
    purpose_kind: 1,
    observer_summary,
    mint_summary: cek_data_v1.map_data_summary_v1(empty_pairs),
    redeemer_context_control_hash: vm_witness.hash_cek_context_parts_control_v1(
      parts,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    55,
  )
  let tail_fields = script_context_v1.tx_info_tail_fields_summary_v1(
    False,
    observer_summary,
    empty_items,
    cek_data_v1.map_data_summary_v1(empty_pairs),
    empty_pairs,
    pre.transaction_id,
  )
  let assembly = vm_machine_types.CekTxInfoAssemblyControlV1 {
    tail_fields,
    redeemer,
    script_info,
  }
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    stage: 12,
    redeemer_context_control_hash: vm_witness.hash_cek_tx_info_assembly_control_v1(
      assembly,
    ),
  }
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    56,
  )
  verify_bounded_context_parity(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.CekContextAssembleWitness { control: parts },
    ),
    inline_field_door(),
  )
}

test cek_context_tx_info_finalize_fits_one_step() {
  let tx = empty_v1_transaction()
  let native_control = cek_context_native_control_fixture(tx)
  let compact = midgard_transaction_to_compact(tx)
  let transaction_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let empty_items = cek_data_v1.empty_data_list_summary_v1()
  let empty_pairs = cek_data_v1.empty_data_pair_summary_v1()
  let redeemer = cek_data_v1.semantic_data_summary_v1(builtin.i_data(0))
  expect Some(script_info) = script_context_v1.script_purpose_summary_v1(
    1,
    h_28_a,
    #"",
    False,
  )
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let mint = cek_data_v1.map_data_summary_v1(empty_pairs)
  let assembly = vm_machine_types.CekTxInfoAssemblyControlV1 {
    tail_fields: script_context_v1.tx_info_tail_fields_summary_v1(
      False,
      observer_summary,
      empty_items,
      mint,
      empty_pairs,
      transaction_id,
    ),
    redeemer,
    script_info,
  }
  let context_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(12),
    purpose_kind: 1,
    observer_summary,
    mint_summary: mint,
    redeemer_context_control_hash: vm_witness.hash_cek_tx_info_assembly_control_v1(
      assembly,
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    55,
  )
  let final_context = vm_machine_types.CekFinalContextControlV1 {
    tx_info: script_context_v1.tx_info_from_tail_summary_v1(
      False,
      empty_items,
      empty_items,
      empty_items,
      compact.body.fee,
      compact.body.validity_interval_start,
      compact.body.validity_interval_end,
      observer_summary,
      mint,
      assembly.tail_fields,
    ),
    redeemer,
    script_info,
  }
  let next_control = vm_machine_types.CekContextControlV1 {
    ..context_control,
    stage: 13,
    redeemer_context_control_hash: vm_witness.hash_cek_final_context_control_v1(
      final_context,
    ),
  }
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    56,
  )
  verify_bounded_context_parity(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.CekTxInfoFinalizeWitness { control: assembly },
    ),
    inline_field_door(),
  )
}

test maximum_mint_field_terminal_matches_typescript(
  fixture via fuzz.constant(maximum_mint_field_terminal_fixture_v1()),
) {
  verify_maximum_mint_field_terminal_fixture_v1(fixture)
}

test cek_context_maximum_mint_initializes_from_authenticated_frontier() {
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor())
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let mint_frontier = validation_merkle_v1.build_frontier(
    maximum_mint_asset_leaves(0, 128),
  )
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let init_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary,
  }
  let semantic_control = vm_machine_types.CekContextControlV1 {
    ..init_control,
    stage: 8,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(init_control),
    0,
    51,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(semantic_control),
    0,
    52,
  )
  vm_one_step.verify_cek_one_step_v1(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
  )
}

test cek_context_maximum_mint_authenticates_last_asset_membership() {
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor())
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  let mint_frontier = validation_merkle_v1.build_frontier(
    maximum_mint_asset_leaves(0, 128),
  )
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..cek_context_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let semantic_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(8),
    observer_summary,
  }
  let last_asset = bytearray.from_int_big_endian(127, 32)
  let asset_items = cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(last_asset) |> cek_data_v1.semantic_data_summary_v1,
    builtin.i_data(1) |> cek_data_v1.semantic_data_summary_v1,
    cek_data_v1.empty_data_pair_summary_v1(),
  )
  let next_control = vm_machine_types.CekContextControlV1 {
    ..semantic_control,
    mint_cursor: 1,
    current_mint_policy: h_28_a,
    current_mint_assets: asset_items,
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(semantic_control),
    0,
    53,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(next_control),
    0,
    54,
  )
  vm_one_step.verify_cek_one_step_v1(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.CekMintContextItemWitness {
        previous: None,
        mint_index: 127,
        policy_id: h_28_a,
        asset_name: last_asset,
        quantity: 1,
        siblings: maximum_mint_last_asset_siblings(0),
      },
    ),
    inline_field_door(),
  )
}

test cek_active_core_step_replays_exact_machine_transition() {
  let Pair(pre, evidence) = cek_active_core_step_fixture()
  and {
    vm_one_step.verify_one_step_evidence(pre, evidence, inline_field_door()),
    !vm_one_step.verify_one_step_evidence(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        ..evidence,
        transition: vm_machine_types.ValidationOneStepWitnessV1 {
          ..evidence.transition,
          claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
            ..evidence.transition.claimed_successor,
            execution_cpu: 16009,
          },
        },
      },
      inline_field_door(),
    ),
  }
}
