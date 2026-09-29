use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/compact.{verify_forced_tx_proof_source_v1}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_output, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput, MidgardTxOutput, MidgardValue,
  MidgardVersionedScript, NativeCardanoScript, PlutusV3Script, ScriptCredential,
}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/phase_a_native_scripts as vm_phase_a_native_scripts
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/script_sources_early_stages as vm_script_sources_early_stages
use midgard/validation_machine/script_sources_late_stages as vm_script_sources_late_stages
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  differential_invalid_field_type_code, differential_missing_witness_code,
  h_28_a, h_a, h_b, zero,
}
use midgard/validation_machine_tests/fixtures_parity.{
  cek_context_step_kind_fixture, cek_core_step_kind_fixture,
  cek_execution_selection_kind_fixture, cek_finish_kind_fixture,
  cek_kind_fixtures, cek_kind_route, cek_kind_routes_agree,
  effectful_descriptor_specialization_matches_general,
  narrow_route_is_wider_than_generic, stage_eleven_audit_discovery,
  stage_ten_missing_fixture, stage_twelve_finish_fixture,
  value_and_mint_begin_kind_fixture, value_and_mint_finalize_kind_fixture,
  value_and_mint_kind_fixtures, value_and_mint_kind_route,
  value_and_mint_kind_routes_agree, value_and_mint_mint_asset_kind_fixture,
  value_and_mint_mint_finish_kind_fixture,
  value_and_mint_output_asset_kind_fixture,
  value_and_mint_output_descriptor_kind_fixture,
  value_and_mint_output_finish_kind_fixture,
  value_and_mint_replay_asset_kind_fixture,
  value_and_mint_replay_begin_kind_fixture,
  value_and_mint_replay_finish_kind_fixture,
  value_and_mint_replay_input_kind_fixture, verify_descriptor_ten_match_parity,
  verify_descriptor_ten_mismatch_parity,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  authenticated_output_material, empty_v1_transaction, exact_rejection_post,
  inline_field_carriage, inline_field_door, next_phase_a_native_state,
  phase_a_native_scan_witness, phase_a_native_step_fixture,
  script_discovery_step_fixture, script_sources_output_step_fixture,
  script_sources_output_step_fixture_with_resolved_items,
  script_sources_receive_scan_step_fixture, v1_program_envelope,
}
use midgard/validation_machine_tests/fixtures_script_identity.{
  stage_routes_agree, stage_zero_script_sources_control,
  stage_zero_script_sources_state, stage_zero_script_sources_work_witness,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// --- stage 0 --------------------------------------------------------------

test stage_differential_zero_begin_routes_agree() {
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
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    0,
    #"",
  )
  let pre = stage_zero_script_sources_state(tx, control, 59)
  let post = stage_zero_script_sources_state(
    tx,
    vm_machine_types.ScriptSourcesControlV1 {
      ..control,
      source_total_count: 1,
      pending_source_cbor: vm_witness.encode_inline_source_hash_control_v1(
        pending,
      ),
    },
    60,
  )
  // Mutated successor: the opened scan claims two sources where the
  // door-authenticated field-6 preimage carries one.
  let mutated_post = stage_zero_script_sources_state(
    tx,
    vm_machine_types.ScriptSourcesControlV1 {
      ..control,
      source_total_count: 2,
      pending_source_cbor: vm_witness.encode_inline_source_hash_control_v1(
        vm_machine_types.InlineSourceHashControlV1 {
          ..pending,
          source_total_count: 2,
        },
      ),
    },
    60,
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  let auxiliary = vm_machine_types.TransactionFieldChunkWitness {
    field_index: 6,
    item_index: 0,
    carriage,
  }
  and {
    stage_routes_agree(
      vm_script_sources_early_stages.verify_script_sources_stage_zero_begin_semantics_v1(
        pre,
        accepting,
        inline_field_door(),
        6,
        0,
        carriage,
      ),
      vm_script_sources.verify_script_sources_stage_zero_semantics_v1(
        pre,
        accepting,
        auxiliary,
        inline_field_door(),
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_early_stages.verify_script_sources_stage_zero_begin_semantics_v1(
        pre,
        rejecting,
        inline_field_door(),
        6,
        0,
        carriage,
      ),
      vm_script_sources.verify_script_sources_stage_zero_semantics_v1(
        pre,
        rejecting,
        auxiliary,
        inline_field_door(),
      ),
      False,
    ),
  }
}

test stage_differential_zero_finish_routes_agree() {
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
  let item_commitment = bounded_item_v1.from_bytes(6, 0, item_cbor)
  let source_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.inline_source_leaf_hash(
        0,
        3,
        #"03" |> bytearray.concat(payload) |> blake2b_224,
        bytearray.length(item_cbor),
        item_commitment,
      ),
    ],
  )
  let control = stage_zero_script_sources_control(tx, source_frontier, 1, #"")
  let pre = stage_zero_script_sources_state(tx, control, 62)
  let post = stage_zero_script_sources_state(
    tx,
    vm_machine_types.ScriptSourcesControlV1 { ..control, stage: 1 },
    63,
  )
  // Mutated successor: stage 2 instead of the mandated stage 1.
  let mutated_post = stage_zero_script_sources_state(
    tx,
    vm_machine_types.ScriptSourcesControlV1 { ..control, stage: 2 },
    63,
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  and {
    stage_routes_agree(
      vm_script_sources_early_stages.verify_script_sources_stage_zero_finish_semantics_v1(
        pre,
        accepting,
      ),
      vm_script_sources.verify_script_sources_stage_zero_semantics_v1(
        pre,
        accepting,
        vm_machine_types.NoAuxiliaryWitness,
        inline_field_door(),
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_early_stages.verify_script_sources_stage_zero_finish_semantics_v1(
        pre,
        rejecting,
      ),
      vm_script_sources.verify_script_sources_stage_zero_semantics_v1(
        pre,
        rejecting,
        vm_machine_types.NoAuxiliaryWitness,
        inline_field_door(),
      ),
      False,
    ),
  }
}

// --- stage 7 --------------------------------------------------------------

test stage_differential_seven_observer_routes_agree() {
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
  let observer_scan = vm_machine_types.ObserverPurposeScanControlV1 {
    total_count: 1,
    seen: 1,
    previous_hash: h_28_a,
  }
  let Pair(post, _) = script_sources_output_step_fixture_with_resolved_items(
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
    observer_scan,
    19,
  )
  // Mutated successor: the observer purpose was never appended to the purpose
  // frontier, but the scan claims it was seen.
  let Pair(mutated_post, _) = script_sources_output_step_fixture_with_resolved_items(
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
    observer_scan,
    19,
  )
  let carriage = inline_field_carriage([h_28_a])
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  let auxiliary = vm_machine_types.TransactionFieldChunkWitness {
    field_index: 3,
    item_index: 0,
    carriage,
  }
  and {
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_semantics_v1(
        pre,
        accepting,
        inline_field_door(),
        3,
        0,
        carriage,
      ),
      vm_script_sources.verify_script_sources_stage_seven_semantics_v1(
        pre,
        accepting,
        auxiliary,
        inline_field_door(),
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_semantics_v1(
        pre,
        rejecting,
        inline_field_door(),
        3,
        0,
        carriage,
      ),
      vm_script_sources.verify_script_sources_stage_seven_semantics_v1(
        pre,
        rejecting,
        auxiliary,
        inline_field_door(),
      ),
      False,
    ),
  }
}

test stage_differential_seven_receive_routes_agree() {
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
  let Pair(post, _) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    1,
    output_frontier,
    selected_receive_scan,
    16,
  )
  // Mutated successor: the output cursor did not advance.
  let Pair(mutated_post, _) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    selected_receive_scan,
    16,
  )
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  let auxiliary = vm_machine_types.ScriptPurposeScanWitness {
    purpose_kind: 3,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: h_28_a,
    siblings: [],
  }
  and {
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_seven_receive_semantics_v1(
        pre,
        accepting,
        3,
        0,
        h_28_a,
        h_28_a,
        [],
      ),
      vm_script_sources.verify_script_sources_stage_seven_semantics_v1(
        pre,
        accepting,
        auxiliary,
        inline_field_door(),
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_seven_receive_semantics_v1(
        pre,
        rejecting,
        3,
        0,
        h_28_a,
        h_28_a,
        [],
      ),
      vm_script_sources.verify_script_sources_stage_seven_semantics_v1(
        pre,
        rejecting,
        auxiliary,
        inline_field_door(),
      ),
      False,
    ),
  }
}

// --- stage 8 --------------------------------------------------------------

test stage_differential_eight_purpose_routes_agree() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, h_28_a, #"0102")],
  )
  let empty_discovery = vm_witness.empty_script_discovery_control()
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    empty_discovery,
    19,
  )
  let selected = vm_machine_types.ScriptDiscoveryControlV1 {
    ..empty_discovery,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: h_28_a,
    current_subject: #"0102",
  }
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    selected,
    20,
  )
  // Mutated successor: the selected purpose subject does not match the
  // authenticated purpose leaf.
  let Pair(mutated_post, _) = script_discovery_step_fixture(
    tx,
    9,
    empty_frontier,
    empty_frontier,
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..selected,
      current_subject: #"0103",
    },
    20,
  )
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  let auxiliary = vm_machine_types.ScriptPurposeScanWitness {
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: #"0102",
    siblings: [],
  }
  and {
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_eight_purpose_semantics_v1(
        pre,
        accepting,
        0,
        0,
        h_28_a,
        #"0102",
        [],
      ),
      vm_script_sources.verify_script_sources_stage_eight_semantics_v1(
        pre,
        accepting,
        auxiliary,
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_eight_purpose_semantics_v1(
        pre,
        rejecting,
        0,
        0,
        h_28_a,
        #"0102",
        [],
      ),
      vm_script_sources.verify_script_sources_stage_eight_semantics_v1(
        pre,
        rejecting,
        auxiliary,
      ),
      False,
    ),
  }
}

test stage_differential_eight_finish_routes_agree() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let empty_discovery = vm_witness.empty_script_discovery_control()
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    19,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    11,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    20,
  )
  // Mutated successor: stage 12 instead of the mandated stage 11.
  let Pair(mutated_post, _) = script_discovery_step_fixture(
    tx,
    12,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    20,
  )
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  and {
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_eight_finish_semantics_v1(
        pre,
        accepting,
      ),
      vm_script_sources.verify_script_sources_stage_eight_semantics_v1(
        pre,
        accepting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_eight_finish_semantics_v1(
        pre,
        rejecting,
      ),
      vm_script_sources.verify_script_sources_stage_eight_semantics_v1(
        pre,
        rejecting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

// --- stage 9 --------------------------------------------------------------

test stage_differential_nine_missing_routes_agree() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    current_purpose_kind: 3,
    current_purpose_index: 0,
    current_script_hash: h_28_a,
    current_subject: h_28_a,
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
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_missing_witness_code,
    ),
  }
  // Mutated successor: a rejection carrying the wrong code.
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_invalid_field_type_code,
    ),
  }
  and {
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_nine_missing_semantics_v1(
        pre,
        accepting,
      ),
      vm_script_sources.verify_script_sources_stage_nine_semantics_v1(
        pre,
        accepting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_nine_missing_semantics_v1(
        pre,
        rejecting,
      ),
      vm_script_sources.verify_script_sources_stage_nine_semantics_v1(
        pre,
        rejecting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

test stage_differential_ten_missing_routes_agree() {
  let Pair(pre, work_witness_cbor) = stage_ten_missing_fixture(#"")
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_missing_witness_code,
    ),
  }
  // Mutated successor: a rejection carrying the wrong code.
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_invalid_field_type_code,
    ),
  }
  and {
    !verify_descriptor_ten_match_parity(
      pre,
      accepting,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    !verify_descriptor_ten_mismatch_parity(
      pre,
      accepting,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_ten_missing_semantics_v1(
        pre,
        accepting,
      ),
      vm_script_sources.verify_script_sources_stage_ten_semantics_v1(
        pre,
        accepting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_ten_missing_semantics_v1(
        pre,
        rejecting,
      ),
      vm_script_sources.verify_script_sources_stage_ten_semantics_v1(
        pre,
        rejecting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

test stage_differential_eleven_finish_routes_agree() {
  let tx = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: v1_program_envelope(),
  }
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let source_leaf = script_proof_v1.source_leaf_hash(0, #"00", script)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"0102")],
  )
  let execution_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let discovery = stage_eleven_audit_discovery(execution_frontier.peaks, 1, #"")
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    11,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    31,
  )
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    32,
  )
  // Mutated successor: the audit never leaves stage 11.
  let Pair(mutated_post, _) = script_discovery_step_fixture(
    tx,
    11,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    discovery,
    32,
  )
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  and {
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_eleven_finish_semantics_v1(
        pre,
        accepting,
      ),
      vm_script_sources.verify_script_sources_stage_eleven_semantics_v1(
        pre,
        accepting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_eleven_finish_semantics_v1(
        pre,
        rejecting,
      ),
      vm_script_sources.verify_script_sources_stage_eleven_semantics_v1(
        pre,
        rejecting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

test stage_differential_twelve_finish_routes_agree() {
  let Pair(pre, Pair(work_witness_cbor, Pair(post, mutated_post))) = stage_twelve_finish_fixture(
    #"",
  )
  let accepting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let rejecting = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: mutated_post,
  }
  and {
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_twelve_finish_semantics_v1(
        pre,
        accepting,
      ),
      vm_script_sources.verify_script_sources_stage_twelve_semantics_v1(
        pre,
        accepting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      True,
    ),
    stage_routes_agree(
      vm_script_sources_late_stages.verify_script_sources_stage_twelve_finish_semantics_v1(
        pre,
        rejecting,
      ),
      vm_script_sources.verify_script_sources_stage_twelve_semantics_v1(
        pre,
        rejecting,
        vm_machine_types.NoAuxiliaryWitness,
      ),
      False,
    ),
  }
}

/// Latent hazard, stage 8. Unreachable: `reset_script_discovery_current` is the
/// only door into stage 8 and it clears the pending hash, so no produced
/// stage-8 state carries one. See the section note above.
test stage_eight_finish_pending_redeemer_hash_divergence_is_unreachable() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let empty_discovery = vm_witness.empty_script_discovery_control()
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..empty_discovery,
      redeemer_item_control_hash: h_a,
    },
    19,
  )
  // `reset_script_discovery_current` clears the pending hash, so the stage-11
  // successor is the same one the clean fixture commits to.
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    11,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    empty_discovery,
    20,
  )
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  narrow_route_is_wider_than_generic(
    vm_script_sources_late_stages.verify_script_sources_stage_eight_finish_semantics_v1(
      pre,
      transition,
    ),
    vm_script_sources.verify_script_sources_stage_eight_semantics_v1(
      pre,
      transition,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  )
}

/// Latent hazard, stage 10. Unreachable: the only writers of the pending hash
/// (validation-machine-v1.ak:11142, :11235) are guarded by
/// `redeemer_cursor != redeemer_count` and leave the cursor alone, while this
/// wrapper's sub-case requires `redeemer_cursor == redeemer_count`. See the
/// section note above.
test stage_ten_missing_pending_redeemer_hash_divergence_is_unreachable() {
  let Pair(pre, work_witness_cbor) = stage_ten_missing_fixture(h_a)
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: exact_rejection_post(
      pre,
      differential_missing_witness_code,
    ),
  }
  narrow_route_is_wider_than_generic(
    vm_script_sources_late_stages.verify_script_sources_stage_ten_missing_semantics_v1(
      pre,
      transition,
    ),
    vm_script_sources.verify_script_sources_stage_ten_semantics_v1(
      pre,
      transition,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  )
}

/// Latent hazard, stage 12. Unreachable for the same reason as stage 10
/// (writers at validation-machine-v1.ak:11693, :11756), plus stage 11 -- the
/// only door into stage 12 -- may not carry a pending hash at all.
/// See the section note above.
test stage_twelve_finish_pending_redeemer_hash_divergence_is_unreachable() {
  let Pair(pre, Pair(work_witness_cbor, Pair(post, _))) = stage_twelve_finish_fixture(
    h_a,
  )
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  narrow_route_is_wider_than_generic(
    vm_script_sources_late_stages.verify_script_sources_stage_twelve_finish_semantics_v1(
      pre,
      transition,
    ),
    vm_script_sources.verify_script_sources_stage_twelve_semantics_v1(
      pre,
      transition,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  )
}

test cek_finish_route_agrees_with_the_aggregate() {
  cek_kind_routes_agree(cek_finish_kind_fixture())
}

test cek_execution_selection_route_agrees_with_the_aggregate() {
  cek_kind_routes_agree(cek_execution_selection_kind_fixture())
}

test cek_context_step_route_agrees_with_the_aggregate() {
  cek_kind_routes_agree(cek_context_step_kind_fixture())
}

test cek_core_step_route_agrees_with_the_aggregate() {
  cek_kind_routes_agree(cek_core_step_kind_fixture())
}

/// Every cek wrapper refuses every other kind's honest step. The wrapper is
/// handed its OWN kind's auxiliary shape (what its validator would rebuild
/// from the redeemer) over the other kind's pre-state and transition, so the
/// refusal can only come from the control discriminator, never from an
/// auxiliary shape mismatch.
test cek_kinds_partition_the_cek_step_space() {
  let fixtures = cek_kind_fixtures()
  list.all(
    fixtures,
    fn(own) {
      list.all(
        fixtures,
        fn(other) {
          or {
            own.kind == other.kind,
            !cek_kind_route(
              own.kind,
              other.pre,
              other.evidence.transition,
              own.evidence.auxiliary,
            ),
          }
        },
      )
    },
  )
}

test value_and_mint_begin_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_begin_kind_fixture())
}

test value_and_mint_replay_begin_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_replay_begin_kind_fixture())
}

test value_and_mint_replay_input_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_replay_input_kind_fixture())
}

test value_and_mint_replay_asset_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_replay_asset_kind_fixture())
}

test value_and_mint_replay_finish_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_replay_finish_kind_fixture())
}

test value_and_mint_output_descriptor_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(
    value_and_mint_output_descriptor_kind_fixture(),
  )
}

test value_and_mint_output_asset_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_output_asset_kind_fixture())
}

test value_and_mint_output_finish_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_output_finish_kind_fixture())
}

test value_and_mint_mint_asset_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_mint_asset_kind_fixture())
}

test value_and_mint_mint_finish_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_mint_finish_kind_fixture())
}

test value_and_mint_finalize_route_agrees_with_the_aggregate() {
  value_and_mint_kind_routes_agree(value_and_mint_finalize_kind_fixture())
}

/// Every ValueAndMint wrapper refuses every other kind's honest step (same
/// construction as the cek partition control above: own auxiliary shape,
/// other kind's pre-state and transition).
test value_and_mint_kinds_partition_the_value_and_mint_step_space() {
  let fixtures = value_and_mint_kind_fixtures()
  list.all(
    fixtures,
    fn(own) {
      list.all(
        fixtures,
        fn(other) {
          or {
            own.kind == other.kind,
            !value_and_mint_kind_route(
              own.kind,
              other.pre,
              other.evidence.transition,
              own.evidence.auxiliary,
            ),
          }
        },
      )
    },
  )
}

// Payload pruning preserves the full binding on reachable positive-stage
// controls; the source/context and canonical carried bytes remain bound.
test phase_a_payload_binding_matches_full_binding(
  stage via fuzz.int_between(3, 6),
) {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: #"8200581c11111111111111111111111111111111111111111111111111111111",
        },
      ],
    },
  }
  let Pair(pre, _) = phase_a_native_step_fixture(tx, h_a)
  let work_witness_cbor = phase_a_native_scan_witness(
    tx,
    stage,
    1,
    0,
    0,
    40,
    zero,
    6,
    #"",
    0,
    1,
    -1,
  )
  let control = vm_witness.phase_a_native_control_from_witness(
    work_witness_cbor,
  )
  let witness = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: pre,
  }
  let Pair(verified, witness_set) = verify_forced_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    vm_witness.phase_a_native_payload_control_is_bound(
      pre,
      witness,
      control,
      3,
      6,
    ),
    vm_witness.phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    !vm_witness.phase_a_native_payload_control_is_bound(
      validation_trace_v1.ValidationMachineStateV1 {
        ..pre,
        transaction_commitment: h_b,
      },
      witness,
      control,
      3,
      6,
    ),
    !vm_witness.phase_a_native_payload_control_is_bound(
      validation_trace_v1.ValidationMachineStateV1 {
        ..pre,
        validation_context_hash: h_b,
      },
      witness,
      control,
      3,
      6,
    ),
    !vm_witness.phase_a_native_payload_control_is_bound(
      pre,
      witness,
      vm_machine_types.PhaseANativeScriptsControlV1 {
        ..control,
        continuation_cbor: #"00",
      },
      3,
      6,
    ),
    !vm_witness.phase_a_native_payload_control_is_bound(
      pre,
      witness,
      vm_machine_types.PhaseANativeScriptsControlV1 { ..control, result: 1 },
      3,
      6,
    ),
    !vm_witness.phase_a_native_payload_control_is_bound(
      pre,
      witness,
      control,
      7,
      8,
    ),
  }
}

/// The split proves the original item predicate over honest and substituted
/// successor roots and coordinates; neither yield alone supplies binding.
test phase_a_item_split_equals_original(choice via fuzz.int_between(0, 7)) {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"82041864",
  }
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      validity_interval_start: 100,
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_native_step_fixture(tx, h_a)
  let item_cbor = encode_midgard_versioned_script(script)
  let next_work = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    3,
    #"",
    0,
    0,
    -1,
  )
  let expected_post = next_phase_a_native_state(pre, next_work)
  let post = if choice % 2 == 0 {
    expected_post
  } else {
    validation_trace_v1.ValidationMachineStateV1 {
      ..expected_post,
      work_root: h_a,
    }
  }
  let witness = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let field_index = if choice < 4 {
    6
  } else {
    5
  }
  let item_index = if choice < 6 {
    0
  } else {
    1
  }
  let carriage = inline_field_carriage([item_cbor])
  let original = vm_phase_a_native_scripts.verify_phase_a_native_item_semantics_v1(
    pre,
    witness,
    inline_field_door(),
    field_index,
    item_index,
    carriage,
  )
  let split = and {
    vm_phase_a_native_scripts.verify_phase_a_native_item_dispatch_v1(
      pre,
      witness,
      field_index,
      item_index,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_item_native_payload_v1(
      pre,
      witness,
      inline_field_door(),
      carriage,
    ) || vm_phase_a_native_scripts.verify_phase_a_native_item_foreign_payload_v1(
      pre,
      witness,
      inline_field_door(),
      carriage,
    ),
  }
  and {
    original == split,
    original == ( choice == 0 || choice == 2 ),
  }
}

/// Structural routing permits bounded self steps and the native continuation
/// round trip; semantic predicates still authenticate the exact controls.
test structural_bounded_phase_routes(choice via fuzz.int_between(0, 7)) {
  let Pair(base, work_witness_cbor) = phase_a_native_step_fixture(
    empty_v1_transaction(),
    h_a,
  )
  let Pair(before, after) = when choice is {
    0 -> Pair(validation_trace_v1.InputSets, validation_trace_v1.InputSets)
    1 -> Pair(validation_trace_v1.Signatures, validation_trace_v1.Signatures)
    2 ->
      Pair(
        validation_trace_v1.PhaseANativeScripts,
        validation_trace_v1.PhaseANativeScripts,
      )
    3 ->
      Pair(
        validation_trace_v1.NativeScripts,
        validation_trace_v1.PhaseANativeScripts,
      )
    4 ->
      Pair(
        validation_trace_v1.PhaseANativeScripts,
        validation_trace_v1.NativeScripts,
      )
    5 -> Pair(validation_trace_v1.InputSets, validation_trace_v1.NativeScripts)
    6 ->
      Pair(validation_trace_v1.Signatures, validation_trace_v1.ScriptIntegrity)
    _ -> Pair(validation_trace_v1.PhaseANativeScripts, validation_trace_v1.Cek)
  }
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..base,
    phase: before,
    work_root: validation_trace_v1.hash_work_witness(
      before,
      base.program_counter,
      work_witness_cbor,
    ),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: after,
    program_counter: pre.program_counter + 1,
  }
  vm_shared.structural_transition_is_valid(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
  ) == ( choice < 5 )
}

test encode_resolve_inputs_initial_equals_generic_none(
  args via fuzz.tuple3(
    fuzz.list_between(fuzz.bytearray(), 4, 4),
    fuzz.list_between(fuzz.bytearray_fixed(32), 4, 4),
    fuzz.tuple(fuzz.int_at_least(0), fuzz.int_at_least(0)),
  ),
) {
  let (sources, hashes, counters) = args
  expect [compact, witnesses, lengths, context] = sources
  expect [accumulator, remaining, frontier, schedule] = hashes
  let (cursor, signers) = counters
  vm_witness.encode_resolve_inputs_initial_witness(
    compact,
    witnesses,
    lengths,
    context,
    cursor,
    accumulator,
    remaining,
    signers,
    frontier,
    schedule,
  ) == vm_witness.encode_resolve_inputs_witness(
    compact,
    witnesses,
    lengths,
    context,
    cursor,
    accumulator,
    remaining,
    signers,
    frontier,
    None,
    schedule,
  )
}

test effectful_descriptor_specialization_preserves_plutus_transitions() {
  effectful_descriptor_specialization_matches_general(3)
}

test effectful_descriptor_specialization_preserves_midgard_transitions() {
  effectful_descriptor_specialization_matches_general(128)
}
