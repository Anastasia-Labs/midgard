use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet, MidgardVersionedScript,
  PlutusV3Script,
}
use midgard/native_tx_field_access_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/script_sources_early_stages as vm_script_sources_early_stages
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_a, h_b, h_c, validation_context_cbor, zero,
}
use midgard/validation_machine_tests/fixtures_cek.{
  maximum_program_envelope_selection_is_exact,
}
use midgard/validation_machine_tests/fixtures_ledger.{zero_filler_bytes}
use midgard/validation_machine_tests/fixtures_phase_a.{
  canonical_step_fixture, empty_v1_transaction, inline_field_carriage,
  inline_field_door, one_step_evidence, script_discovery_step_fixture,
  submitted_source, v1_program_envelope,
}
use midgard/validation_machine_tests/fixtures_script_identity.{
  crossing_inline_source_hash_fixture, stage_zero_hash_block_checks_binding,
  stage_zero_script_sources_control, stage_zero_script_sources_state,
  stage_zero_script_sources_work_witness,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

/// The on-wire size of one `CanonicalDecode` chunk instruction, at both ends of
/// §8's carriage ladder.
///
/// The retired counted evidence was bounded by *proof* size — a 14-peak
/// collection frontier, 14 siblings, a chunk frontier and one
/// `bounded_item_v1.chunk_bytes` window — and this row pinned that worst case.
/// §8 replaced all of it with a `FieldCarriageV1`, so the bound now has two
/// shapes and both are pinned:
///
///   * **tier 1** is the only tier whose evidence grows with the field, and
///     §8.3 caps a tier-1 plan at `max_tier1_redeemer_preimage_bytes`;
///   * **tier 3** names one certificate reference input and at most
///     `max_tier3_chunk_count` chunk reference inputs, so it is O(1) in the
///     field's size — which is what makes the measured 16,225-byte field-2
///     maximum reachable by a machine step at all (#593 Ruling 1, option c).
test maximum_general_field_bounded_chunk_instruction_evidence_is_bounded() {
  let Pair(_pre, base_evidence) = canonical_step_fixture()
  let maximum_tier1_preimage = zero_filler_bytes(
    native_tx_field_access_v1.max_tier1_redeemer_preimage_bytes,
  )
  let maximum_tier3_chunk_indices = [1, 2, 3]
  let tier1_evidence = vm_machine_types.ValidationOneStepEvidenceV1 {
    transition: base_evidence.transition,
    auxiliary: vm_machine_types.TransactionFieldChunkWitness {
      field_index: 8,
      item_index: 16_382,
      carriage: native_tx_field_access_v1.Inline {
        preimage: maximum_tier1_preimage,
      },
    },
  }
  let tier3_evidence = vm_machine_types.ValidationOneStepEvidenceV1 {
    transition: base_evidence.transition,
    auxiliary: vm_machine_types.TransactionFieldChunkWitness {
      field_index: 8,
      item_index: 16_382,
      carriage: native_tx_field_access_v1.Certified {
        cert_ref_input_index: 0,
        chunk_ref_input_indices: maximum_tier3_chunk_indices,
      },
    },
  }
  and {
    bytearray.length(maximum_tier1_preimage) == native_tx_field_access_v1.max_tier1_redeemer_preimage_bytes,
    list.length(maximum_tier3_chunk_indices) == native_tx_field_access_v1.max_tier3_chunk_count,
    bytearray.length(cbor.serialise(tier1_evidence)) < 16_384,
    // The whole point of the ladder: tier-3 evidence does not grow with the
    // field, so it is smaller than the tier-1 form by three orders of magnitude.
    bytearray.length(cbor.serialise(tier3_evidence)) < 1_024,
  }
}

test maximum_script_program_instruction_evidence_is_bounded() {
  let tx = empty_v1_transaction()
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let Pair(base_pre, _base_work_witness_cbor) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    empty_frontier,
    vm_witness.empty_script_discovery_control(),
    52,
  )
  let proof_source = submitted_source(tx)
  let dense_frontier_four = validation_merkle_v1.build_frontier(
    list.repeat(h_a, 15),
  )
  let dense_frontier_six = validation_merkle_v1.build_frontier(
    list.repeat(h_b, 63),
  )
  let dense_frontier_seven = validation_merkle_v1.build_frontier(
    list.repeat(h_c, 127),
  )
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    purpose_cursor: 15,
    source_cursor: 15,
    redeemer_cursor: 15,
    current_purpose_kind: 3,
    current_purpose_index: 15,
    current_script_hash: h_28_a,
    current_subject: bytearray.from_int_big_endian(0, 36),
    matched_source_index: 15,
    matched_language_tag: 128,
    matched_source_leaf: h_a,
    used_inline_bitmap: 65_535,
    used_redeemer_bitmap: 65_535,
    execution_count: 15,
    execution_peaks: dense_frontier_four.peaks,
    redeemer_item_control_hash: #"",
  }
  let control = vm_machine_types.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 127,
    resolved_inputs_accumulator: h_a,
    resolved_item_peaks: dense_frontier_seven.peaks,
    signer_count: 63,
    signer_frontier_commitment: h_b,
    stage: 8,
    source_count: 15,
    source_peaks: dense_frontier_four.peaks,
    redeemer_count: 15,
    redeemer_peaks: dense_frontier_four.peaks,
    replay_cursor: 127,
    replay_accumulator: h_b,
    replay_remaining_schedule_hash: h_c,
    spend_index: 63,
    purpose_count: 15,
    purpose_peaks: dense_frontier_four.peaks,
    output_cursor: 63,
    output_count: 63,
    output_peaks: dense_frontier_six.peaks,
    output_total_count: 63,
    receive_scan: vm_witness.empty_receive_purpose_scan_control(),
    source_total_count: 15,
    redeemer_total_count: 15,
    observer_scan: vm_witness.empty_observer_purpose_scan_control(),
    discovery,
    output_proof: None,
    pending_source_cbor: #"",
    mint_fold: vm_witness.empty_mint_fold_control(),
    resolution_schedule_hash: h_c,
  }
  let work_witness_cbor = vm_witness.encode_script_sources_discovery_witness(
    control,
    8,
    discovery,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..base_pre,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      base_pre.program_counter,
      work_witness_cbor,
    ),
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 53,
    work_root: h_b,
  }
  let evidence = one_step_evidence(
    work_witness_cbor,
    post,
    vm_machine_types.ScriptSourceScanWitness {
      source_index: 15,
      origin_kind: 1,
      source_key: bytearray.from_int_big_endian(0, 36),
      script_language_tag: 3,
      script_hash: h_28_a,
      script_total_length: 50,
      script_item_commitment: h_a,
      siblings: list.repeat(zero, 4),
    },
  )
  and {
    bytearray.length(cbor.serialise(evidence)) < 16_384,
    maximum_program_envelope_selection_is_exact(),
  }
}

test script_sources_stage_zero_begin_initializes_an_authenticated_identity_trace() {
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
  let successor_control = vm_machine_types.ScriptSourcesControlV1 {
    ..control,
    source_total_count: 1,
    pending_source_cbor: vm_witness.encode_inline_source_hash_control_v1(
      pending,
    ),
  }
  let pre = stage_zero_script_sources_state(tx, control, 59)
  let post = stage_zero_script_sources_state(tx, successor_control, 60)
  vm_script_sources_early_stages.verify_script_sources_stage_zero_begin_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: stage_zero_script_sources_work_witness(control),
      claimed_successor: post,
    },
    inline_field_door(),
    6,
    0,
    carriage,
  )
}

test script_sources_stage_zero_finish_advances_a_complete_source_frontier() {
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
  let successor_control = vm_machine_types.ScriptSourcesControlV1 {
    ..control,
    stage: 1,
  }
  let pre = stage_zero_script_sources_state(tx, control, 62)
  let post = stage_zero_script_sources_state(tx, successor_control, 63)
  vm_script_sources_early_stages.verify_script_sources_stage_zero_finish_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: stage_zero_script_sources_work_witness(control),
      claimed_successor: post,
    },
  )
}

test script_sources_stage_zero_accepts_an_authenticated_hash_block() {
  stage_zero_hash_block_checks_binding(False)
}

test script_sources_stage_zero_hash_block_refuses_a_substituted_source() fail {
  stage_zero_hash_block_checks_binding(True)
}

test script_sources_stage_zero_advances_an_inline_hash_round() {
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
  let hash_control = blake2b_224_trace_v1.initial_control_v1(
    bytearray.length(payload) + 1,
  )
  expect Some(round_control) = blake2b_224_trace_v1.step_v1(
    hash_control,
    Some(#"03" |> bytearray.concat(payload)),
  )
  expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
    round_control,
    None,
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
    hash_control: round_control,
  }
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    1,
    vm_witness.encode_inline_source_hash_control_v1(pending),
  )
  let successor_control = vm_machine_types.ScriptSourcesControlV1 {
    ..control,
    pending_source_cbor: vm_witness.encode_inline_source_hash_control_v1(
      vm_machine_types.InlineSourceHashControlV1 {
        ..pending,
        hash_control: next_hash_control,
      },
    ),
  }
  let pre = stage_zero_script_sources_state(tx, control, 60)
  let post = stage_zero_script_sources_state(tx, successor_control, 61)
  vm_script_sources_early_stages.verify_script_sources_stage_zero_hash_advance_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: stage_zero_script_sources_work_witness(control),
      claimed_successor: post,
    },
  )
}

test script_sources_stage_zero_commits_the_terminal_script_identity() {
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
  let script_hash = #"03" |> bytearray.concat(payload) |> blake2b_224
  let terminal_hash_control = blake2b_224_trace_v1.Blake2b224TraceControlV1 {
    version: 1,
    stage: blake2b_224_trace_v1.stage_terminal,
    cursor: bytearray.length(payload) + 1,
    total_length: bytearray.length(payload) + 1,
    chaining_value: script_hash
      |> bytearray.concat(builtin.replicate_byte(36, 0)),
    active_block: #"",
    active_block_length: 0,
    working_value: #"",
    round: 0,
  }
  let pending = vm_machine_types.InlineSourceHashControlV1 {
    version: 1,
    source_index: 0,
    source_total_count: 1,
    language_tag: 3,
    payload_offset: bytearray.length(item_cbor) - bytearray.length(payload),
    payload_length: bytearray.length(payload),
    item_length: bytearray.length(item_cbor),
    item_commitment,
    hash_control: terminal_hash_control,
  }
  let control = stage_zero_script_sources_control(
    tx,
    validation_merkle_v1.build_frontier([]),
    1,
    vm_witness.encode_inline_source_hash_control_v1(pending),
  )
  let successor_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.inline_source_leaf_hash(
        0,
        3,
        script_hash,
        bytearray.length(item_cbor),
        item_commitment,
      ),
    ],
  )
  let successor_control = stage_zero_script_sources_control(
    tx,
    successor_frontier,
    1,
    #"",
  )
  let pre = stage_zero_script_sources_state(tx, control, 61)
  let post = stage_zero_script_sources_state(tx, successor_control, 62)
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_zero_script_sources_work_witness(control),
    claimed_successor: post,
  }
  and {
    vm_script_sources.verify_script_sources_stage_zero_semantics_v1(
      pre,
      transition,
      vm_machine_types.NoAuxiliaryWitness,
      inline_field_door(),
    ),
    vm_script_sources_early_stages.verify_script_sources_stage_zero_hash_terminal_semantics_v1(
      pre,
      transition,
    ),
  }
}

test inline_source_hash_block_authenticates_a_chunk_boundary() {
  let Pair(pending, Pair(current, next)) = crossing_inline_source_hash_fixture()
  vm_script_sources_early_stages.inline_source_hash_block_v1(
    pending,
    current,
    Some(next),
  ) == Some(builtin.replicate_byte(128, 171))
}

test inline_source_hash_block_fails_closed_when_boundary_data_is_withheld() {
  let Pair(pending, Pair(current, _next)) = crossing_inline_source_hash_fixture()
  vm_script_sources_early_stages.inline_source_hash_block_v1(
    pending,
    current,
    None,
  ) == None
}
