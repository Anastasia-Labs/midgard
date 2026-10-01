use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_output, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_mint_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxOutput,
  MidgardValue, MidgardVersionedScript, PubKeyCredential, ScriptCredential,
  native_tx_version_v1,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/script_sources_late_stages as vm_script_sources_late_stages
use midgard/validation_machine/script_sources_middle_semantics as vm_middle_semantics
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_a, maximum_input_schedule_last_key,
  maximum_input_schedule_last_value, maximum_mint_policy_item_cbor,
  network_mismatch_rejection_code, validation_context_cbor,
}
use midgard/validation_machine_tests/fixture_types.{
  ResolutionScheduleTerminalVectorV1,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  authenticated_output_material, empty_v1_transaction, exact_rejection_post,
  forced_pending_state, inline_field_carriage, inline_field_door,
  maximum_mint_preimage_cbor, script_discovery_step_fixture,
  script_discovery_step_fixture_with_output_frontier,
  script_sources_active_output_proof_fixture, script_sources_mint_fold_fixture,
  script_sources_output_step_fixture,
  script_sources_output_step_fixture_with_resolved_items,
  script_sources_receive_scan_step_fixture, script_sources_step_fixture,
  signer_frontier_for_tx, submitted_source,
}
use midgard/validation_machine_tests/fixtures_script_identity.{
  stage_zero_script_sources_control, stage_zero_script_sources_work_witness,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub fn maximum_spend_input_schedule_terminal_vector() -> ResolutionScheduleTerminalVectorV1 {
  ResolutionScheduleTerminalVectorV1 {
    original_schedule_hash: #"b1276de655cef9d7a3bc33abb49024dd36462f2cd2486dd25d9f7347563eab35",
    terminal_accumulator: #"fb2154b7f046126c8cda87674c2822c3dad1ac294531845e6e6b49c0035e4858",
    terminal_frontier_commitment: #"8d0f7813e70f9bf63f84b78365a38e5c00efb807ac4f5ed16d21f0eff4b5e5b7",
    penultimate_accumulator: #"7f4f9eb3bfca6b31c4c6781cc36b58f0305c7cafbfbbe4dbfef9ad2370f8cbdc",
    penultimate_remaining_schedule_hash: #"6d5383cff6142a4e20298cf82b71837886eeb853245d068a8cf43a540e1d2b3d",
    penultimate_spend_index: 433,
    penultimate_resolved_item_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"490d3ccf112900b8af68cdf9873ea682bc41f5e3a18ce80e8cbaad97c7afe1f0",
      },
      validation_merkle_v1.FrontierPeak {
        height: 4,
        hash: #"d01cd11a1af0cc1270e6f3f3a7dfff15c52f7710c1c12bdd3245c068f328c53b",
      },
      validation_merkle_v1.FrontierPeak {
        height: 5,
        hash: #"4bd02c803151903ed7bb3a09e8c7823ba0391d78edfc6dacaf66badc3950d9bf",
      },
      validation_merkle_v1.FrontierPeak {
        height: 7,
        hash: #"66b7b616caa9588056af4d6191ecd23ba077e9c7644e70bb2666fb08f759a0a6",
      },
      validation_merkle_v1.FrontierPeak {
        height: 8,
        hash: #"998486423647076a38bdd62961e92f2c404cd7c9f3ce8f862890f806e82eb543",
      },
    ],
    last_source_kind: 0,
    last_key: maximum_input_schedule_last_key,
    last_value: maximum_input_schedule_last_value,
  }
}

pub fn maximum_reference_input_schedule_terminal_vector() -> ResolutionScheduleTerminalVectorV1 {
  ResolutionScheduleTerminalVectorV1 {
    original_schedule_hash: #"320bc3d5140983c35d48db52cbf4e722de03b8ea618ef56b6a7f8b6fc6162e56",
    terminal_accumulator: #"e0254c2da094b13f9857dd7f442bd346f74ccf0f4a1337bd1328983d80156421",
    terminal_frontier_commitment: #"b4757e20d40f58687e5421545c69f0399d6de0026a8e9728ec6e9bb13b510a46",
    penultimate_accumulator: #"cef5b3dc9392a0a6f359309a1b6ef88bc48196c7d85e4b2ab2b539a778fec226",
    penultimate_remaining_schedule_hash: #"11e90c7b9d22faa10a757fe7cbcfc12f5784cb6c546408583d36ce0dd5d49329",
    penultimate_spend_index: 1,
    penultimate_resolved_item_peaks: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: #"c830521b7ad8c959ae55b31633e207388fae52161373666e11f0cfb8f2636258",
      },
      validation_merkle_v1.FrontierPeak {
        height: 4,
        hash: #"fb7125a9b42801c60668ae85fbb63c9688ae6f1db51b66e808f887beb33ad939",
      },
      validation_merkle_v1.FrontierPeak {
        height: 5,
        hash: #"d434d9b86a8a039067633f60db0b7028cd8c0e20d7738f136c71ba4ebb17f600",
      },
      validation_merkle_v1.FrontierPeak {
        height: 7,
        hash: #"a0b8592e98857754eb7c710a2b73418391aef2bda4fcbd1e1bef9db50945bed7",
      },
      validation_merkle_v1.FrontierPeak {
        height: 8,
        hash: #"7d334c4861036c40c26095c4cb1d9db17b736a5a4108beb136aa4988ce854fe7",
      },
    ],
    last_source_kind: 1,
    last_key: maximum_input_schedule_last_key,
    last_value: maximum_input_schedule_last_value,
  }
}

pub fn resolution_schedule_terminal_control(
  vector: ResolutionScheduleTerminalVectorV1,
) -> vm_machine_types.ScriptSourcesControlV1 {
  let base = stage_zero_script_sources_control(
    empty_v1_transaction(),
    validation_merkle_v1.build_frontier([]),
    0,
    #"",
  )
  vm_machine_types.ScriptSourcesControlV1 {
    ..base,
    resolved_input_count: 434,
    resolved_inputs_accumulator: vector.terminal_accumulator,
    resolved_item_peaks: vector.penultimate_resolved_item_peaks,
    stage: 3,
    replay_cursor: 433,
    replay_accumulator: vector.penultimate_accumulator,
    replay_remaining_schedule_hash: vector.penultimate_remaining_schedule_hash,
    spend_index: vector.penultimate_spend_index,
    resolution_schedule_hash: vector.original_schedule_hash,
  }
}

pub fn resolution_schedule_terminal_state(
  control: vm_machine_types.ScriptSourcesControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let tx = empty_v1_transaction()
  let compact = midgard_transaction_to_compact(tx)
  let transaction_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = stage_zero_script_sources_work_witness(control)
  Pair(
    forced_pending_state(
      transaction_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.ScriptSources,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

pub fn verify_maximum_input_schedule_terminal_vector(
  vector: ResolutionScheduleTerminalVectorV1,
) -> Bool {
  let pre_control = resolution_schedule_terminal_control(vector)
  let terminal_accumulator = vm_witness.resolved_input_accumulator_successor(
    pre_control.replay_accumulator,
    vector.last_source_kind,
    vector.last_key,
    vector.last_value,
  )
  let terminal_resolved_item_peaks = validation_merkle_v1.append_leaf(
    pre_control.replay_cursor,
    pre_control.resolved_item_peaks,
    script_proof_v1.resolved_context_item_leaf_hash(
      vector.last_source_kind,
      pre_control.replay_cursor,
      vector.last_key,
      vector.last_value,
    ),
  )
  let terminal_spend_index = pre_control.spend_index + 1 - vector.last_source_kind
  let terminal_control = vm_machine_types.ScriptSourcesControlV1 {
    ..pre_control,
    resolved_item_peaks: terminal_resolved_item_peaks,
    replay_cursor: 434,
    replay_accumulator: terminal_accumulator,
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    spend_index: terminal_spend_index,
  }
  let finish_control = vm_machine_types.ScriptSourcesControlV1 {
    ..terminal_control,
    stage: 4,
  }
  let Pair(pre, pre_work_witness_cbor) = resolution_schedule_terminal_state(
    pre_control,
    12,
  )
  let Pair(terminal, terminal_work_witness_cbor) = resolution_schedule_terminal_state(
    terminal_control,
    13,
  )
  let Pair(finish, _) = resolution_schedule_terminal_state(finish_control, 14)
  let replay_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: pre_work_witness_cbor,
    claimed_successor: terminal,
  }
  let replay_evidence = vm_machine_types.ValidationOneStepEvidenceV1 {
    transition: replay_transition,
    auxiliary: vm_machine_types.ResolvedInputReplayWitness {
      source_kind: vector.last_source_kind,
      key: vector.last_key,
      next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      value: vector.last_value,
    },
  }
  let finish_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: terminal_work_witness_cbor,
    claimed_successor: finish,
  }
  let finish_evidence = vm_machine_types.ValidationOneStepEvidenceV1 {
    transition: finish_transition,
    auxiliary: vm_machine_types.NoAuxiliaryWitness,
  }
  let premature_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: pre_work_witness_cbor,
    claimed_successor: finish,
  }
  let malformed_terminal_cursor_peaks = validation_merkle_v1.append_leaf(
    434,
    terminal_resolved_item_peaks,
    h_a,
  )
  let malformed_terminal_cursor_control = vm_machine_types.ScriptSourcesControlV1 {
    ..terminal_control,
    resolved_item_peaks: malformed_terminal_cursor_peaks,
    replay_cursor: 435,
  }
  let malformed_terminal_cursor_finish_control = vm_machine_types.ScriptSourcesControlV1 {
    ..malformed_terminal_cursor_control,
    stage: 4,
  }
  let
      Pair(malformed_terminal_cursor,
      malformed_terminal_cursor_work_witness_cbor)
    = resolution_schedule_terminal_state(malformed_terminal_cursor_control, 13)
  let Pair(malformed_terminal_cursor_finish, _) = resolution_schedule_terminal_state(
    malformed_terminal_cursor_finish_control,
    14,
  )
  let malformed_terminal_cursor_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: malformed_terminal_cursor_work_witness_cbor,
    claimed_successor: malformed_terminal_cursor_finish,
  }
  let malformed_terminal_accumulator_control = vm_machine_types.ScriptSourcesControlV1 {
    ..terminal_control,
    replay_accumulator: h_a,
  }
  let malformed_terminal_accumulator_finish_control = vm_machine_types.ScriptSourcesControlV1 {
    ..malformed_terminal_accumulator_control,
    stage: 4,
  }
  let
      Pair(malformed_terminal_accumulator,
      malformed_terminal_accumulator_work_witness_cbor)
    = resolution_schedule_terminal_state(
    malformed_terminal_accumulator_control,
    13,
  )
  let Pair(malformed_terminal_accumulator_finish, _) = resolution_schedule_terminal_state(
    malformed_terminal_accumulator_finish_control,
    14,
  )
  let malformed_terminal_accumulator_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: malformed_terminal_accumulator_work_witness_cbor,
    claimed_successor: malformed_terminal_accumulator_finish,
  }
  let stage_two_control = vm_machine_types.ScriptSourcesControlV1 {
    ..pre_control,
    resolved_item_peaks: [],
    stage: 2,
    replay_cursor: 0,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    spend_index: 0,
  }
  let stage_three_control = vm_machine_types.ScriptSourcesControlV1 {
    ..stage_two_control,
    stage: 3,
    replay_remaining_schedule_hash: vector.original_schedule_hash,
  }
  let altered_head_control = vm_machine_types.ScriptSourcesControlV1 {
    ..stage_three_control,
    resolution_schedule_hash: h_a,
  }
  let Pair(stage_two, stage_two_work_witness_cbor) = resolution_schedule_terminal_state(
    stage_two_control,
    10,
  )
  let Pair(stage_three, _) = resolution_schedule_terminal_state(
    stage_three_control,
    11,
  )
  let Pair(altered_head, _) = resolution_schedule_terminal_state(
    altered_head_control,
    11,
  )
  let stage_two_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_two_work_witness_cbor,
    claimed_successor: stage_three,
  }
  let altered_head_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: stage_two_work_witness_cbor,
    claimed_successor: altered_head,
  }
  let mutated_source_kind = 1 - vector.last_source_kind
  let mutated_key = bytearray.concat(
    builtin.slice_bytearray(
      0,
      bytearray.length(vector.last_key) - 1,
      vector.last_key,
    ),
    #"ff",
  )
  and {
    terminal_accumulator == vector.terminal_accumulator,
    validation_merkle_v1.frontier_commitment(434, terminal_resolved_item_peaks) == vector.terminal_frontier_commitment,
    bytearray.length(cbor.serialise(replay_evidence)) < 16_384,
    bytearray.length(cbor.serialise(finish_evidence)) < 16_384,
    vm_script_sources.verify_script_sources_stage_two_semantics_v1(
      stage_two,
      stage_two_transition,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    vm_middle_semantics.stage_two_advance(stage_two, stage_two_transition),
    !vm_middle_semantics.stage_two_advance(stage_two, altered_head_transition),
    !vm_script_sources.verify_script_sources_stage_two_semantics_v1(
      stage_two,
      altered_head_transition,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    vm_script_sources.verify_script_sources_stage_three_replay_semantics_v1(
      pre,
      replay_transition,
      vector.last_source_kind,
      vector.last_key,
      vm_witness.empty_resolution_schedule_hash(),
      vector.last_value,
    ),
    vm_middle_semantics.stage_three_replay(
      pre,
      replay_transition,
      vector.last_source_kind,
      vector.last_key,
      vm_witness.empty_resolution_schedule_hash(),
      vector.last_value,
    ),
    !vm_script_sources.verify_script_sources_stage_three_replay_semantics_v1(
      pre,
      replay_transition,
      mutated_source_kind,
      vector.last_key,
      vm_witness.empty_resolution_schedule_hash(),
      vector.last_value,
    ),
    !vm_script_sources.verify_script_sources_stage_three_replay_semantics_v1(
      pre,
      replay_transition,
      vector.last_source_kind,
      mutated_key,
      vm_witness.empty_resolution_schedule_hash(),
      vector.last_value,
    ),
    !vm_script_sources.verify_script_sources_stage_three_replay_semantics_v1(
      pre,
      replay_transition,
      vector.last_source_kind,
      vector.last_key,
      h_a,
      vector.last_value,
    ),
    !vm_script_sources.verify_script_sources_stage_three_finish_semantics_v1(
      pre,
      premature_transition,
    ),
    !vm_script_sources.verify_script_sources_stage_three_finish_semantics_v1(
      malformed_terminal_cursor,
      malformed_terminal_cursor_transition,
    ),
    !vm_script_sources.verify_script_sources_stage_three_finish_semantics_v1(
      malformed_terminal_accumulator,
      malformed_terminal_accumulator_transition,
    ),
    vm_script_sources.verify_script_sources_stage_three_finish_semantics_v1(
      terminal,
      finish_transition,
    ),
  }
}

/// Replay values are compact ledger-output descriptors, not raw output CBOR:
/// `script_sources_replay_item` decodes the value with
/// `ledger_output_commitment_v1.decode` and reads the resolved address and the
/// reference-script fields out of it.
pub fn replay_output_descriptor_fixture(
  output_index: Int,
  output_cbor: ByteArray,
  address: ByteArray,
  reference_script: Option<MidgardVersionedScript>,
) -> ByteArray {
  expect Some(cardano_tx_out) = script_context_v1.tx_out_summary_v1(
    output_cbor,
    False,
  )
  expect Some(midgard_tx_out) = script_context_v1.tx_out_summary_v1(
    output_cbor,
    True,
  )
  expect Some(cardano_spend_datum) = script_context_v1.spend_datum_summary_v1(
    output_cbor,
  )
  let base = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index,
    total_length: bytearray.length(output_cbor),
    item_commitment: bounded_item_v1.from_bytes(2, output_index, output_cbor),
    address,
    lovelace: 0,
    asset_count: 0,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
    cardano_value_size: 0,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out,
    midgard_tx_out,
    cardano_spend_datum,
  }
  let descriptor = when reference_script is {
    None -> base
    Some(script) -> {
      let script_cbor = encode_midgard_versioned_script(script)
      ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
        ..base,
        reference_script_language: script_proof_v1.language_tag(script.language),
        reference_script_hash: script_proof_v1.versioned_script_hash(script),
        reference_script_total_length: bytearray.length(script_cbor),
        reference_script_item_commitment: bounded_item_v1.from_bytes(
          2,
          output_index,
          script_cbor,
        ),
      }
    }
  }
  ledger_output_commitment_v1.encode(descriptor)
}

/// The one output every stage-4 output-commitment fixture in this module folds.
pub fn canonical_output_commitment_output() -> MidgardTxOutput {
  MidgardTxOutput {
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
}

pub fn canonical_output_commitment_output_cbor() -> ByteArray {
  encode_midgard_tx_output(canonical_output_commitment_output())
}

/// One stage-4 output-commitment step, with the two quantities the fold commits
/// about the item left free so a caller can forge either.
///
/// **Why the forgery moved from the evidence to the claimed successor.** Under
/// the retired counted scheme the prover asserted the item's
/// `(item_length, item_commitment)` in an `ItemProofV1` and
/// `bounded_collection_v1.verify_item` bound that tuple into `outputs_hash` —
/// the predicate #592 measured as unsatisfiable under §4. Both quantities are
/// now *derived* from bytes §8's door authenticated
/// (`machine_field_item_commitment` / `machine_field_item_count`), so no field
/// is left in the evidence to forge: a prover who changes either has changed
/// the field preimage, and the door's single `blake2b_256` against
/// `outputs_hash` refuses it by aborting (§7.3) rather than by returning
/// `False`. What a prover can still get wrong is what it *claims the fold
/// accumulated*, and that is what these two parameters perturb — the same
/// `C21-STAGE4` Option A soundness claim, checked at the surface where it is
/// still decidable.
pub fn canonical_output_commitment_step_with_successor(
  step_to_verify: Int,
  claimed_output_leaf: ByteArray,
  claimed_output_total_count: Int,
) -> Bool {
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
  let Pair(pre, work_witness_cbor) = script_sources_step_fixture(
    tx,
    0,
    accumulator,
    4,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    14,
  )
  let claimed_output_frontier = validation_merkle_v1.build_frontier(
    [claimed_output_leaf],
  )
  let Pair(after_item, after_item_work_witness_cbor) = script_sources_output_step_fixture_with_resolved_items(
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
    claimed_output_frontier,
    claimed_output_total_count,
    vm_witness.empty_receive_purpose_scan_control(),
    vm_witness.empty_observer_purpose_scan_control(),
    15,
  )
  let Pair(post, _) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    5,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    output_frontier,
    16,
  )
  if step_to_verify == 0 {
    vm_script_sources.verify_script_sources_stage_four_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: after_item,
      },
      vm_machine_types.TransactionRedeemerItemBeginWitness {
        carriage: inline_field_carriage([output_cbor]),
      },
      inline_field_door(),
    )
  } else {
    and {
      step_to_verify == 1,
      vm_script_sources.verify_script_sources_stage_four_semantics_v1(
        after_item,
        vm_machine_types.ValidationOneStepWitnessV1 {
          work_witness_cbor: after_item_work_witness_cbor,
          claimed_successor: post,
        },
        vm_machine_types.NoAuxiliaryWitness,
        inline_field_door(),
      ),
    }
  }
}

pub fn canonical_output_commitment_step(step_to_verify: Int) -> Bool {
  canonical_output_commitment_step_with_successor(
    step_to_verify,
    script_proof_v1.output_leaf_hash(
      0,
      canonical_output_commitment_output_cbor(),
    ),
    1,
  )
}

pub fn authenticated_output_proof_step(step_to_verify: Int) -> Bool {
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
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(before_begin, before_begin_work_witness_cbor) = script_sources_output_step_fixture(
    tx,
    0,
    accumulator,
    5,
    empty_frontier,
    empty_frontier,
    0,
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    empty_frontier,
    0,
    output_frontier,
    16,
  )
  let Pair(before_step, before_step_work_witness_cbor) = script_sources_active_output_proof_fixture(
    tx,
    0,
    output_frontier,
    vm_witness.empty_receive_purpose_scan_control(),
    material.initial,
    17,
  )
  let Pair(after_step, _) = script_sources_active_output_proof_fixture(
    tx,
    0,
    output_frontier,
    vm_witness.empty_receive_purpose_scan_control(),
    material.first_successor,
    18,
  )
  let begin_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: before_begin_work_witness_cbor,
    claimed_successor: before_step,
  }
  let begin_auxiliary = vm_machine_types.LedgerOutputProofBeginWitness {
    output_index: 0,
    total_length: bytearray.length(output_cbor),
    item_commitment: material.item_commitment,
    siblings: [],
  }
  if step_to_verify == 0 {
    vm_script_sources.verify_script_sources_output_proof_begin_semantics_v1(
      before_begin,
      begin_transition,
      0,
      bytearray.length(output_cbor),
      material.item_commitment,
      [],
    )
  } else if step_to_verify == 2 {
    !vm_script_sources.verify_script_sources_non_output_semantics_v1(
      before_begin,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: begin_transition,
        auxiliary: begin_auxiliary,
      },
      inline_field_door(),
    )
  } else {
    and {
      step_to_verify == 1,
      vm_script_sources.verify_script_sources_output_proof_step_semantics_v1(
        before_step,
        vm_machine_types.ValidationOneStepWitnessV1 {
          work_witness_cbor: before_step_work_witness_cbor,
          claimed_successor: after_step,
        },
        ledger_output_proof_v1.LedgerOutputProofChunks {
          chunk_proof: material.chunk_proof,
          next_chunk_proof: None,
        },
      ),
    }
  }
}

pub fn protected_receive_purpose_step(step_to_verify: Int) -> Bool {
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
    source_count: receive_source_frontier.count,
    source_peaks: receive_source_frontier.peaks,
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: h_28_a,
    descriptor_peaks: output_descriptor_frontier.peaks,
  }
  let committed_receive_scan = vm_machine_types.ReceivePurposeScanControlV1 {
    source_count: receive_source_frontier.count,
    source_peaks: receive_source_frontier.peaks,
    receive_count: 1,
    previous_hash: h_28_a,
    candidate_hash: #"",
    descriptor_peaks: output_descriptor_frontier.peaks,
  }
  let receive_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(3, 0, h_28_a, h_28_a)],
  )
  let Pair(pre, work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    0,
    output_frontier,
    empty_receive_scan,
    15,
  )
  let Pair(after_candidate, after_candidate_work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    empty_frontier,
    1,
    output_frontier,
    selected_receive_scan,
    16,
  )
  let Pair(after_append, after_append_work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    receive_purpose_frontier,
    0,
    output_frontier,
    committed_receive_scan,
    17,
  )
  let Pair(after_rescan, after_rescan_work_witness_cbor) = script_sources_receive_scan_step_fixture(
    tx,
    receive_purpose_frontier,
    1,
    output_frontier,
    committed_receive_scan,
    18,
  )
  let Pair(post, _) = script_discovery_step_fixture_with_output_frontier(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    receive_purpose_frontier,
    1,
    output_frontier,
    output_descriptor_frontier,
    vm_witness.empty_script_discovery_control(),
    19,
  )
  let select_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: after_candidate,
  }
  let append_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: after_candidate_work_witness_cbor,
    claimed_successor: after_append,
  }
  let rescan_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: after_append_work_witness_cbor,
    claimed_successor: after_rescan,
  }
  let finish_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: after_rescan_work_witness_cbor,
    claimed_successor: post,
  }
  if step_to_verify == 0 {
    and {
      vm_script_sources_late_stages.verify_script_sources_stage_seven_receive_semantics_v1(
        pre,
        select_transition,
        3,
        0,
        h_28_a,
        h_28_a,
        [],
      ),
      vm_one_step.verify_one_step_evidence(
        pre,
        vm_machine_types.ValidationOneStepEvidenceV1 {
          transition: select_transition,
          auxiliary: vm_machine_types.ScriptPurposeScanWitness {
            purpose_kind: 3,
            purpose_index: 0,
            script_hash: h_28_a,
            subject: h_28_a,
            siblings: [],
          },
        },
        inline_field_door(),
      ),
    }
  } else if step_to_verify == 1 {
    and {
      vm_script_sources_late_stages.verify_script_sources_stage_seven_finish_semantics_v1(
        after_candidate,
        append_transition,
      ),
      vm_one_step.verify_one_step_evidence(
        after_candidate,
        vm_machine_types.ValidationOneStepEvidenceV1 {
          transition: append_transition,
          auxiliary: vm_machine_types.NoAuxiliaryWitness,
        },
        inline_field_door(),
      ),
    }
  } else if step_to_verify == 2 {
    and {
      vm_script_sources_late_stages.verify_script_sources_stage_seven_receive_semantics_v1(
        after_append,
        rescan_transition,
        3,
        0,
        h_28_a,
        h_28_a,
        [],
      ),
      vm_one_step.verify_one_step_evidence(
        after_append,
        vm_machine_types.ValidationOneStepEvidenceV1 {
          transition: rescan_transition,
          auxiliary: vm_machine_types.ScriptPurposeScanWitness {
            purpose_kind: 3,
            purpose_index: 0,
            script_hash: h_28_a,
            subject: h_28_a,
            siblings: [],
          },
        },
        inline_field_door(),
      ),
    }
  } else {
    and {
      step_to_verify == 3,
      vm_script_sources_late_stages.verify_script_sources_stage_seven_finish_semantics_v1(
        after_rescan,
        finish_transition,
      ),
      vm_one_step.verify_one_step_evidence(
        after_rescan,
        vm_machine_types.ValidationOneStepEvidenceV1 {
          transition: finish_transition,
          auxiliary: vm_machine_types.NoAuxiliaryWitness,
        },
        inline_field_door(),
      ),
    }
  }
}

pub fn script_sources_rejects_wrong_output_network(
  network_id: Int,
  protected: Bool,
) -> Bool {
  let output = MidgardTxOutput {
    address: MidgardAddress {
      protected,
      network_id,
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
  vm_script_sources.verify_script_sources_output_proof_finalize_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(
        pre,
        network_mismatch_rejection_code,
      ),
    },
    vm_machine_types.NoSignerSetProof,
  )
}

pub fn maximum_mint_asset_leaves(
  asset_index: Int,
  asset_count: Int,
) -> List<ByteArray> {
  if asset_index == asset_count {
    []
  } else {
    [
      vm_shared.mint_asset_leaf_hash(
        h_28_a,
        bytearray.from_int_big_endian(asset_index, 32),
        1,
      ),
      ..maximum_mint_asset_leaves(asset_index + 1, asset_count)
    ]
  }
}

pub fn crossing_mint_fold_step(withhold_next_chunk: Bool) -> Bool {
  let mint = decode_midgard_tx_mint_preimage_cbor(maximum_mint_preimage_cbor())
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, mint },
  }
  // §5.6's policy item is now a named fixture in its own right rather than the
  // retired raw map's tail with a fresh head glued on.
  let item_cbor = maximum_mint_policy_item_cbor
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
  let first_hash = bounded_item_v1.hash_chunk(5, 0, 0, first_chunk)
  let second_hash = bounded_item_v1.hash_chunk(5, 0, 1, second_chunk)
  let chunk_frontier = validation_merkle_v1.build_frontier(
    [first_hash, second_hash],
  )
  let item_commitment = bounded_item_v1.commitment(
    5,
    0,
    bytearray.length(item_cbor),
    chunk_frontier.peaks,
  )
  let current_chunk_proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 5,
    item_index: 0,
    total_length: bytearray.length(item_cbor),
    chunk_index: 0,
    chunk: first_chunk,
    frontier: chunk_frontier.peaks,
    siblings: [second_hash],
  }
  let next_chunk_proof = bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 5,
    item_index: 0,
    total_length: bytearray.length(item_cbor),
    chunk_index: 1,
    chunk: second_chunk,
    frontier: chunk_frontier.peaks,
    siblings: [first_hash],
  }
  let purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(1, 0, h_28_a, h_28_a)],
  )
  let prior_asset_frontier = validation_merkle_v1.build_frontier(
    maximum_mint_asset_leaves(0, 116),
  )
  let next_asset_frontier = validation_merkle_v1.build_frontier(
    maximum_mint_asset_leaves(0, 117),
  )
  let pre_fold = vm_machine_types.MintFoldControlV1 {
    policy_count: 1,
    policy_cursor: 0,
    previous_policy: #"",
    active_policy: h_28_a,
    item_length: bytearray.length(item_cbor),
    item_commitment,
    item_cursor: 4_093,
    assets_remaining: 12,
    policy_asset_cursor: 116,
    previous_asset: bytearray.from_int_big_endian(115, 32),
    asset_count: 116,
    asset_peaks: prior_asset_frontier.peaks,
  }
  let post_fold = vm_machine_types.MintFoldControlV1 {
    ..pre_fold,
    item_cursor: 4_128,
    assets_remaining: 11,
    policy_asset_cursor: 117,
    previous_asset: bytearray.from_int_big_endian(116, 32),
    asset_count: 117,
    asset_peaks: next_asset_frontier.peaks,
  }
  let Pair(pre, work_witness_cbor) = script_sources_mint_fold_fixture(
    tx,
    purpose_frontier,
    pre_fold,
    6,
    51,
  )
  let Pair(post, _) = script_sources_mint_fold_fixture(
    tx,
    purpose_frontier,
    post_fold,
    6,
    52,
  )
  vm_script_sources.verify_script_sources_stage_six_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    vm_machine_types.MintFoldAssetWitness {
      chunk_proof: current_chunk_proof,
      next_chunk_proof: if withhold_next_chunk {
        None
      } else {
        Some(next_chunk_proof)
      },
    },
    inline_field_door(),
  )
}

pub fn canonical_observer_purpose_step(step_to_verify: Int) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, required_observers: [h_28_a] },
  }
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let observer_purpose_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.purpose_leaf_hash(2, 0, h_28_a, h_28_a)],
  )
  let final_purpose_frontier = validation_merkle_v1.build_frontier(
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
  let Pair(after_observer, after_observer_work_witness_cbor) = script_sources_output_step_fixture_with_resolved_items(
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
  let Pair(post, _) = script_discovery_step_fixture(
    tx,
    8,
    empty_frontier,
    empty_frontier,
    final_purpose_frontier,
    vm_witness.empty_script_discovery_control(),
    20,
  )
  let carriage = inline_field_carriage([h_28_a])
  let observer_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: after_observer,
  }
  let finish_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor: after_observer_work_witness_cbor,
    claimed_successor: post,
  }
  if step_to_verify == 0 {
    and {
      vm_script_sources_late_stages.verify_script_sources_stage_seven_observer_semantics_v1(
        pre,
        observer_transition,
        inline_field_door(),
        3,
        0,
        carriage,
      ),
      vm_one_step.verify_one_step_evidence(
        pre,
        vm_machine_types.ValidationOneStepEvidenceV1 {
          transition: observer_transition,
          auxiliary: vm_machine_types.TransactionFieldChunkWitness {
            field_index: 3,
            item_index: 0,
            carriage,
          },
        },
        inline_field_door(),
      ),
    }
  } else {
    and {
      step_to_verify == 1,
      vm_script_sources_late_stages.verify_script_sources_stage_seven_finish_semantics_v1(
        after_observer,
        finish_transition,
      ),
      vm_one_step.verify_one_step_evidence(
        after_observer,
        vm_machine_types.ValidationOneStepEvidenceV1 {
          transition: finish_transition,
          auxiliary: vm_machine_types.NoAuxiliaryWitness,
        },
        inline_field_door(),
      ),
    }
  }
}

// ---------------------------------------------------------------------------
// Successor-encoding controls.
//
// Every ScriptSources successor must be committed in the single canonical
// encoding produced by `encode_script_sources_witness` (definite-length array
// header, `encode_definite_bytes` for every bytestring). Re-serialising the
// decoded `Data` items with `builtin.list_data |> cbor.serialise` is Plutus
// `serialiseData`, which emits an indefinite `9f..ff` array and chunks
// bytestrings over 64 bytes as `5f..ff`. The two forms are distinct byte
// strings, so committing the wrong one wedges the trace: the next step's
// control binding re-derives the canonical encoding and rejects.
//
// These controls pin both directions -- the canonical successor is accepted and
// the `serialiseData` successor (as well as a semantically mutated successor)
// is still rejected -- so a fix here cannot degrade into widened acceptance.
// ---------------------------------------------------------------------------

/// Re-serialise a work witness through Plutus `serialiseData`. The result
/// decodes to the same `Data` but is a different byte string than the canonical
/// encoding, which is exactly what the successor commitments must discriminate.
pub fn serialise_data_reencoding(work_witness_cbor: ByteArray) -> ByteArray {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  data |> builtin.un_list_data |> builtin.list_data |> cbor.serialise
}

/// A machine state committing to an arbitrary ScriptSources work witness, used
/// to build deliberately wrong successors without going through a fixture.
pub fn script_sources_state_for_work_witness(
  tx: MidgardTransaction,
  work_witness_cbor: ByteArray,
  program_counter: Int,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
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

pub fn late_phase_a_native_scan_witness(
  tx: MidgardTransaction,
  continuation_cbor: ByteArray,
  stage: Int,
  item_length: Int,
  item_commitment: ByteArray,
  cursor: Int,
  node_count: Int,
  result: Int,
) -> ByteArray {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  vm_witness.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    blake2b_256(continuation_cbor),
    stage,
    1,
    0,
    0,
    item_length,
    item_commitment,
    cursor,
    #"",
    0,
    node_count,
    result,
    signer_frontier.count,
    signer_frontier.peaks,
    continuation_cbor,
  )
}
