//// R5 item 1 (#617): validator-level vectors for the cek decomposition.
////
//// Index 11 of the validation-dispute machine used to be one aggregate
//// "direct" resolver. It is now `cek_v1` (a `prepare_selected` over a group
//// of four) plus one semantic validator per auxiliary kind:
////
////   0  `cek_finish_semantic_v1`               hand-off to ValueAndMint, no auxiliary
////   1  `cek_execution_selection_semantic_v1`  `NativeExecutionScanWitness` + material route
////   2  `cek_context_step_semantic_v1`         context-build step, §8 door
////   3  `cek_core_step_semantic_v1`            `CekCoreStepWitness`
////
//// These vectors drive every one of the five validators through `main.spend`
//// on the smallest honest step of its kind, pin the redeemer wire layouts
//// (the 9f191e9a lesson: an ABI that moves without a golden moves again,
//// silently), and show that each semantic validator refuses the other
//// kinds' honest steps and that the prepare refuses the retired group
//// cardinality. The semantic partition itself -- every wrapper against every
//// kind -- is proven at the library level in
//// `lib/midgard/validation-machine-v1.test.ak` (`cek_kinds_partition_the_cek_step_space`);
//// what these add is that the deployed validators reach those wrappers with
//// the right arguments, commit to the right evidence hash, and route the
//// material through `verify_cek_route_v1`.

use aiken/cbor
use aiken/primitive/bytearray
use fraud_proofs/validation_trace/cek_context_step_semantic_v1 as context_validator
use fraud_proofs/validation_trace/cek_core_step_semantic_v1 as core_validator
use fraud_proofs/validation_trace/cek_execution_selection_semantic_v1 as selection_validator
use fraud_proofs/validation_trace/cek_finish_semantic_v1 as finish_validator
use fraud_proofs/validation_trace/cek_v1 as prepare_validator
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/transaction.{
  midgard_transaction_proof_commitment_v1, midgard_transaction_proof_source_v1,
  midgard_transaction_to_compact,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardVersionedScript, NativeCardanoScript, PlutusV3Script, TxIsValid,
  native_tx_version_v1,
}
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types.{
  CekContextControlV1, CekCoreStepWitness, NativeExecutionScanWitness,
  NativeScriptsControlV1, NoAuxiliaryWitness, ValidationAuxiliaryWitnessV1,
  ValidationOneStepEvidenceV1, ValidationOneStepWitnessV1,
}
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_merkle_v1
use midgard/validation_resolution_v1.{
  PreparedValidationResolutionStateV1, ValidationResolutionStateV1,
  hash_one_step_evidence, prepare_semantic_resolution,
  prepared_resolution_version, resolution_version, winning_resolution,
}
use midgard/validation_resolver_v1.{
  CekMaterialRouteV1, DirectCekMaterial, NoCekMaterial, PrepareSelected,
}
use midgard/validation_trace_v1

// ---------------------------------------------------------------------------
// Shared fixtures
// ---------------------------------------------------------------------------

const h_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const h_b = #"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

const h_c = #"cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"

const h_28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

const zero = #"0000000000000000000000000000000000000000000000000000000000000000"

const validation_context_cbor =
  #"8701546d6964676172642d636f6e73656e7375732d763118640000001864"

const ct_asset = "cek-split-thread"

/// The deployed group, in semantic-resolver order. Only the slot under test
/// is the next-step hash the L1 fixture pays to; the others are the
/// unrelated step hash so a mis-routed index fails on the output script.
fn group_with_slot(slot: Int) -> List<ByteArray> {
  [
    if slot == 0 {
      fixture.next_step_script_hash
    } else {
      fixture.step_script_hash
    },
    if slot == 1 {
      fixture.next_step_script_hash
    } else {
      fixture.step_script_hash
    },
    if slot == 2 {
      fixture.next_step_script_hash
    } else {
      fixture.step_script_hash
    },
    if slot == 3 {
      fixture.next_step_script_hash
    } else {
      fixture.step_script_hash
    },
  ]
}

fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = []
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: zero,
      auxiliary_data_hash: zero,
      network_id: 255,
    },
    witness_set: MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

/// The native control of a transaction with nothing to resolve, sign,
/// spend, mint or execute (the twin of the lib test module's
/// `empty_native_control_fixture`, restricted to witness-free transactions).
fn empty_native_control(tx: MidgardTransaction) -> NativeScriptsControlV1 {
  let proof_source = midgard_transaction_proof_source_v1(tx)
  let empty = validation_merkle_v1.build_frontier([])
  NativeScriptsControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: vm_witness.initial_resolution_accumulator(),
    spend_input_count: 0,
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: empty.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      empty.count,
      empty.peaks,
    ),
    source_count: 0,
    source_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_descriptor_peaks: validation_merkle_v1.empty_frontier(),
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: 0,
    execution_peaks: validation_merkle_v1.empty_frontier(),
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

/// A cek-phase machine state over `tx` whose work root commits to the given
/// cek witness control fields, paired with the work witness bytes.
fn cek_state(
  tx: MidgardTransaction,
  control: NativeScriptsControlV1,
  context_control_cbor: ByteArray,
  execution_cursor: Int,
  active_state_hash: ByteArray,
  execution_cpu_limit: Int,
  execution_memory_limit: Int,
  program_envelope_hash: ByteArray,
  program_counter: Int,
  execution_cpu: Int,
  execution_memory: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = vm_witness.encode_cek_witness_v1(
    control,
    context_control_cbor,
    execution_cursor,
    0,
    0,
    active_state_hash,
    execution_cpu_limit,
    execution_memory_limit,
    program_envelope_hash,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: h_a,
      transaction_id: tx_id,
      transaction_commitment: midgard_transaction_proof_commitment_v1(tx),
      validation_context_hash: validation_trace_v1.hash_validation_context(
        validation_context_cbor,
      ),
      source_kind: validation_trace_v1.Forced,
      prior_ledger_root: h_c,
      phase: validation_trace_v1.Cek,
      program_counter,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.Cek,
        program_counter,
        work_witness_cbor,
      ),
      execution_cpu,
      execution_memory,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero,
      ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
    },
    work_witness_cbor,
  )
}

/// One honest cek step of some kind: the pre-state, the transition, the
/// auxiliary the kind's validator rebuilds from its action, and the
/// material route the selection validator carries.
type CekStepFixtureV1 {
  kind: Int,
  pre: validation_trace_v1.ValidationMachineStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  material_route: CekMaterialRouteV1,
}

fn step_fixture(
  kind: Int,
  pre: validation_trace_v1.ValidationMachineStateV1,
  work_witness_cbor: ByteArray,
  post: validation_trace_v1.ValidationMachineStateV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  material_route: CekMaterialRouteV1,
) -> CekStepFixtureV1 {
  CekStepFixtureV1 {
    kind,
    pre,
    transition: ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary,
    material_route,
  }
}

/// Kind 0: the hand-off. A transaction with no execution at all
/// (`execution_count == 0`) has nothing to select, so its whole cek phase is
/// this one step.
fn finish_fixture() -> CekStepFixtureV1 {
  let tx = empty_v1_transaction()
  let control = empty_native_control(tx)
  let Pair(pre, work_witness_cbor) = cek_state(
    tx,
    control,
    #"",
    0,
    #"",
    0,
    0,
    #"",
    40,
    0,
    0,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      41,
      vm_witness.encode_value_and_mint_witness_v1(control),
    ),
  }
  step_fixture(
    0,
    pre,
    work_witness_cbor,
    post,
    NoAuxiliaryWitness,
    NoCekMaterial,
  )
}

/// The one-node program (`error`) whose complete material fits a direct
/// route: the same envelope and sidecar `validation-resolver-v1.test.ak`
/// verifies through `verify_cek_material_route_for_selected_envelope_v1`.
fn one_node_envelope() -> ByteArray {
  cek_proof_v1.encode_program_envelope_v1(
    1,
    1,
    0,
    cek_proof_v1.hash_error_term_v1(),
    1,
    2,
  )
}

fn one_node_sidecar() -> ByteArray {
  #"820181825820"
    |> bytearray.concat(cek_proof_v1.hash_error_term_v1())
    |> bytearray.concat(#"830100428106")
}

fn singleton_field_item_chunk_proof(
  field_index: Int,
  key: ByteArray,
) -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index,
    item_index: 0,
    total_length: bytearray.length(key),
    chunk_index: 0,
    chunk: key,
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: bounded_item_v1.hash_chunk(field_index, 0, 0, key),
      },
    ],
    siblings: [],
  }
}

/// Kind 1: selecting the single PlutusV3 execution of a transaction whose
/// one script is the one-node program, with the complete material carried
/// directly in the redeemer.
fn selection_fixture() -> CekStepFixtureV1 {
  let tx = empty_v1_transaction()
  let envelope = one_node_envelope()
  let envelope_hash = cek_proof_v1.hash_program_envelope_v1(
    1,
    1,
    0,
    cek_proof_v1.hash_error_term_v1(),
    1,
    2,
  )
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: envelope,
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let subject = #"00"
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    3,
    script_hash,
    bytearray.length(script_cbor),
    script_item_commitment,
  )
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    subject,
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
  let control = NativeScriptsControlV1 {
    ..empty_native_control(tx),
    source_count: 1,
    source_peaks: source_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let context_control = CekContextControlV1 {
    stage: 0,
    language_tag: 3,
    program_term_root: cek_proof_v1.hash_error_term_v1(),
    program_envelope_hash: envelope_hash,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash,
    subject,
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
  let Pair(pre, work_witness_cbor) = cek_state(
    tx,
    control,
    #"",
    0,
    #"",
    0,
    0,
    #"",
    40,
    0,
    0,
  )
  let Pair(post, _) = cek_state(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    #"",
    0,
    0,
    envelope_hash,
    41,
    0,
    0,
  )
  step_fixture(
    1,
    pre,
    work_witness_cbor,
    post,
    NativeExecutionScanWitness {
      execution_index: 0,
      language_tag: 3,
      purpose_kind: 0,
      purpose_index: 0,
      script_hash,
      subject,
      purpose_siblings: [],
      source_index: 0,
      origin_kind: 0,
      source_key: #"00",
      script_total_length: bytearray.length(script_cbor),
      script_item_commitment,
      source_siblings: [],
      redeemer_leaf: h_c,
      execution_siblings: [],
      first_chunk_proof: singleton_field_item_chunk_proof(6, script_cbor),
    },
    DirectCekMaterial {
      envelope_cbor: envelope,
      sidecar_cbor: one_node_sidecar(),
    },
  )
}

/// Kind 1 again, over the case #629 turned on: a transaction whose only
/// script execution is native. The language bitmap records Plutus languages
/// only, so it is `0` here, yet the execution is still there to select --
/// one selection step, exactly as the machine's trace emits it. Because it
/// is the last (and only) execution, the selection the step makes *is* the
/// hand-off to ValueAndMint, so its claimed successor is byte-for-byte the
/// successor the finish branch would have to claim. That makes the pair
/// below a clean read of the discriminator alone: the selection validator
/// accepts this step and the finish validator must refuse it.
fn native_only_selection_fixture() -> CekStepFixtureV1 {
  let tx = empty_v1_transaction()
  // The smallest native script: `ScriptPubkey(h_28_a)`, whose versioned
  // encoding is the 36-byte `[0, h#"8200581c..."]` the scan reads a
  // `language_tag == 0` header out of.
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"8200581c" |> bytearray.concat(h_28_a),
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let subject = #"00"
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    0,
    script_hash,
    bytearray.length(script_cbor),
    script_item_commitment,
  )
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    subject,
  )
  // A native execution carries no redeemer, so its execution leaf binds the
  // empty redeemer leaf.
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    0,
    purpose_leaf,
    source_leaf,
    #"",
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([execution_leaf])
  let control = NativeScriptsControlV1 {
    ..empty_native_control(tx),
    source_count: 1,
    source_peaks: source_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 0,
  }
  let Pair(pre, work_witness_cbor) = cek_state(
    tx,
    control,
    #"",
    0,
    #"",
    0,
    0,
    #"",
    40,
    0,
    0,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      41,
      vm_witness.encode_value_and_mint_witness_v1(control),
    ),
  }
  step_fixture(
    1,
    pre,
    work_witness_cbor,
    post,
    NativeExecutionScanWitness {
      execution_index: 0,
      language_tag: 0,
      purpose_kind: 0,
      purpose_index: 0,
      script_hash,
      subject,
      purpose_siblings: [],
      source_index: 0,
      origin_kind: 0,
      source_key: #"00",
      script_total_length: bytearray.length(script_cbor),
      script_item_commitment,
      source_siblings: [],
      redeemer_leaf: #"",
      execution_siblings: [],
      first_chunk_proof: singleton_field_item_chunk_proof(6, script_cbor),
    },
    // A native selection selects no program, so it carries no material.
    NoCekMaterial,
  )
}

/// Kind 2: the mint-context initialisation (context stage 6) of a
/// transaction that mints nothing -- the smallest context step, and one
/// with no auxiliary, so the door is reached but never opened.
fn context_step_fixture() -> CekStepFixtureV1 {
  let tx = empty_v1_transaction()
  let singleton = validation_merkle_v1.build_frontier([h_a])
  let control = NativeScriptsControlV1 {
    ..empty_native_control(tx),
    purpose_count: 1,
    purpose_peaks: singleton.peaks,
    execution_count: 1,
    execution_peaks: singleton.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let redeemer_context = vm_witness.initial_cek_redeemer_context_control_v1()
  let init_control = CekContextControlV1 {
    stage: 6,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: h_b,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: #"00",
    redeemer_leaf: h_c,
    redeemer_context_control_hash: vm_witness.hash_cek_redeemer_context_control_v1(
      redeemer_context,
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
    observer_summary,
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
  let successor_control = CekContextControlV1 {
    ..init_control,
    stage: 9,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(init_control),
    0,
    #"",
    0,
    0,
    h_b,
    47,
    0,
    0,
  )
  let Pair(post, _) = cek_state(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(successor_control),
    0,
    #"",
    0,
    0,
    h_b,
    48,
    0,
    0,
  )
  step_fixture(
    2,
    pre,
    work_witness_cbor,
    post,
    NoAuxiliaryWitness,
    NoCekMaterial,
  )
}

/// Kind 3: one active core step, a `ComputeApplication` over two leaf terms
/// (the lib module's `cek_active_core_step_fixture`).
fn core_step_fixture() -> CekStepFixtureV1 {
  let tx = empty_v1_transaction()
  let purpose_frontier = validation_merkle_v1.build_frontier([h_a])
  let execution_frontier = validation_merkle_v1.build_frontier([h_b])
  let control = NativeScriptsControlV1 {
    ..empty_native_control(tx),
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let application_root = cek_proof_v1.hash_application_term_v1(h_a, h_b)
  let machine_pre = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: 0,
    focus_root: application_root,
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: cek_proof_v1.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 10,
    memory: 11,
  }
  let continuation_root = cek_proof_v1.hash_apply_argument_continuation_v1(
    h_b,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
  let machine_post = cek_machine_v1.MachineStateV1 {
    ..machine_pre,
    focus_root: h_a,
    continuation_root,
    cpu: 16010,
    memory: 111,
  }
  let active_pre_hash = cek_proof_v1.hash_machine_state_v1(
    machine_pre.mode,
    machine_pre.execution_index,
    machine_pre.focus_root,
    machine_pre.environment_root,
    machine_pre.continuation_root,
    machine_pre.auxiliary,
    machine_pre.cpu,
    machine_pre.memory,
  )
  let active_post_hash = cek_proof_v1.hash_machine_state_v1(
    machine_post.mode,
    machine_post.execution_index,
    machine_post.focus_root,
    machine_post.environment_root,
    machine_post.continuation_root,
    machine_post.auxiliary,
    machine_post.cpu,
    machine_post.memory,
  )
  let Pair(pre, work_witness_cbor) = cek_state(
    tx,
    control,
    #"",
    0,
    active_pre_hash,
    1000000,
    1000000,
    h_b,
    40,
    10,
    11,
  )
  let Pair(post, _) = cek_state(
    tx,
    control,
    #"",
    0,
    active_post_hash,
    1000000,
    1000000,
    h_b,
    41,
    16010,
    111,
  )
  step_fixture(
    3,
    pre,
    work_witness_cbor,
    post,
    CekCoreStepWitness {
      step: cek_machine_v1.CoreStepEvidenceV1 {
        pre: machine_pre,
        post: machine_post,
        witness: cek_machine_v1.ComputeApplication {
          function: h_a,
          argument: h_b,
        },
      },
    },
    NoCekMaterial,
  )
}

fn fixture_of_kind(kind: Int) -> CekStepFixtureV1 {
  if kind == 0 {
    finish_fixture()
  } else if kind == 1 {
    selection_fixture()
  } else if kind == 2 {
    context_step_fixture()
  } else {
    core_step_fixture()
  }
}

// ---------------------------------------------------------------------------
// Resolution and prepared states
// ---------------------------------------------------------------------------

fn resolution(f: CekStepFixtureV1) -> ValidationResolutionStateV1 {
  ValidationResolutionStateV1 {
    version: resolution_version,
    pre_state: f.pre,
    operator_successor_hash: h_b,
    challenger_successor_hash: validation_trace_v1.hash_machine_state(
      f.transition.claimed_successor,
    ),
  }
}

fn auxiliary_data(f: CekStepFixtureV1) -> Data {
  let auxiliary: Data = f.auxiliary
  auxiliary
}

fn prepared(f: CekStepFixtureV1) -> PreparedValidationResolutionStateV1 {
  let transition: Data = f.transition
  PreparedValidationResolutionStateV1 {
    version: prepared_resolution_version,
    resolution: resolution(f),
    evidence_hash: hash_one_step_evidence(transition, auxiliary_data(f)),
  }
}

fn resolver_datum(
  f: CekStepFixtureV1,
) -> ct.StepDatum<ValidationResolutionStateV1> {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(resolution(f)),
  }
}

fn semantic_datum(
  base: PreparedValidationResolutionStateV1,
) -> ct.StepDatum<PreparedValidationResolutionStateV1> {
  ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: Some(base) }
}

fn winning_output() -> Data {
  let output: Data = winning_resolution()
  output
}

// ---------------------------------------------------------------------------
// Wire-layout goldens
// ---------------------------------------------------------------------------

type PrepareWireMirrorV1 {
  PrepareSelectedMirror {
    input_index: Int,
    output_index: Int,
    semantic_resolver_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: Data,
  }
}

type FinishWireMirrorV1 {
  VerifyFinishMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
  }
}

type SelectionWireMirrorV1 {
  VerifyExecutionSelectionMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: ValidationAuxiliaryWitnessV1,
    material_route: CekMaterialRouteV1,
  }
}

type ContextWireMirrorV1 {
  VerifyContextStepMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    auxiliary: ValidationAuxiliaryWitnessV1,
  }
}

type CoreWireMirrorV1 {
  VerifyCoreStepMirror {
    input_index: Int,
    output_index: Int,
    transition: ValidationOneStepWitnessV1,
    step: cek_machine_v1.CoreStepEvidenceV1,
  }
}

fn golden_state() -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: zero,
    transaction_id: zero,
    transaction_commitment: zero,
    validation_context_hash: zero,
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: zero,
    phase: validation_trace_v1.Cek,
    program_counter: 0,
    work_root: zero,
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: zero,
  }
}

fn golden_transition() -> ValidationOneStepWitnessV1 {
  ValidationOneStepWitnessV1 {
    work_witness_cbor: #"",
    claimed_successor: golden_state(),
  }
}

/// The prepare keeps the shared `validation_resolver_v1.PrepareSelected`
/// shape (with the auxiliary as opaque `Data`) -- the same redeemer every
/// other prepare-selected phase takes.
test prepare_selected_wire_layout_is_pinned() {
  let auxiliary: Data = NoAuxiliaryWitness
  let real: validation_resolver_v1.PrepareSelectedSpendRedeemer = ct.Continue(
    PrepareSelected {
      input_index: 1,
      output_index: 2,
      semantic_resolver_index: 3,
      transition: golden_transition(),
      auxiliary,
    },
  )
  let mirror: ct.StepRedeemer<PrepareWireMirrorV1> = ct.Continue(
    PrepareSelectedMirror {
      input_index: 1,
      output_index: 2,
      semantic_resolver_index: 3,
      transition: golden_transition(),
      auxiliary,
    },
  )
  let mirror_data: Data = mirror
  expect decoded: validation_resolver_v1.PrepareSelectedSpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test finish_wire_layout_is_pinned() {
  let real: finish_validator.SpendRedeemer = ct.Continue(
    finish_validator.VerifyFinish {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let mirror: ct.StepRedeemer<FinishWireMirrorV1> = ct.Continue(
    VerifyFinishMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
    },
  )
  let mirror_data: Data = mirror
  expect decoded: finish_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test execution_selection_wire_layout_is_pinned() {
  let f = selection_fixture()
  let real: selection_validator.SpendRedeemer = ct.Continue(
    selection_validator.VerifyExecutionSelection {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      auxiliary: f.auxiliary,
      material_route: f.material_route,
    },
  )
  let mirror: ct.StepRedeemer<SelectionWireMirrorV1> = ct.Continue(
    VerifyExecutionSelectionMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      auxiliary: f.auxiliary,
      material_route: f.material_route,
    },
  )
  let mirror_data: Data = mirror
  expect decoded: selection_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test context_step_wire_layout_is_pinned() {
  let real: context_validator.SpendRedeemer = ct.Continue(
    context_validator.VerifyContextStep {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      auxiliary: NoAuxiliaryWitness,
    },
  )
  let mirror: ct.StepRedeemer<ContextWireMirrorV1> = ct.Continue(
    VerifyContextStepMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      auxiliary: NoAuxiliaryWitness,
    },
  )
  let mirror_data: Data = mirror
  expect decoded: context_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

test core_step_wire_layout_is_pinned() {
  let f = core_step_fixture()
  expect CekCoreStepWitness { step } = f.auxiliary
  let real: core_validator.SpendRedeemer = ct.Continue(
    core_validator.VerifyCoreStep {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      step,
    },
  )
  let mirror: ct.StepRedeemer<CoreWireMirrorV1> = ct.Continue(
    VerifyCoreStepMirror {
      input_index: 1,
      output_index: 2,
      transition: golden_transition(),
      step,
    },
  )
  let mirror_data: Data = mirror
  expect decoded: core_validator.SpendRedeemer = mirror_data
  and {
    cbor.serialise(real) == cbor.serialise(mirror),
    decoded == real,
  }
}

/// The evidence each semantic validator commits to is exactly
/// `hash_one_step_evidence(transition, <auxiliary the validator rebuilds>)`,
/// which is what the prepare installed -- so the core validator's
/// `CekCoreStepWitness { step }` reconstruction and the finish validator's
/// fixed `NoAuxiliaryWitness` both round-trip through the prepare.
test every_kind_commits_to_the_prepared_evidence_hash() {
  let finish = finish_fixture()
  let selection = selection_fixture()
  let context = context_step_fixture()
  let core = core_step_fixture()
  let transition_of = fn(f: CekStepFixtureV1) -> Data {
    let transition: Data = f.transition
    transition
  }
  expect CekCoreStepWitness { step } = core.auxiliary
  let rebuilt_core: Data = CekCoreStepWitness { step }
  let rebuilt_finish: Data = NoAuxiliaryWitness
  and {
    prepared(finish).evidence_hash == hash_one_step_evidence(
      transition_of(finish),
      rebuilt_finish,
    ),
    prepared(core).evidence_hash == hash_one_step_evidence(
      transition_of(core),
      rebuilt_core,
    ),
    prepared(selection).evidence_hash != prepared(finish).evidence_hash,
    prepared(context).evidence_hash != prepared(finish).evidence_hash,
  }
}

// ---------------------------------------------------------------------------
// Prepare vectors
// ---------------------------------------------------------------------------

fn run_prepare(
  f: CekStepFixtureV1,
  group: List<ByteArray>,
  semantic_resolver_index: Int,
  output_state: Data,
) -> Bool {
  prepare_validator.main.spend(
    group,
    fixture.computation_thread_policy_id,
    Some(resolver_datum(f)),
    ct.Continue(
      PrepareSelected {
        input_index: 0,
        output_index: 0,
        semantic_resolver_index,
        transition: f.transition,
        auxiliary: auxiliary_data(f),
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, output_state),
  )
}

fn expected_prepared_output(f: CekStepFixtureV1) -> Data {
  let output: Data = prepare_semantic_resolution(
    resolution(f),
    f.transition,
    auxiliary_data(f),
  )
  output
}

fn prepare_routes_kind(kind: Int) -> Bool {
  let f = fixture_of_kind(kind)
  let output = expected_prepared_output(f)
  let expected: Data = prepared(f)
  and {
    output == expected,
    run_prepare(f, group_with_slot(kind), kind, output),
  }
}

test prepare_routes_the_finish_kind_to_slot_zero() {
  prepare_routes_kind(0)
}

test prepare_routes_the_execution_selection_kind_to_slot_one() {
  prepare_routes_kind(1)
}

test prepare_routes_the_context_step_kind_to_slot_two() {
  prepare_routes_kind(2)
}

test prepare_routes_the_core_step_kind_to_slot_three() {
  prepare_routes_kind(3)
}

/// The group cardinality is pinned at four: the retired single-resolver
/// shape (a one-hash group) is refused before any hand-off.
test prepare_refuses_the_retired_single_resolver_group() fail {
  let f = finish_fixture()
  run_prepare(
    f,
    [fixture.next_step_script_hash],
    0,
    expected_prepared_output(f),
  )
}

test prepare_refuses_a_fifth_semantic_resolver_index() fail {
  let f = finish_fixture()
  run_prepare(
    f,
    [
      fixture.step_script_hash,
      fixture.step_script_hash,
      fixture.step_script_hash,
      fixture.step_script_hash,
    ],
    4,
    expected_prepared_output(f),
  )
}

test prepare_refuses_a_widened_resolver_group() fail {
  let f = finish_fixture()
  run_prepare(
    f,
    [
      fixture.next_step_script_hash,
      fixture.step_script_hash,
      fixture.step_script_hash,
      fixture.step_script_hash,
      fixture.step_script_hash,
    ],
    0,
    expected_prepared_output(f),
  )
}

/// A wrong index routes the thread to a slot whose hash is not the output's
/// script, so the hand-off fails on the output script pin.
test prepare_refuses_a_misrouted_index() fail {
  let f = finish_fixture()
  run_prepare(f, group_with_slot(0), 1, expected_prepared_output(f))
}

/// The phase pin: a resolution state parked in ValueAndMint cannot be
/// prepared through the cek resolver.
test prepare_refuses_a_value_and_mint_phase_state() fail {
  let f = finish_fixture()
  let foreign_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..f.pre,
    phase: validation_trace_v1.ValueAndMint,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      f.pre.program_counter,
      f.transition.work_witness_cbor,
    ),
  }
  let foreign = CekStepFixtureV1 { ..f, pre: foreign_pre }
  run_prepare(foreign, group_with_slot(0), 0, expected_prepared_output(foreign))
}

// ---------------------------------------------------------------------------
// Semantic vectors: each validator on its honest kind, then on the others
// ---------------------------------------------------------------------------

fn run_finish(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
) -> Bool {
  finish_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      finish_validator.VerifyFinish {
        input_index: 0,
        output_index: 0,
        transition,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, winning_output()),
  )
}

fn run_selection(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  material_route: CekMaterialRouteV1,
) -> Bool {
  selection_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.catalogue_script_hash,
    Some(semantic_datum(base)),
    ct.Continue(
      selection_validator.VerifyExecutionSelection {
        input_index: 0,
        output_index: 0,
        transition,
        auxiliary,
        material_route,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, winning_output()),
  )
}

fn run_context_step(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  context_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      context_validator.VerifyContextStep {
        input_index: 0,
        output_index: 0,
        transition,
        auxiliary,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, winning_output()),
  )
}

fn run_core_step(
  base: PreparedValidationResolutionStateV1,
  transition: ValidationOneStepWitnessV1,
  step: cek_machine_v1.CoreStepEvidenceV1,
) -> Bool {
  core_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      core_validator.VerifyCoreStep {
        input_index: 0,
        output_index: 0,
        transition,
        step,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, winning_output()),
  )
}

/// Runs the semantic validator of kind `kind` over the prepared state and
/// transition of fixture `f`, with the auxiliary shape kind `kind` carries
/// (taken from the kind's own honest fixture when `f` is of another kind).
fn run_kind(kind: Int, f: CekStepFixtureV1) -> Bool {
  let own = if f.kind == kind {
    f
  } else {
    fixture_of_kind(kind)
  }
  if kind == 0 {
    run_finish(prepared(f), f.transition)
  } else if kind == 1 {
    run_selection(prepared(f), f.transition, own.auxiliary, own.material_route)
  } else if kind == 2 {
    run_context_step(prepared(f), f.transition, own.auxiliary)
  } else {
    expect CekCoreStepWitness { step } = own.auxiliary
    run_core_step(prepared(f), f.transition, step)
  }
}

test finish_validator_wins_the_hand_off() {
  run_kind(0, finish_fixture())
}

test execution_selection_validator_wins_with_direct_material() {
  run_kind(1, selection_fixture())
}

test context_step_validator_wins_the_mint_context_initialisation() {
  run_kind(2, context_step_fixture())
}

test core_step_validator_wins_the_application_step() {
  run_kind(3, core_step_fixture())
}

/// The retired direct shape pinned the route only at finalisation; now the
/// selection validator itself refuses a selection whose material is missing
/// (`NoCekMaterial` against a selected Plutus envelope) or whose direct
/// sidecar does not reconstruct the envelope.
test execution_selection_validator_refuses_a_missing_material_route() fail {
  let f = selection_fixture()
  run_selection(prepared(f), f.transition, f.auxiliary, NoCekMaterial)
}

test execution_selection_validator_refuses_a_truncated_sidecar() fail {
  let f = selection_fixture()
  run_selection(
    prepared(f),
    f.transition,
    f.auxiliary,
    DirectCekMaterial {
      envelope_cbor: one_node_envelope(),
      sidecar_cbor: bytearray.take(one_node_sidecar(), 8),
    },
  )
}

/// Kind partition at the validator boundary. Each semantic validator is run
/// over every OTHER kind's honest prepared state and transition; the
/// evidence hash the prepare installed for that kind never matches the
/// auxiliary this validator reconstructs, and even where it could (kind 0
/// against kind 2: both `NoAuxiliaryWitness`) the control discriminator
/// refuses. Each row is its own `fail` test so a regression names the pair.
test finish_validator_refuses_an_execution_selection() fail {
  run_kind(0, selection_fixture())
}

test finish_validator_refuses_a_context_step() fail {
  run_kind(0, context_step_fixture())
}

test finish_validator_refuses_a_core_step() fail {
  run_kind(0, core_step_fixture())
}

test execution_selection_validator_refuses_the_hand_off() fail {
  run_kind(1, finish_fixture())
}

test execution_selection_validator_refuses_a_context_step() fail {
  run_kind(1, context_step_fixture())
}

test execution_selection_validator_refuses_a_core_step() fail {
  run_kind(1, core_step_fixture())
}

test context_step_validator_refuses_the_hand_off() fail {
  run_kind(2, finish_fixture())
}

test context_step_validator_refuses_an_execution_selection() fail {
  run_kind(2, selection_fixture())
}

test context_step_validator_refuses_a_core_step() fail {
  run_kind(2, core_step_fixture())
}

test core_step_validator_refuses_the_hand_off() fail {
  run_kind(3, finish_fixture())
}

test core_step_validator_refuses_an_execution_selection() fail {
  run_kind(3, selection_fixture())
}

test core_step_validator_refuses_a_context_step() fail {
  run_kind(3, context_step_fixture())
}

/// The evidence pin at the semantic boundary: a prepared state whose hash
/// was computed over another auxiliary (here the retired aggregate shape's
/// habit of committing the selection under `NoAuxiliaryWitness`) is refused
/// even though the transition itself is honest.
test finish_validator_refuses_a_foreign_evidence_commitment() fail {
  let f = finish_fixture()
  let selection = selection_fixture()
  let foreign = PreparedValidationResolutionStateV1 {
    ..prepared(f),
    evidence_hash: prepared(selection).evidence_hash,
  }
  run_finish(foreign, f.transition)
}

test finish_validator_refuses_a_swapped_claimed_successor() fail {
  let f = finish_fixture()
  let swapped = ValidationOneStepWitnessV1 {
    ..f.transition,
    claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
      ..f.transition.claimed_successor,
      work_root: h_b,
    },
  }
  run_finish(prepared(f), swapped)
}

test finish_validator_refuses_a_wrong_award_output() fail {
  let f = finish_fixture()
  let base = prepared(f)
  let not_winning: Data = base
  finish_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(semantic_datum(base)),
    ct.Continue(
      finish_validator.VerifyFinish {
        input_index: 0,
        output_index: 0,
        transition: f.transition,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(ct_asset, not_winning),
  )
}

/// The shared cek guards -- the discriminator each wrapper pins and the
/// well-formedness of the cek witness against the pre-state -- are what the
/// kind partition above cannot see: every fixture it runs is honest in all
/// of them. The negatives below break exactly one of those shared guards
/// each.
///
/// A control that still has an execution to select (`execution_cursor 0 <
/// execution_count 1` with `language_bitmap 1`, the selection fixture's
/// control) is an execution selection and not the hand-off, so the finish
/// validator must refuse it however well formed the claimed successor is.
fn pending_execution_control(tx: MidgardTransaction) -> NativeScriptsControlV1 {
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: one_node_envelope(),
  }
  let script_cbor = encode_midgard_versioned_script(script)
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
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, script_hash, #"00")
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    3,
    purpose_leaf,
    source_leaf,
    h_c,
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([execution_leaf])
  NativeScriptsControlV1 {
    ..empty_native_control(tx),
    source_count: 1,
    source_peaks: source_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
}

test finish_validator_refuses_a_hand_off_with_pending_executions() fail {
  let tx = empty_v1_transaction()
  let control = pending_execution_control(tx)
  let Pair(pre, work_witness_cbor) = cek_state(
    tx,
    control,
    #"",
    0,
    #"",
    0,
    0,
    #"",
    40,
    0,
    0,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.ValueAndMint,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      41,
      vm_witness.encode_value_and_mint_witness_v1(control),
    ),
  }
  let f = step_fixture(
    0,
    pre,
    work_witness_cbor,
    post,
    NoAuxiliaryWitness,
    NoCekMaterial,
  )
  run_finish(prepared(f), f.transition)
}

/// #629: the same guard read from the other side. A native-only transaction
/// (`language_bitmap == 0`) with one execution still pending is an execution
/// selection, and the selection validator wins on it -- the language bitmap
/// takes no part in the discrimination, only the cursor does.
test execution_selection_validator_wins_a_native_only_selection() {
  run_kind(1, native_only_selection_fixture())
}

/// #629, the negative twin. The very same pre-state and claimed successor
/// -- an impeccably formed ValueAndMint hand-off under `NoAuxiliaryWitness`,
/// so the prepared evidence hash is the one the finish validator rebuilds --
/// is refused by the finish validator, because a cursor short of the
/// execution count is never the hand-off however empty the bitmap is. Restore
/// `|| language_bitmap == 0` to the finish discriminator and this vector is
/// accepted; restore `language_bitmap != 0` to the selection discriminator
/// and the positive above is refused.
test finish_validator_refuses_a_native_only_selection() fail {
  let honest = native_only_selection_fixture()
  let f = CekStepFixtureV1 { ..honest, kind: 0, auxiliary: NoAuxiliaryWitness }
  run_finish(prepared(f), f.transition)
}

/// And the aggregate route agrees: `verify_cek`'s else-branch is the
/// execution selection, so the four kinds still cover the native-only step.
test native_only_selection_is_an_honest_machine_step() {
  let f = native_only_selection_fixture()
  vm_one_step.verify_cek_one_step_v1(
    f.pre,
    ValidationOneStepEvidenceV1 {
      transition: f.transition,
      auxiliary: f.auxiliary,
    },
    field_door.MachineFieldDoorV1 {
      reference_inputs: [],
      certificate_policy_id: #"",
    },
  )
}

/// `pre.transaction_commitment` is what binds the cek control's native
/// transaction proof to the transaction under dispute; a hand-off that is
/// honest in every other field but whose pre-state commits to a different
/// transaction must be refused.
test finish_validator_refuses_a_foreign_transaction_commitment() fail {
  let f = finish_fixture()
  let forged = CekStepFixtureV1 {
    ..f,
    pre: validation_trace_v1.ValidationMachineStateV1 {
      ..f.pre,
      transaction_commitment: zero,
    },
    transition: ValidationOneStepWitnessV1 {
      ..f.transition,
      claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
        ..f.transition.claimed_successor,
        transaction_commitment: zero,
      },
    },
  }
  run_finish(prepared(forged), forged.transition)
}

/// The cek cursor is bounded by the control's execution count, and the two
/// places that bound lives are what make this vector awkward to build:
/// `encode_cek_witness_v1` refuses to *encode* an out-of-bound cursor, so an
/// honest encode cannot produce the witness, and the well-formedness guard
/// refuses to *accept* one. The vector is therefore the honest hand-off
/// witness with its encoded cursor byte bumped by hand -- the last seven
/// bytes of a hand-off witness are `cursor, completed_cpu,
/// completed_memory, active_state_hash, program_envelope_hash,
/// execution_cpu_limit, execution_memory_limit`, all empty or zero, so the
/// cursor is the byte at `length - 7`. The positive below pins that layout;
/// without it the negative could start bumping some other byte and still
/// look like a refusal.
///
/// The accept path re-encodes the witness through that same encoder, so the
/// well-formedness bound is defence in depth rather than the sole gate:
/// drop it and this vector is refused by the encoder's own `expect`
/// instead, a crash where there was a `False`. What the negative pins is
/// the refusal, which is the property the machine needs.
test finish_witness_tail_layout_is_pinned() {
  let witness_cbor = finish_fixture().transition.work_witness_cbor
  let length = bytearray.length(witness_cbor)
  bytearray.slice(witness_cbor, length - 7, length - 1) == #"00000040400000"
}

fn witness_with_cursor_past_the_execution_count(
  witness_cbor: ByteArray,
) -> ByteArray {
  let length = bytearray.length(witness_cbor)
  bytearray.take(witness_cbor, length - 7)
    |> bytearray.concat(#"01")
    |> bytearray.concat(bytearray.drop(witness_cbor, length - 6))
}

test finish_validator_refuses_a_cek_cursor_past_the_execution_count() fail {
  let honest = finish_fixture()
  let work_witness_cbor = witness_with_cursor_past_the_execution_count(
    honest.transition.work_witness_cbor,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..honest.pre,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      40,
      work_witness_cbor,
    ),
  }
  let f = step_fixture(
    0,
    pre,
    work_witness_cbor,
    honest.transition.claimed_successor,
    NoAuxiliaryWitness,
    NoCekMaterial,
  )
  run_finish(prepared(f), f.transition)
}

/// Sanity on the fixtures themselves: every honest step is accepted by the
/// aggregate evidence route the machine defines, so a validator that
/// accepts it is agreeing with the machine and not with a broken vector.
test every_fixture_is_an_honest_machine_step() {
  let door = field_door.MachineFieldDoorV1 {
    reference_inputs: [],
    certificate_policy_id: #"",
  }
  and {
    vm_one_step.verify_cek_one_step_v1(
      finish_fixture().pre,
      ValidationOneStepEvidenceV1 {
        transition: finish_fixture().transition,
        auxiliary: NoAuxiliaryWitness,
      },
      door,
    ),
    vm_one_step.verify_cek_one_step_v1(
      selection_fixture().pre,
      ValidationOneStepEvidenceV1 {
        transition: selection_fixture().transition,
        auxiliary: selection_fixture().auxiliary,
      },
      door,
    ),
    vm_one_step.verify_cek_one_step_v1(
      context_step_fixture().pre,
      ValidationOneStepEvidenceV1 {
        transition: context_step_fixture().transition,
        auxiliary: NoAuxiliaryWitness,
      },
      door,
    ),
    vm_one_step.verify_cek_one_step_v1(
      core_step_fixture().pre,
      ValidationOneStepEvidenceV1 {
        transition: core_step_fixture().transition,
        auxiliary: core_step_fixture().auxiliary,
      },
      door,
    ),
  }
}
