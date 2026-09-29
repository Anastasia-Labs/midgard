use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use midgard/cek_context_step as bounded_context
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardExecutionUnits, MidgardRedeemerWitness, MidgardTransaction,
  MidgardTransactionWitnessSet, MidgardVersionedScript, PlutusV3Script,
  SpendRedeemer, native_tx_version_v1,
}
use midgard/ledger_output_commitment_v1
use midgard/redeemer_item_proof_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/script_sources_descriptor as descriptor_claim
use midgard/validation_machine/cek as vm_cek
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/native_scripts as vm_native_scripts
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/script_sources as vm_script_sources
use midgard/validation_machine/script_sources_late_stages as vm_script_sources_late_stages
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/value_and_mint as vm_value_and_mint
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_machine_tests/constants.{
  h_28_a, h_28_b, h_a, h_b, h_c, replayed_input_key, validation_context_cbor,
  zero,
}
use midgard/validation_machine_tests/fixture_types.{
  CekKindFixtureV1, ValueAndMintKindFixtureV1,
}
use midgard/validation_machine_tests/fixtures_cek.{
  cek_active_core_step_fixture, maximum_program_envelope_selection_fixture,
}
use midgard/validation_machine_tests/fixtures_ledger.{
  value_output_descriptor_fixture,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  cek_context_native_control_fixture, cek_state_fixture,
  empty_cek_context_control_fixture, empty_native_control_fixture,
  empty_v1_transaction, forced_pending_state, inline_field_door,
  native_scripts_step_fixture, one_step_evidence, script_discovery_step_fixture,
  singleton_root, v1_program_envelope,
}
use midgard/validation_machine_tests/fixtures_script_identity.{
  stage_routes_agree,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// --- stage 10 -------------------------------------------------------------

/// The stage-10 "missing redeemer" fixture: one redeemer in the witness set,
/// the discovery scan already past it, so `redeemer_cursor == redeemer_count`
/// and the only legal step is the `E_MISSING_REQUIRED_WITNESS` rejection.
pub fn stage_ten_missing_fixture(
  redeemer_item_control_hash: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
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
  script_discovery_step_fixture(
    tx,
    10,
    singleton_frontier,
    singleton_frontier,
    purpose_frontier,
    vm_machine_types.ScriptDiscoveryControlV1 {
      ..vm_witness.empty_script_discovery_control(),
      source_cursor: 1,
      redeemer_cursor: 1,
      current_purpose_kind: 0,
      current_purpose_index: 1,
      current_script_hash: h_28_a,
      current_subject: #"0102",
      matched_source_index: 0,
      matched_language_tag: 3,
      matched_source_leaf: redeemer_leaf,
      redeemer_item_control_hash,
    },
    31,
  )
}

// --- stage 11 / 12 --------------------------------------------------------

/// One inline PlutusV3 source that has already been discovered and executed;
/// the stage-11 audit and the stage-12 hand-off both run over this shape.
pub fn stage_eleven_audit_discovery(
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
  source_cursor: Int,
  redeemer_item_control_hash: ByteArray,
) -> vm_machine_types.ScriptDiscoveryControlV1 {
  vm_machine_types.ScriptDiscoveryControlV1 {
    ..vm_witness.empty_script_discovery_control(),
    purpose_cursor: 1,
    source_cursor,
    used_inline_bitmap: 1,
    execution_count: 1,
    execution_peaks,
    redeemer_item_control_hash,
  }
}

/// The stage-12 hand-off fixture, parameterised by the dangling redeemer item
/// control hash so the differential probe below can reuse it. Returns the
/// pre-state, its work witness, the canonical NativeScripts successor and a
/// mutated successor that starts one execution late.
pub fn stage_twelve_finish_fixture(
  redeemer_item_control_hash: ByteArray,
) -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  Pair<
    ByteArray,
    Pair<
      validation_trace_v1.ValidationMachineStateV1,
      validation_trace_v1.ValidationMachineStateV1,
    >,
  >,
> {
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
  let Pair(pre, work_witness_cbor) = script_discovery_step_fixture(
    tx,
    12,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    stage_eleven_audit_discovery(
      execution_frontier.peaks,
      1,
      redeemer_item_control_hash,
    ),
    32,
  )
  let Pair(post, _) = native_scripts_step_fixture(
    tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    33,
  )
  // Mutated successor: the NativeScripts hand-off starts one execution late.
  let Pair(mutated_post, _) = native_scripts_step_fixture(
    tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    1,
    0,
    33,
  )
  Pair(pre, Pair(work_witness_cbor, Pair(post, mutated_post)))
}

// ---------------------------------------------------------------------------
// LATENT HAZARD -- dangling `redeemer_item_control_hash`.
//
// These three are NOT route-agreement controls and they are NOT a defect
// report. They are latent-hazard notes: they pin a real asymmetry between the
// two implementations of one rule, on states the machine provably cannot
// produce, so that the asymmetry is visible to whoever next touches the narrow
// stage wrappers.
//
// THE ASYMMETRY.
// The generic route binds a ScriptSources control with
// `script_sources_control_is_bound`, which calls
// `script_discovery_stage_is_well_formed` (validation-machine-v1.ak:11862).
// That predicate forbids a non-empty `discovery.redeemer_item_control_hash` at
// stages 8, 9 and 11 (:11881, :11892, :11922), and `script_sources_stage_ten`
// (:11094) / `script_sources_stage_twelve` (:11644) re-impose the same rule as
// an explicit conjunct in their terminal branches. None of the deployed
// per-stage binders
// (`script_sources_stage_{eight,ten,twelve}_control_is_bound`, :10457, :11267,
// :11788) carries that conjunct, and none of the narrow terminal wrappers
// restates it. So on a state carrying a pending hash the narrow route ACCEPTS
// where the generic route REJECTS -- the narrow route is strictly wider. Each
// test below pins exactly that: `narrow_route_is_wider_than_generic` asserts
// the deployed operand is True AND the generic operand is False, on a fixture
// that differs from its passing clean-hash sibling
// (`stage_differential_{eight_finish,ten_missing,twelve_finish}_routes_agree`)
// in that one field alone.
//
// WHY IT IS NOT EXPLOITABLE TODAY (reachability).
// `discovery.redeemer_item_control_hash` is set to a non-empty value in exactly
// four places -- :11142 and :11235 in `script_sources_stage_ten`, :11693 and
// :11756 in `script_sources_stage_twelve`. All four sit under the branch guard
// `discovery.redeemer_cursor != control.redeemer_count` and all four leave
// `redeemer_cursor` unchanged. It is cleared at :11218 and :11740 (the cursor
// advances past the item) and by `reset_script_discovery_current` (:10367),
// which is the only way stages 8 and 11 are ever entered (:10386, :10527,
// :10682, :11185); stage 7's hand-off into stage 8 and stage 11's hand-off into
// stage 12 (:11422) carry an already-empty hash because
// `script_discovery_stage_is_well_formed` forbids a pending hash at stages 8, 9
// and 11 on the pre-state. The deployed narrow validators that produce
// discovery successors (`script-sources-stage-{eight-purpose,nine-*,eleven-*}-semantic-v1`)
// only propagate `..discovery` and never write the field, and the two
// redeemer-scan validators that do write it
// (`script-sources-stage-ten-mismatch-semantic-v1`,
// `script-sources-stage-twelve-redeemer-semantic-v1`) call the GENERIC
// dispatcher, so they carry the guard. Hence, for every state the machine can
// produce:
//   * stage 8, 9 or 11  =>  `redeemer_item_control_hash == #""`;
//   * stage 10 or 12 with a pending hash  =>  `redeemer_cursor < redeemer_count`.
// Each narrow wrapper below only applies in the opposite regime -- stage-8
// finish is a stage-8 state; stage-10 missing and stage-12 finish both require
// `redeemer_cursor == redeemer_count` -- so no reachable pre-state can reach
// the divergence. There is no reachable-state variant of these probes to build:
// in the reachable pending-hash regime (stage 10/12, cursor < count) the rule is
// implemented only once, by the generic dispatcher, so there is no second
// implementation to differentially test.
//
// THE HAZARD, AND WHAT BREAKS THESE TESTS.
// The divergence becomes live the moment a narrow successor-splice wrapper is
// added for a stage-10/12 redeemer-scan sub-case, or any narrow wrapper is
// allowed to hand off a state that still carries a pending hash. At that point
// the narrow route accepts ScriptSources steps the canonical machine forbids,
// which is an acceptance-widening soundness break of the dispute game. These
// tests are the tripwire: they FAIL if the narrow binders gain the missing
// conjunct (the correct fix -- delete the note then), and they FAIL if the
// generic route loses it. Do not relax them to `deployed == generic`; that
// assertion is false today and asserts nothing about which side moved.
// ---------------------------------------------------------------------------

/// The deployed narrow route accepts and the generic route rejects. Both
/// operands are pinned, so neither side can drift without failing the test.
pub fn narrow_route_is_wider_than_generic(deployed: Bool, generic: Bool) -> Bool {
  and {
    deployed,
    !generic,
  }
}

// ====================================================================// R5 item 1 (#617) -- cek and ValueAndMint per-kind route agreement controls.
//
// Indices 11 (`cek`) and 12 (`valueAndMint`) used to be served by one
// aggregate resolver each. They are now `prepare_selected` plus one semantic
// resolver per auxiliary kind, and each of those validators calls a narrow
// wrapper -- `verify_cek_<kind>_semantics_v1` (finish, execution selection,
// context step, core step) or `verify_value_and_mint_<kind>_semantics_v1`
// (the eleven stage/cursor kinds) -- that pins its own discriminator and then
// runs the very branch the aggregate dispatcher (`verify_cek_one_step_v1`,
// `verify_value_and_mint_one_step_v1`) would have taken.
//
// As with the ScriptSources controls above, every honest fixture is run
// through both routes and the verdicts must agree, on the accepting step and
// on a mutated successor. The second property is the one the R5 totality
// verifier relies on at 11/12: every wrapper must refuse every OTHER kind's
// honest step with a clean `False`, so the kinds partition the phase's step
// space exactly -- no step is claimable under two resolvers, and none is
// claimable under none.
//
// Kind numbering follows the deployed semantic-resolver group order
// (`VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES.semantics` in the SDK).
// ====================================================================
pub fn work_root_mutated(
  witness: vm_machine_types.ValidationOneStepWitnessV1,
) -> vm_machine_types.ValidationOneStepWitnessV1 {
  vm_machine_types.ValidationOneStepWitnessV1 {
    ..witness,
    claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
      ..witness.claimed_successor,
      work_root: h_b,
    },
  }
}

/// Kind 0: the stand-alone hand-off to ValueAndMint. A transaction with no
/// execution at all (`execution_count == 0`) has nothing to select, so its
/// whole cek phase is this one step.
pub fn cek_finish_kind_fixture() -> CekKindFixtureV1 {
  let tx = empty_v1_transaction()
  let control = empty_native_control_fixture(tx)
  let Pair(pre, work_witness_cbor) = cek_state_fixture(tx, control, #"", 0, 40)
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
  CekKindFixtureV1 {
    kind: 0,
    pre,
    evidence: one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  }
}

/// Kind 1: a PlutusV3 execution selection.
pub fn cek_execution_selection_kind_fixture() -> CekKindFixtureV1 {
  let f = maximum_program_envelope_selection_fixture()
  CekKindFixtureV1 { kind: 1, pre: f.pre, evidence: f.evidence }
}

/// Kind 2: a context step -- the mint-context initialisation over a
/// transaction that mints nothing, which is the smallest context step with
/// no auxiliary at all.
pub fn cek_context_step_kind_fixture() -> CekKindFixtureV1 {
  let tx = empty_v1_transaction()
  let control = cek_context_native_control_fixture(tx)
  expect Some(observer_summary) = script_context_v1.observer_collection_summary_v1(
    [],
    False,
  )
  let init_control = vm_machine_types.CekContextControlV1 {
    ..empty_cek_context_control_fixture(6),
    observer_summary,
  }
  let successor_control = vm_machine_types.CekContextControlV1 {
    ..init_control,
    stage: 9,
    mint_summary: cek_data_v1.map_data_summary_v1(
      cek_data_v1.empty_data_pair_summary_v1(),
    ),
  }
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(init_control),
    0,
    47,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(successor_control),
    0,
    48,
  )
  CekKindFixtureV1 {
    kind: 2,
    pre,
    evidence: one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  }
}

/// Kind 3: an active core step.
pub fn cek_core_step_kind_fixture() -> CekKindFixtureV1 {
  let Pair(pre, evidence) = cek_active_core_step_fixture()
  CekKindFixtureV1 { kind: 3, pre, evidence }
}

pub fn cek_kind_fixtures() -> List<CekKindFixtureV1> {
  [
    cek_finish_kind_fixture(),
    cek_execution_selection_kind_fixture(),
    cek_context_step_kind_fixture(),
    cek_core_step_kind_fixture(),
  ]
}

/// The deployed narrow route for cek kind `kind`, fed the auxiliary the
/// kind's validator reconstructs from its action fields.
pub fn cek_kind_route(
  kind: Int,
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: vm_machine_types.ValidationOneStepWitnessV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> Bool {
  if kind == 0 {
    vm_cek.verify_cek_finish_semantics_v1(pre, witness)
  } else if kind == 1 {
    vm_cek.verify_cek_execution_selection_semantics_v1(pre, witness, auxiliary)
  } else if kind == 2 {
    vm_cek.verify_cek_context_step_semantics_v1(
      pre,
      witness,
      auxiliary,
      inline_field_door(),
    )
  } else {
    expect vm_machine_types.CekCoreStepWitness { step } = auxiliary
    vm_cek.verify_cek_core_step_semantics_v1(pre, witness, step)
  }
}

pub fn cek_aggregate_route(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: vm_machine_types.ValidationOneStepWitnessV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> Bool {
  vm_one_step.verify_cek_one_step_v1(
    pre,
    vm_machine_types.ValidationOneStepEvidenceV1 {
      transition: witness,
      auxiliary,
    },
    inline_field_door(),
  )
}

pub fn cek_kind_routes_agree(f: CekKindFixtureV1) -> Bool {
  let witness = f.evidence.transition
  let auxiliary = f.evidence.auxiliary
  and {
    stage_routes_agree(
      cek_kind_route(f.kind, f.pre, witness, auxiliary),
      cek_aggregate_route(f.pre, witness, auxiliary),
      True,
    ),
    stage_routes_agree(
      cek_kind_route(f.kind, f.pre, work_root_mutated(witness), auxiliary),
      cek_aggregate_route(f.pre, work_root_mutated(witness), auxiliary),
      False,
    ),
  }
}

/// A ValueAndMint control at `stage` with every cursor at zero, every
/// accumulator initial and no replay schedule.
pub fn idle_value_and_mint_control(
  native_control: vm_machine_types.NativeScriptsControlV1,
  stage: Int,
) -> vm_machine_types.ValueAndMintControlV1 {
  vm_machine_types.ValueAndMintControlV1 {
    native_control,
    stage,
    replay_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 0,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
}

pub fn value_and_mint_state_fixture(
  tx: MidgardTransaction,
  control: vm_machine_types.ValueAndMintControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(control)
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.ValueAndMint,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

/// One ValueAndMint-to-ValueAndMint step from `control` to `next_control`.
pub fn value_and_mint_kind_fixture(
  kind: Int,
  tx: MidgardTransaction,
  control: vm_machine_types.ValueAndMintControlV1,
  next_control: vm_machine_types.ValueAndMintControlV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> ValueAndMintKindFixtureV1 {
  let Pair(pre, work_witness_cbor) = value_and_mint_state_fixture(
    tx,
    control,
    40,
  )
  let Pair(post, _) = value_and_mint_state_fixture(tx, next_control, 41)
  ValueAndMintKindFixtureV1 {
    kind,
    pre,
    evidence: one_step_evidence(work_witness_cbor, post, auxiliary),
  }
}

pub fn replayed_input_descriptor(
  asset_count: Int,
) -> ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
  let asset_frontier = if asset_count == 0 {
    validation_merkle_v1.build_frontier([])
  } else {
    validation_merkle_v1.build_frontier(
      [ledger_output_commitment_v1.asset_leaf_hash(h_28_a, #"beef", 7)],
    )
  }
  let empty_summary = cek_data_v1.DataSummaryV1 {
    root: h_a,
    cbor_length: 0,
    memory: 0,
  }
  ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: 0,
    total_length: 16384,
    item_commitment: h_b,
    address: #"60" |> bytearray.concat(h_28_b),
    lovelace: 3,
    asset_count,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
      asset_frontier.count,
      asset_frontier.peaks,
    ),
    cardano_value_size: 5000,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: empty_summary,
    midgard_tx_out: empty_summary,
    cardano_spend_datum: empty_summary,
  }
}

/// A native control whose single resolved input is `replayed_input_key`
/// resolving to `descriptor_cbor`, with the matching one-node schedule.
pub fn single_replayed_input_native_control(
  tx: MidgardTransaction,
  descriptor_cbor: ByteArray,
) -> vm_machine_types.NativeScriptsControlV1 {
  vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolved_input_count: 1,
    spend_input_count: 1,
    resolved_item_peaks: validation_merkle_v1.build_frontier([h_a]).peaks,
    resolved_inputs_accumulator: vm_witness.resolved_input_accumulator_successor(
      vm_witness.initial_resolution_accumulator(),
      0,
      replayed_input_key,
      descriptor_cbor,
    ),
    resolution_schedule_hash: vm_witness.resolution_schedule_node_hash(
      0,
      replayed_input_key,
      vm_witness.empty_resolution_schedule_hash(),
    ),
  }
}

/// Kind 0: `begin` (stage 0 -> 1) installs the native resolution schedule as
/// the replay schedule.
pub fn value_and_mint_begin_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolution_schedule_hash: h_a,
  }
  let control = idle_value_and_mint_control(native_control, 0)
  value_and_mint_kind_fixture(
    0,
    tx,
    control,
    vm_machine_types.ValueAndMintControlV1 {
      ..control,
      stage: 1,
      replay_schedule_hash: h_a,
      replay_remaining_schedule_hash: h_a,
    },
    vm_machine_types.NoAuxiliaryWitness,
  )
}

/// Kind 1: `replay_begin` (stage 1 -> 2).
pub fn value_and_mint_replay_begin_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let control = idle_value_and_mint_control(empty_native_control_fixture(tx), 1)
  value_and_mint_kind_fixture(
    1,
    tx,
    control,
    vm_machine_types.ValueAndMintControlV1 { ..control, stage: 2 },
    vm_machine_types.NoAuxiliaryWitness,
  )
}

/// Kind 2: `replay_input` (stage 2, schedule pending, asset cursor 0) over an
/// asset-free descriptor, which completes the input in one step.
pub fn value_and_mint_replay_input_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let descriptor_cbor = ledger_output_commitment_v1.encode(
    replayed_input_descriptor(0),
  )
  let native_control = single_replayed_input_native_control(tx, descriptor_cbor)
  let control = vm_machine_types.ValueAndMintControlV1 {
    ..idle_value_and_mint_control(native_control, 2),
    replay_schedule_hash: native_control.resolution_schedule_hash,
    replay_remaining_schedule_hash: native_control.resolution_schedule_hash,
  }
  value_and_mint_kind_fixture(
    2,
    tx,
    control,
    vm_machine_types.ValueAndMintControlV1 {
      ..control,
      replay_cursor: 1,
      replay_accumulator: native_control.resolved_inputs_accumulator,
      replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      value_accumulator: vm_machine_types.ValueAccumulatorV1 {
        ..control.value_accumulator,
        lovelace_delta: 3,
      },
    },
    vm_machine_types.ResolvedInputReplayWitness {
      source_kind: 0,
      key: replayed_input_key,
      next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      value: descriptor_cbor,
    },
  )
}

/// Kind 3: `replay_asset` (stage 2, asset cursor > 0) folding the single
/// asset of a one-asset descriptor, which also completes the input.
pub fn value_and_mint_replay_asset_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let descriptor = replayed_input_descriptor(1)
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let native_control = single_replayed_input_native_control(tx, descriptor_cbor)
  let unit = h_28_a |> bytearray.concat(#"beef")
  let control = vm_machine_types.ValueAndMintControlV1 {
    ..idle_value_and_mint_control(native_control, 2),
    replay_schedule_hash: native_control.resolution_schedule_hash,
    replay_remaining_schedule_hash: native_control.resolution_schedule_hash,
    replay_asset_cursor: 1,
    replay_value_hash: blake2b_256(descriptor_cbor),
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 3,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
  value_and_mint_kind_fixture(
    3,
    tx,
    control,
    vm_machine_types.ValueAndMintControlV1 {
      ..control,
      replay_cursor: 1,
      replay_asset_cursor: 0,
      replay_value_hash: zero,
      replay_accumulator: native_control.resolved_inputs_accumulator,
      replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      value_accumulator: vm_machine_types.ValueAccumulatorV1 {
        lovelace_delta: 3,
        asset_root: singleton_root(unit, cbor.serialise(7)),
        seen_asset_count: 1,
        nonzero_asset_count: 1,
      },
    },
    vm_machine_types.ValueInputAssetWitness {
      source_kind: 0,
      key: replayed_input_key,
      next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
      descriptor_cbor,
      asset_index: 0,
      policy_id: h_28_a,
      asset_name: #"beef",
      quantity: 7,
      asset_peaks: validation_merkle_v1.build_frontier(
        [ledger_output_commitment_v1.asset_leaf_hash(h_28_a, #"beef", 7)],
      ).peaks,
      asset_siblings: [],
      mutation: vm_machine_types.ValueAssetMutationWitnessV1 {
        delta_was_present: False,
        old_delta: 0,
        delta_proof: [],
      },
    },
  )
}

/// Kind 4: `replay_finish` (stage 2, schedule exhausted -> stage 3).
pub fn value_and_mint_replay_finish_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let control = idle_value_and_mint_control(empty_native_control_fixture(tx), 2)
  value_and_mint_kind_fixture(
    4,
    tx,
    control,
    vm_machine_types.ValueAndMintControlV1 { ..control, stage: 3 },
    vm_machine_types.NoAuxiliaryWitness,
  )
}

/// Kind 5: `output_descriptor` (stage 3, outputs pending, asset cursor 0).
pub fn value_and_mint_output_descriptor_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let f = value_output_descriptor_fixture()
  ValueAndMintKindFixtureV1 {
    kind: 5,
    pre: f.pre,
    evidence: one_step_evidence(
      f.initial_work,
      f.active_state,
      vm_machine_types.ValueOutputDescriptorWitness {
        output_index: 0,
        descriptor_cbor: f.descriptor_cbor,
        siblings: [],
      },
    ),
  }
}

/// Kind 6: `output_asset` (stage 3, asset cursor > 0).
pub fn value_and_mint_output_asset_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let f = value_output_descriptor_fixture()
  ValueAndMintKindFixtureV1 {
    kind: 6,
    pre: f.active_state,
    evidence: one_step_evidence(
      f.active_work,
      f.final_state,
      vm_machine_types.ValueOutputAssetWitness {
        output_index: 0,
        descriptor_cbor: f.descriptor_cbor,
        asset_index: 0,
        policy_id: f.policy_id,
        asset_name: f.asset_name,
        quantity: f.quantity,
        asset_peaks: f.asset_peaks,
        asset_siblings: [],
        mutation: vm_machine_types.ValueAssetMutationWitnessV1 {
          delta_was_present: False,
          old_delta: 0,
          delta_proof: [],
        },
      },
    ),
  }
}

/// Kind 7: `output_finish` (stage 3, outputs exhausted -> stage 4).
pub fn value_and_mint_output_finish_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let control = idle_value_and_mint_control(empty_native_control_fixture(tx), 3)
  value_and_mint_kind_fixture(
    7,
    tx,
    control,
    vm_machine_types.ValueAndMintControlV1 { ..control, stage: 4 },
    vm_machine_types.NoAuxiliaryWitness,
  )
}

/// Kind 8: `mint_asset` (stage 4, mints pending) -- the signed burn leaf
/// replay from `value_and_mint_replays_signed_burn_leaf_on_l1`.
pub fn value_and_mint_mint_asset_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let policy_id = #"11111111111111111111111111111111111111111111111111111111"
  let asset_name = #"beef"
  let unit = policy_id |> bytearray.concat(asset_name)
  let mint_frontier = validation_merkle_v1.build_frontier(
    [vm_shared.mint_asset_leaf_hash(policy_id, asset_name, -5)],
  )
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = vm_machine_types.ValueAndMintControlV1 {
    ..idle_value_and_mint_control(native_control, 4),
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: singleton_root(unit, cbor.serialise(5)),
      seen_asset_count: 1,
      nonzero_asset_count: 1,
    },
  }
  value_and_mint_kind_fixture(
    8,
    tx,
    control,
    vm_machine_types.ValueAndMintControlV1 {
      ..control,
      mint_cursor: 1,
      value_accumulator: vm_machine_types.ValueAccumulatorV1 {
        lovelace_delta: 0,
        asset_root: singleton_root(unit, cbor.serialise(0)),
        seen_asset_count: 1,
        nonzero_asset_count: 0,
      },
    },
    vm_machine_types.ValueMintAssetWitness {
      mint_index: 0,
      policy_id,
      asset_name,
      quantity: -5,
      siblings: [],
      mutation: vm_machine_types.ValueAssetMutationWitnessV1 {
        delta_was_present: True,
        old_delta: 5,
        delta_proof: [],
      },
    },
  )
}

/// Kind 9: `mint_finish` (stage 4, mints exhausted -> stage 5).
pub fn value_and_mint_mint_finish_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let control = idle_value_and_mint_control(empty_native_control_fixture(tx), 4)
  value_and_mint_kind_fixture(
    9,
    tx,
    control,
    vm_machine_types.ValueAndMintControlV1 { ..control, stage: 5 },
    vm_machine_types.NoAuxiliaryWitness,
  )
}

/// Kind 10: `finalize` (stage 5 -> LedgerDelta) on a settled accumulator.
pub fn value_and_mint_finalize_kind_fixture() -> ValueAndMintKindFixtureV1 {
  let tx = empty_v1_transaction()
  let native_control = empty_native_control_fixture(tx)
  let control = idle_value_and_mint_control(native_control, 5)
  let Pair(pre, work_witness_cbor) = value_and_mint_state_fixture(
    tx,
    control,
    40,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.LedgerDelta,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      41,
      vm_witness.encode_ledger_delta_witness_v1(
        native_control,
        pre.prior_ledger_root,
        control.replay_schedule_hash,
      ),
    ),
  }
  ValueAndMintKindFixtureV1 {
    kind: 10,
    pre,
    evidence: one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  }
}

pub fn value_and_mint_kind_fixtures() -> List<ValueAndMintKindFixtureV1> {
  [
    value_and_mint_begin_kind_fixture(),
    value_and_mint_replay_begin_kind_fixture(),
    value_and_mint_replay_input_kind_fixture(),
    value_and_mint_replay_asset_kind_fixture(),
    value_and_mint_replay_finish_kind_fixture(),
    value_and_mint_output_descriptor_kind_fixture(),
    value_and_mint_output_asset_kind_fixture(),
    value_and_mint_output_finish_kind_fixture(),
    value_and_mint_mint_asset_kind_fixture(),
    value_and_mint_mint_finish_kind_fixture(),
    value_and_mint_finalize_kind_fixture(),
  ]
}

/// The deployed narrow route for ValueAndMint kind `kind`, fed the auxiliary
/// fields the kind's validator carries in its action.
pub fn value_and_mint_kind_route(
  kind: Int,
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: vm_machine_types.ValidationOneStepWitnessV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> Bool {
  if kind == 0 {
    vm_value_and_mint.verify_value_and_mint_begin_semantics_v1(pre, witness)
  } else if kind == 1 {
    vm_value_and_mint.verify_value_and_mint_replay_begin_semantics_v1(
      pre,
      witness,
    )
  } else if kind == 2 {
    expect
        vm_machine_types.ResolvedInputReplayWitness {
          source_kind,
          key,
          next_schedule_hash,
          value,
        }
      = auxiliary
    vm_value_and_mint.verify_value_and_mint_replay_input_semantics_v1(
      pre,
      witness,
      source_kind,
      key,
      next_schedule_hash,
      value,
    )
  } else if kind == 3 {
    expect
        vm_machine_types.ValueInputAssetWitness {
          source_kind,
          key,
          next_schedule_hash,
          descriptor_cbor,
          asset_index,
          policy_id,
          asset_name,
          quantity,
          asset_peaks,
          asset_siblings,
          mutation,
        }
      = auxiliary
    vm_value_and_mint.verify_value_and_mint_replay_asset_semantics_v1(
      pre,
      witness,
      source_kind,
      key,
      next_schedule_hash,
      descriptor_cbor,
      asset_index,
      policy_id,
      asset_name,
      quantity,
      asset_peaks,
      asset_siblings,
      mutation,
    )
  } else if kind == 4 {
    vm_value_and_mint.verify_value_and_mint_replay_finish_semantics_v1(
      pre,
      witness,
    )
  } else if kind == 5 {
    expect
        vm_machine_types.ValueOutputDescriptorWitness {
          output_index,
          descriptor_cbor,
          siblings,
        }
      = auxiliary
    vm_value_and_mint.verify_value_and_mint_output_descriptor_semantics_v1(
      pre,
      witness,
      output_index,
      descriptor_cbor,
      siblings,
    )
  } else if kind == 6 {
    expect
        vm_machine_types.ValueOutputAssetWitness {
          output_index,
          descriptor_cbor,
          asset_index,
          policy_id,
          asset_name,
          quantity,
          asset_peaks,
          asset_siblings,
          mutation,
        }
      = auxiliary
    vm_value_and_mint.verify_value_and_mint_output_asset_semantics_v1(
      pre,
      witness,
      output_index,
      descriptor_cbor,
      asset_index,
      policy_id,
      asset_name,
      quantity,
      asset_peaks,
      asset_siblings,
      mutation,
    )
  } else if kind == 7 {
    vm_value_and_mint.verify_value_and_mint_output_finish_semantics_v1(
      pre,
      witness,
    )
  } else if kind == 8 {
    expect
        vm_machine_types.ValueMintAssetWitness {
          mint_index,
          policy_id,
          asset_name,
          quantity,
          siblings,
          mutation,
        }
      = auxiliary
    vm_value_and_mint.verify_value_and_mint_mint_asset_semantics_v1(
      pre,
      witness,
      mint_index,
      policy_id,
      asset_name,
      quantity,
      siblings,
      mutation,
    )
  } else if kind == 9 {
    vm_value_and_mint.verify_value_and_mint_mint_finish_semantics_v1(
      pre,
      witness,
    )
  } else {
    vm_value_and_mint.verify_value_and_mint_finalize_semantics_v1(pre, witness)
  }
}

pub fn value_and_mint_aggregate_route(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: vm_machine_types.ValidationOneStepWitnessV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> Bool {
  vm_one_step.verify_value_and_mint_one_step_v1(
    pre,
    vm_machine_types.ValidationOneStepEvidenceV1 {
      transition: witness,
      auxiliary,
    },
  )
}

pub fn value_and_mint_kind_routes_agree(f: ValueAndMintKindFixtureV1) -> Bool {
  let witness = f.evidence.transition
  let auxiliary = f.evidence.auxiliary
  and {
    stage_routes_agree(
      value_and_mint_kind_route(f.kind, f.pre, witness, auxiliary),
      value_and_mint_aggregate_route(f.pre, witness, auxiliary),
      True,
    ),
    stage_routes_agree(
      value_and_mint_kind_route(
        f.kind,
        f.pre,
        work_root_mutated(witness),
        auxiliary,
      ),
      value_and_mint_aggregate_route(
        f.pre,
        work_root_mutated(witness),
        auxiliary,
      ),
      False,
    ),
  }
}

/// Compare the decomposed context leaf with the original admitted transition.
pub fn verify_bounded_context_parity(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: vm_machine_types.ValidationOneStepEvidenceV1,
  door: field_door.MachineFieldDoorV1,
) -> Bool {
  if !vm_one_step.verify_one_step_evidence(pre, evidence, door) {
    False
  } else {
    let cek = vm_witness.cek_witness_control_v1(evidence.transition)
    let context = vm_witness.cek_context_control_from_cbor(
      cek.context_control_cbor,
    )
    let native = bounded_context.native_facts(cek.control)
    let auxiliary: Data = evidence.auxiliary
    let next_work = if context.stage == 13 {
      vm_witness.encode_cek_witness_v1(
        cek.control,
        #"",
        cek.execution_cursor,
        cek.completed_cpu,
        cek.completed_memory,
        bounded_context.seed_hash(context, cek.execution_cursor, auxiliary),
        context.execution_cpu_limit,
        context.execution_memory_limit,
        cek.program_envelope_hash,
      )
    } else {
      let next = when context.stage is {
        1 -> bounded_context.next_resolved(context, native, auxiliary, 1)
        2 -> bounded_context.next_resolved(context, native, auxiliary, 0)
        3 -> bounded_context.next_output(context, native, auxiliary)
        4 -> bounded_context.next_signer(context, native, auxiliary)
        6 -> bounded_context.next_mint_init(context, native, auxiliary)
        8 -> bounded_context.next_mint_item(context, native, auxiliary)
        11 ->
          bounded_context.next_assemble(context, pre.transaction_id, auxiliary)
        12 ->
          bounded_context.next_tx_info(
            pre.source_kind,
            context,
            native,
            auxiliary,
          )
        _ -> fail
      }
      vm_witness.encode_cek_witness_v1(
        cek.control,
        vm_witness.encode_cek_context_control_v1(next),
        cek.execution_cursor,
        cek.completed_cpu,
        cek.completed_memory,
        #"",
        0,
        0,
        next.program_envelope_hash,
      )
    }
    evidence.transition.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      pre.program_counter + 1,
      next_work,
    )
  }
}

pub fn verify_descriptor_ten_match_parity(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: vm_machine_types.ValidationOneStepWitnessV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> Bool {
  let original = vm_script_sources.verify_script_sources_stage_ten_match_semantics_v1(
    pre,
    witness,
    auxiliary,
  )
  if original {
    let split = when auxiliary is {
      vm_machine_types.RedeemerItemStepWitness {
        control,
        witness: item_witness,
        ..
      } -> {
        expect
            Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
              control: next,
            })
          = redeemer_item_proof_v1.step_v1(control, item_witness)
        expect Some(chunk_proof) = item_witness.chunk_proof
        let claim = descriptor_claim.DescriptorStepClaim {
          control: descriptor_claim.narrow_control(control),
          open_tail: item_witness.action == redeemer_item_proof_v1.RedeemerItemOpenTail,
          chunk_proof,
          next_chunk_proof: item_witness.next_chunk_proof,
          claimed_next: descriptor_claim.narrow_control(next),
        }
        and {
          vm_script_sources_late_stages.verify_descriptor_step_claim(
            witness,
            claim,
          ),
          vm_script_sources_late_stages.verify_descriptor_match(
            pre,
            witness,
            claim,
          ),
          !vm_script_sources_late_stages.verify_descriptor_step_claim(
            witness,
            descriptor_claim.DescriptorStepClaim {
              ..claim,
              claimed_next: descriptor_claim.DescriptorControl {
                ..claim.claimed_next,
                execution_steps: claim.claimed_next.execution_steps + 1,
              },
            },
          ),
        }
      }
      _ -> False
    }
    expect split
    original
  } else {
    False
  }
}

pub fn verify_descriptor_ten_mismatch_parity(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: vm_machine_types.ValidationOneStepWitnessV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> Bool {
  let original = vm_script_sources.verify_script_sources_stage_ten_mismatch_semantics_v1(
    pre,
    witness,
    auxiliary,
  )
  if original {
    let split = when auxiliary is {
      vm_machine_types.RedeemerScanBeginWitness {
        item_index,
        item_count,
        total_length,
        item_commitment,
        siblings,
      } ->
        vm_script_sources_late_stages.verify_descriptor_begin_ten(
          pre,
          witness,
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        )
      vm_machine_types.RedeemerItemStepWitness {
        control,
        witness: item_witness,
        ..
      } -> {
        expect
            Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
              control: next,
            })
          = redeemer_item_proof_v1.step_v1(control, item_witness)
        expect Some(chunk_proof) = item_witness.chunk_proof
        let claim = descriptor_claim.DescriptorStepClaim {
          control: descriptor_claim.narrow_control(control),
          open_tail: item_witness.action == redeemer_item_proof_v1.RedeemerItemOpenTail,
          chunk_proof,
          next_chunk_proof: item_witness.next_chunk_proof,
          claimed_next: descriptor_claim.narrow_control(next),
        }
        and {
          vm_script_sources_late_stages.verify_descriptor_step_claim(
            witness,
            claim,
          ),
          vm_script_sources_late_stages.verify_descriptor_mismatch(
            pre,
            witness,
            claim,
          ),
          !vm_script_sources_late_stages.verify_descriptor_step_claim(
            witness,
            descriptor_claim.DescriptorStepClaim {
              ..claim,
              claimed_next: descriptor_claim.DescriptorControl {
                ..claim.claimed_next,
                execution_steps: claim.claimed_next.execution_steps + 1,
              },
            },
          ),
        }
      }
      _ -> False
    }
    expect split
    original
  } else {
    False
  }
}

pub fn verify_descriptor_twelve_redeemer_parity(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: vm_machine_types.ValidationOneStepWitnessV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> Bool {
  let original = vm_script_sources.verify_script_sources_stage_twelve_redeemer_semantics_v1(
    pre,
    witness,
    auxiliary,
  )
  if original {
    let split = when auxiliary is {
      vm_machine_types.RedeemerScanBeginWitness {
        item_index,
        item_count,
        total_length,
        item_commitment,
        siblings,
      } ->
        vm_script_sources_late_stages.verify_descriptor_begin_twelve(
          pre,
          witness,
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        )
      vm_machine_types.RedeemerItemStepWitness {
        control,
        witness: item_witness,
        ..
      } -> {
        expect
            Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced {
              control: next,
            })
          = redeemer_item_proof_v1.step_v1(control, item_witness)
        expect Some(chunk_proof) = item_witness.chunk_proof
        let claim = descriptor_claim.DescriptorStepClaim {
          control: descriptor_claim.narrow_control(control),
          open_tail: item_witness.action == redeemer_item_proof_v1.RedeemerItemOpenTail,
          chunk_proof,
          next_chunk_proof: item_witness.next_chunk_proof,
          claimed_next: descriptor_claim.narrow_control(next),
        }
        and {
          vm_script_sources_late_stages.verify_descriptor_step_claim(
            witness,
            claim,
          ),
          vm_script_sources_late_stages.verify_descriptor_used(
            pre,
            witness,
            claim,
          ),
          !vm_script_sources_late_stages.verify_descriptor_step_claim(
            witness,
            descriptor_claim.DescriptorStepClaim {
              ..claim,
              claimed_next: descriptor_claim.DescriptorControl {
                ..claim.claimed_next,
                execution_steps: claim.claimed_next.execution_steps + 1,
              },
            },
          ),
        }
      }
      _ -> False
    }
    expect split
    original
  } else {
    False
  }
}

pub fn effectful_descriptor_specialization_matches_general(
  language_tag: Int,
) -> Bool {
  let tx = empty_v1_transaction()
  let script_hash = bytearray.take(h_a, 28)
  let script_item_commitment = h_b
  let redeemer_leaf = h_c
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    #"0102",
  )
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    language_tag,
    script_hash,
    3,
    script_item_commitment,
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let redeemer_frontier = validation_merkle_v1.build_frontier([redeemer_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier(
    [
      script_proof_v1.execution_leaf_hash(
        language_tag,
        purpose_leaf,
        source_leaf,
        redeemer_leaf,
      ),
    ],
  )
  let Pair(pre, work_witness_cbor) = native_scripts_step_fixture(
    tx,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    30,
  )
  let Pair(post, _) = native_scripts_step_fixture(
    tx,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    execution_frontier,
    1,
    if language_tag == 3 {
      1
    } else {
      2
    },
    31,
  )
  let witness = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let auxiliary = vm_machine_types.NativeExecutionDescriptorWitness {
    execution_index: 0,
    language_tag,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash,
    subject: #"0102",
    purpose_siblings: [],
    source_index: 0,
    origin_kind: 0,
    source_key: #"00",
    script_total_length: 3,
    script_item_commitment,
    source_siblings: [],
    redeemer_leaf,
    execution_siblings: [],
    first_chunk_proof: None,
    signer_peaks: [],
  }
  let forged_successor = vm_machine_types.ValidationOneStepWitnessV1 {
    ..witness,
    claimed_successor: validation_trace_v1.ValidationMachineStateV1 {
      ..post,
      work_root: zero,
    },
  }
  let forged_source = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    transaction_commitment: zero,
  }
  and {
    vm_native_scripts.verify_native_scripts(pre, witness, auxiliary),
    vm_native_scripts.verify_native_scripts_effectful_descriptor(
      pre,
      witness,
      auxiliary,
    ),
    !vm_native_scripts.verify_native_scripts(pre, forged_successor, auxiliary),
    !vm_native_scripts.verify_native_scripts_effectful_descriptor(
      pre,
      forged_successor,
      auxiliary,
    ),
    !vm_native_scripts.verify_native_scripts(forged_source, witness, auxiliary),
    !vm_native_scripts.verify_native_scripts_effectful_descriptor(
      forged_source,
      witness,
      auxiliary,
    ),
    !vm_native_scripts.verify_native_scripts_native_descriptor(
      pre,
      witness,
      auxiliary,
    ),
  }
}
