use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use env
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_field_preimage_lengths_v1, encode_native_tx_body_compact,
  native_tx_canonical_size_v1, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxOutput,
  MidgardValue, MidgardVersionedScript, PlutusV3Script, PubKeyCredential,
  native_tx_version_v1,
}
use midgard/ledger_output_commitment_v1
use midgard/rejection_reason_v1
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_28_b, h_a, h_b, h_c, target_snapshot_validation_context_cbor,
  test_l1_slot_config, validation_context_cbor,
  value_output_descriptor_total_length, zero,
}
use midgard/validation_machine_tests/fixture_types.{
  ValueOutputDescriptorFixture,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_native_control_fixture, empty_v1_transaction, exact_rejection_post,
  forced_pending_state, inline_field_door, one_step_evidence, singleton_root,
  submitted_source,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

/// The exact minimum-Ada floor for that output, at the production rate.
/// Derived, never written out: a rate or intercept change moves this fixture
/// with the rule instead of leaving a stale literal behind.
/// `min_ada_wiring_floor_is_the_pinned_absolute_value` pins its absolute value.
pub fn value_output_descriptor_min_ada_floor() -> Int {
  vm_shared.min_ada_lovelace_v1(
    env.coins_per_utxo_byte,
    value_output_descriptor_total_length,
  )
}

/// RE-AUTHORED, NOT SUPPRESSED (#618 ruling 1). This fixture used to fund its
/// output with 3 lovelace, which was fine while the min-Ada floor was unwired
/// arithmetic. Now that the ValueAndMint output-descriptor step convicts an
/// under-funded output with `E_MIN_ADA`, a 3-lovelace output can no longer take
/// the advancing step at all -- so the fixture funds the exact floor, and the
/// under-funded leg became its own reject vector below rather than being
/// deleted.
pub fn value_output_descriptor_fixture_at(
  output_lovelace: Int,
) -> ValueOutputDescriptorFixture {
  let tx = empty_v1_transaction()
  let policy_id = h_28_a
  let asset_name = #"beef"
  let quantity = 7
  let unit = policy_id |> bytearray.concat(asset_name)
  let asset_frontier = validation_merkle_v1.build_frontier(
    [
      ledger_output_commitment_v1.asset_leaf_hash(
        policy_id,
        asset_name,
        quantity,
      ),
    ],
  )
  let empty_summary = cek_data_v1.DataSummaryV1 {
    root: h_a,
    cbor_length: 0,
    memory: 0,
  }
  let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: 0,
    total_length: value_output_descriptor_total_length,
    item_commitment: h_b,
    address: #"60" |> bytearray.concat(h_28_b),
    lovelace: output_lovelace,
    asset_count: 1,
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
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let descriptor_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.output_descriptor_leaf_hash(0, descriptor_cbor)],
  )
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    output_count: 1,
    output_peaks: validation_merkle_v1.build_frontier([h_c]).peaks,
    output_descriptor_peaks: descriptor_frontier.peaks,
  }
  let initial_control = vm_machine_types.ValueAndMintControlV1 {
    native_control,
    stage: 3,
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
  let active_control = vm_machine_types.ValueAndMintControlV1 {
    ..initial_control,
    output_asset_cursor: 1,
    replay_value_hash: blake2b_256(descriptor_cbor),
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      ..initial_control.value_accumulator,
      lovelace_delta: -output_lovelace,
    },
  }
  let final_control = vm_machine_types.ValueAndMintControlV1 {
    ..active_control,
    output_cursor: 1,
    output_asset_cursor: 0,
    replay_value_hash: zero,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: -output_lovelace,
      asset_root: singleton_root(unit, cbor.serialise(-7)),
      seen_asset_count: 1,
      nonzero_asset_count: 1,
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let initial_work = vm_witness.encode_value_and_mint_control_v1(
    initial_control,
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.ValueAndMint,
    46,
    initial_work,
  )
  let active_work = vm_witness.encode_value_and_mint_control_v1(active_control)
  let active_state = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 47,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      47,
      active_work,
    ),
  }
  let final_state = validation_trace_v1.ValidationMachineStateV1 {
    ..active_state,
    program_counter: 48,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      48,
      vm_witness.encode_value_and_mint_control_v1(final_control),
    ),
  }
  ValueOutputDescriptorFixture {
    pre,
    active_state,
    final_state,
    initial_work,
    active_work,
    descriptor_cbor,
    policy_id,
    asset_name,
    quantity,
    asset_peaks: asset_frontier.peaks,
  }
}

pub fn value_output_descriptor_fixture() -> ValueOutputDescriptorFixture {
  value_output_descriptor_fixture_at(value_output_descriptor_min_ada_floor())
}

// ---------------------------------------------------------------------------
// C49 (#540) closure -- the four prescribed value/fee/mint selectors.
//
// The six selectors above authenticate the ValueAndMint scan machinery. These
// four close the row's remaining semantics: the canonical parameterized
// minimum-fee formula and the minimum-Ada floor, both instantiated at the C70
// target snapshot with adjacent accept/reject; the exact signed multi-asset
// plus Ada conservation equation, pinned on the same numeric vector the
// TypeScript twin uses; and mint/burn authorization against the authenticated
// mint frontier.
// ---------------------------------------------------------------------------

// The exact canonical size the min-fee formula is evaluated against, for the
// empty V1 transaction carrying `fee`. Size depends on the fee's own CBOR
// width, so the boundary is a fixed point: the tests below assert that the
// probe fee, the boundary fee and the adjacent below-boundary fee all encode to
// the same width and therefore share one canonical size.
pub fn target_snapshot_canonical_size(fee: Int) -> Int {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, fee },
  }
  let compact = midgard_transaction_to_compact(tx)
  let proof_source = submitted_source(tx)
  native_tx_canonical_size_v1(
    compact,
    decode_native_tx_field_preimage_lengths_v1(
      proof_source.field_preimage_lengths_cbor,
    ),
  )
}

// One StaticLedgerRules step over the empty V1 transaction carrying `fee`,
// evaluated against the target-snapshot validation context. `claims_advance`
// selects which successor the prover claims: the exact InputSets advance, or
// the exact `E_MIN_FEE` terminal. Every other field is identical between the
// two legs, so the only thing that can move the result is the min-fee conjunct
// of `static_rules_rejection`.
pub fn target_snapshot_min_fee_step_is_provable(
  fee: Int,
  claims_advance: Bool,
) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, fee },
  }
  let compact = midgard_transaction_to_compact(tx)
  let proof_source = submitted_source(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = vm_witness.encode_static_rules_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    target_snapshot_validation_context_cbor,
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    target_snapshot_validation_context_cbor,
    validation_trace_v1.StaticLedgerRules,
    2,
    work_witness_cbor,
  )
  let claimed_successor = if claims_advance {
    validation_trace_v1.ValidationMachineStateV1 {
      ..pre,
      phase: validation_trace_v1.InputSets,
      program_counter: 3,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.InputSets,
        3,
        vm_witness.encode_input_sets_scan_witness(
          proof_source.compact_cbor,
          proof_source.witness_set_compact_cbor,
          proof_source.field_preimage_lengths_cbor,
          target_snapshot_validation_context_cbor,
          0,
          0,
          0,
          0,
          #"",
          vm_witness.empty_resolution_schedule_hash(),
        ),
      ),
    }
  } else {
    exact_rejection_post(pre, rejection_reason_v1.FeeBelowMinimum)
  }
  vm_one_step.verify_one_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor,
    },
    inline_field_door(),
    test_l1_slot_config,
  )
}

// ---------------------------------------------------------------------------
// D-S4 (#618) -- the Aiken half of the min-Ada twin cross-check.
//
// `parameterized_min_ada_boundary_matches_target_snapshot` above exercises one
// rate (the C70 snapshot's 4,310) and one output shape, and its slope and
// comparison legs are stated relative to the formula under test. It does catch
// a moved intercept, slope or comparison, but it cannot see the rate being
// ignored altogether (a hard-coded `4_310 *` passes it), and it binds nothing
// to the TypeScript side. The vectors below are the same seven
// the TypeScript twin pins in
// demo/midgard-validation/tests/min-ada-twin-cross-check.test.ts, carried
// here as ABSOLUTE lovelace literals: nothing in this module derives them, so
// they can only agree with the TypeScript side or fail.
//
// Two of the seven are anchored harder still. The 41-byte and 196-byte sizes
// are not written as bare integers but measured from two canonical outputs
// whose `encode_midgard_tx_output` bytes are pinned as absolute CBOR -- the
// same two fixtures, byte for byte, that the TypeScript suite pins against
// `encodeMidgardTxOutput`. A codec change on either side therefore breaks the
// byte pin here before it can silently re-price an output.
// ---------------------------------------------------------------------------

/// One pinned vector: the absolute floor, and the four boundary legs around it.
///
/// `floor - 1` must be refused and `floor` accepted -- that adjacency is the
/// whole content of the `>=` in `output_meets_min_ada_v1`, and it is what a
/// weakening to `>` would break. A zero-funded output is refused at every
/// vector here because no vector's floor is zero.
pub fn min_ada_absolute_vector_is_exact(
  coins_per_utxo_byte: Int,
  serialized_output_bytes: Int,
  floor: Int,
) -> Bool {
  and {
    vm_shared.min_ada_lovelace_v1(coins_per_utxo_byte, serialized_output_bytes) == floor,
    vm_shared.output_meets_min_ada_v1(
      coins_per_utxo_byte,
      serialized_output_bytes,
      floor,
    ),
    !vm_shared.output_meets_min_ada_v1(
      coins_per_utxo_byte,
      serialized_output_bytes,
      floor - 1,
    ),
    vm_shared.output_meets_min_ada_v1(
      coins_per_utxo_byte,
      serialized_output_bytes,
      floor + 1,
    ),
    !vm_shared.output_meets_min_ada_v1(
      coins_per_utxo_byte,
      serialized_output_bytes,
      0,
    ),
  }
}

/// The first shared canonical output: an unprotected network-0 enterprise
/// pub-key address over `0xaa * 28`, holding exactly 2 ADA and no assets.
pub fn min_ada_probe_enterprise_output() -> MidgardTxOutput {
  MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: None,
    },
    value: MidgardValue { lovelace: 2_000_000, assets: [] },
    datum_cbor: None,
    script_ref: None,
  }
}

/// The second shared canonical output: the same payment credential under an
/// `0xbb * 28` stake credential, carrying three assets across two policies, an
/// inline datum and a PlutusV3 reference script.
pub fn min_ada_probe_rich_output() -> MidgardTxOutput {
  MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(h_28_a),
      stake_credential: Some(PubKeyCredential(h_28_b)),
    },
    value: MidgardValue {
      lovelace: 9_999_999,
      assets: [
        Pair(#"11111111111111111111111111111111111111111111111111111111abcd", 7),
        Pair(
          #"11111111111111111111111111111111111111111111111111111111ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
          1,
        ), Pair(#"22222222222222222222222222222222222222222222222222222222", 42),
      ],
    },
    datum_cbor: Some(#"d87980"),
    script_ref: Some(
      MidgardVersionedScript {
        language: PlutusV3Script,
        script_bytes: #"4d01000033222220051200120011",
      },
    ),
  }
}

/// The advancing output-descriptor step for a fixture: fold the descriptor's
/// lovelace and open the asset cursor.
pub fn value_output_descriptor_advance_evidence(
  f: ValueOutputDescriptorFixture,
) -> vm_machine_types.ValidationOneStepEvidenceV1 {
  one_step_evidence(
    f.initial_work,
    f.active_state,
    vm_machine_types.ValueOutputDescriptorWitness {
      output_index: 0,
      descriptor_cbor: f.descriptor_cbor,
      siblings: [],
    },
  )
}

/// The rejecting output-descriptor step for a fixture, carrying `code`. Same
/// pre-state and same authenticated descriptor as the advancing step above --
/// only the claimed successor differs, so these two vectors are a true fork on
/// the min-Ada predicate and nothing else.
pub fn value_output_descriptor_rejection_evidence(
  f: ValueOutputDescriptorFixture,
  reason: rejection_reason_v1.RejectionReasonV1,
) -> vm_machine_types.ValidationOneStepEvidenceV1 {
  one_step_evidence(
    f.initial_work,
    exact_rejection_post(f.pre, reason),
    vm_machine_types.ValueOutputDescriptorWitness {
      output_index: 0,
      descriptor_cbor: f.descriptor_cbor,
      siblings: [],
    },
  )
}

// The conservation vector's transaction. `fee = 1` is the TypeScript twin's
// fee, and stage five reads it straight off the authenticated body.
pub fn conservation_transaction() -> MidgardTransaction {
  let base = empty_v1_transaction()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, fee: 1 },
  }
}

pub fn conservation_unit() -> ByteArray {
  h_28_a |> bytearray.concat(#"beef")
}

// One ValueAndMint stage-four mint step that folds `mint_quantity` into an
// asset delta that already stands at `pre_asset_delta` (inputs minus outputs
// for that unit). This is where the signed multi-asset arithmetic actually
// happens: `apply_value_asset_mutation` updates the delta trie and maintains
// `nonzero_asset_count`.
pub fn conservation_mint_step_is_exact(
  mint_quantity: Int,
  pre_asset_delta: Int,
) -> Bool {
  let asset_name = #"beef"
  let tx = conservation_transaction()
  let unit = conservation_unit()
  let mint_frontier = validation_merkle_v1.build_frontier(
    [vm_shared.mint_asset_leaf_hash(h_28_a, asset_name, mint_quantity)],
  )
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = vm_machine_types.ValueAndMintControlV1 {
    native_control,
    stage: 4,
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
      lovelace_delta: 1,
      asset_root: singleton_root(unit, cbor.serialise(pre_asset_delta)),
      seen_asset_count: 1,
      nonzero_asset_count: 1,
    },
  }
  let next_asset_delta = pre_asset_delta + mint_quantity
  let next_control = vm_machine_types.ValueAndMintControlV1 {
    ..control,
    mint_cursor: 1,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 1,
      asset_root: singleton_root(unit, cbor.serialise(next_asset_delta)),
      seen_asset_count: 1,
      nonzero_asset_count: if next_asset_delta == 0 {
        0
      } else {
        1
      },
    },
  }
  let work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(control)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.ValueAndMint,
    60,
    work_witness_cbor,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 61,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      61,
      vm_witness.encode_value_and_mint_control_v1(next_control),
    ),
  }
  vm_one_step.verify_value_and_mint_one_step_v1(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.ValueMintAssetWitness {
        mint_index: 0,
        policy_id: h_28_a,
        asset_name,
        quantity: mint_quantity,
        siblings: [],
        mutation: vm_machine_types.ValueAssetMutationWitnessV1 {
          delta_was_present: True,
          old_delta: pre_asset_delta,
          delta_proof: [],
        },
      },
    ),
  )
}

// The ValueAndMint stage-five final equation on the settled accumulator.
// `claims_advance` selects the claimed successor: the accepting Terminal, or
// the exact `E_VALUE_NOT_PRESERVED` terminal.
pub fn conservation_final_step_is_provable(
  lovelace_delta: Int,
  asset_delta: Int,
  claims_advance: Bool,
) -> Bool {
  let tx = conservation_transaction()
  let unit = conservation_unit()
  let mint_frontier = validation_merkle_v1.build_frontier(
    [vm_shared.mint_asset_leaf_hash(h_28_a, #"beef", 3)],
  )
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = vm_machine_types.ValueAndMintControlV1 {
    native_control,
    stage: 5,
    replay_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 0,
    mint_cursor: 1,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta,
      asset_root: singleton_root(unit, cbor.serialise(asset_delta)),
      seen_asset_count: 1,
      nonzero_asset_count: if asset_delta == 0 {
        0
      } else {
        1
      },
    },
  }
  let work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(control)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.ValueAndMint,
    62,
    work_witness_cbor,
  )
  let claimed_successor = if claims_advance {
    validation_trace_v1.ValidationMachineStateV1 {
      ..pre,
      phase: validation_trace_v1.Terminal,
      program_counter: 63,
      verdict: validation_trace_v1.Accepted,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.Terminal,
        63,
        vm_shared.terminal_acceptance_witness,
      ),
    }
  } else {
    exact_rejection_post(pre, rejection_reason_v1.ValueNotPreserved)
  }
  vm_one_step.verify_value_and_mint_one_step_v1(
    pre,
    one_step_evidence(
      work_witness_cbor,
      claimed_successor,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  )
}

// One ValueAndMint stage-four step whose mint witness names
// `witnessed_policy_id`, against a mint frontier that authenticates
// `authenticated_policy_id` at the same asset name and quantity. Everything the
// prover claims -- the successor control, the accumulator mutation, the cursor
// -- is derived from the WITNESSED policy, so the two legs differ in exactly
// one field of the auxiliary witness.
pub fn mint_authorization_step_is_provable(
  authenticated_policy_id: ByteArray,
  witnessed_policy_id: ByteArray,
  quantity: Int,
) -> Bool {
  let asset_name = #"beef"
  let tx = empty_v1_transaction()
  let mint_frontier = validation_merkle_v1.build_frontier(
    [
      vm_shared.mint_asset_leaf_hash(
        authenticated_policy_id,
        asset_name,
        quantity,
      ),
    ],
  )
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = vm_machine_types.ValueAndMintControlV1 {
    native_control,
    stage: 4,
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
  let unit = witnessed_policy_id |> bytearray.concat(asset_name)
  let next_control = vm_machine_types.ValueAndMintControlV1 {
    ..control,
    mint_cursor: 1,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: singleton_root(unit, cbor.serialise(quantity)),
      seen_asset_count: 1,
      nonzero_asset_count: 1,
    },
  }
  let work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(control)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.ValueAndMint,
    64,
    work_witness_cbor,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 65,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      65,
      vm_witness.encode_value_and_mint_control_v1(next_control),
    ),
  }
  vm_one_step.verify_value_and_mint_one_step_v1(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.ValueMintAssetWitness {
        mint_index: 0,
        policy_id: witnessed_policy_id,
        asset_name,
        quantity,
        siblings: [],
        mutation: vm_machine_types.ValueAssetMutationWitnessV1 {
          delta_was_present: False,
          old_delta: 0,
          delta_proof: [],
        },
      },
    ),
  )
}

/// `length` zero bytes.
///
/// `bytearray.from_int_big_endian` caps at 8,192 bytes, which is below §8.3's
/// tier-1 planning bound, so the filler is built in 4,096-byte blocks.
pub fn zero_filler_bytes(length: Int) -> ByteArray {
  if length <= 4096 {
    bytearray.from_int_big_endian(0, length)
  } else {
    bytearray.from_int_big_endian(0, 4096)
      |> bytearray.concat(zero_filler_bytes(length - 4096))
  }
}
