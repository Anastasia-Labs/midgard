use aiken/cbor
use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/helpers.{combine}
use aiken/merkle_patricia_forestry/merkling.{
  null_hash, null_hash_2, null_hash_4,
}
use aiken/primitive/bytearray
use env
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_mint_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
  MidgardTxOutput, MidgardValue, PubKeyCredential, native_tx_version_v1,
}
use midgard/mpf_proof_fold_v1
use midgard/native_tx_field_access_v1
use midgard/script_proof_v1
use midgard/validation_machine/ledger_delta as vm_ledger_delta
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_28_b, h_a, h_b, h_c, reject_min_ada_code,
  reject_value_not_preserved_code, target_snapshot_coins_per_utxo_byte,
  target_snapshot_min_fee_a, target_snapshot_min_fee_b, validation_context_cbor,
  value_output_descriptor_total_length, zero,
}
use midgard/validation_machine_tests/fixtures_ledger.{
  compact_ledger_delta_control, compact_ledger_delta_state,
  compact_ledger_delta_successor, compact_ledger_output_descriptor,
  conservation_final_step_is_provable, conservation_mint_step_is_exact,
  ledger_delta_operation_authentication_fixture,
  min_ada_absolute_vector_is_exact, min_ada_probe_enterprise_output,
  min_ada_probe_rich_output, mint_authorization_step_is_provable,
  target_snapshot_canonical_size, target_snapshot_min_fee_step_is_provable,
  value_output_descriptor_advance_evidence, value_output_descriptor_fixture,
  value_output_descriptor_fixture_at, value_output_descriptor_min_ada_floor,
  value_output_descriptor_rejection_evidence,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_mpf_proof_descriptor, empty_native_control_fixture, empty_v1_transaction,
  forced_pending_state, inline_field_door, non_empty_claimed_delta_root,
  one_step_evidence, singleton_root,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

test ledger_delta_spend_step_authenticates_deletion_operation() {
  ledger_delta_operation_authentication_fixture(0)
}

test ledger_delta_output_step_authenticates_insertion_operation() {
  ledger_delta_operation_authentication_fixture(1)
}

test ledger_delta_reference_replay_authenticates_descriptor() {
  let key = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_a, output_index: 0 },
  )
  let value = compact_ledger_output_descriptor(0)
  let next_schedule_hash = vm_witness.empty_resolution_schedule_hash()
  let schedule_hash = vm_witness.resolution_schedule_node_hash(
    1,
    key,
    next_schedule_hash,
  )
  let resolved_accumulator = vm_witness.resolved_input_accumulator_successor(
    vm_witness.initial_resolution_accumulator(),
    1,
    key,
    value,
  )
  let control = vm_machine_types.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(0),
    resolved_input_count: 1,
    resolved_inputs_accumulator: resolved_accumulator,
    replay_schedule_hash: schedule_hash,
    replay_remaining_schedule_hash: schedule_hash,
  }
  let pre = compact_ledger_delta_state(
    control,
    50,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  let post = compact_ledger_delta_successor(
    pre,
    vm_machine_types.LedgerDeltaControlV1 {
      ..control,
      replay_cursor: 1,
      replay_accumulator: resolved_accumulator,
      replay_remaining_schedule_hash: next_schedule_hash,
    },
  )
  vm_ledger_delta.verify_ledger_delta_replay_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: vm_witness.encode_ledger_delta_control_v1(control),
      claimed_successor: post,
    },
    1,
    key,
    next_schedule_hash,
    value,
  )
}

test ledger_delta_spend_replay_initializes_authenticated_fold() {
  let key = encode_midgard_tx_input(
    MidgardTxInput { tx_id: h_a, output_index: 0 },
  )
  let value = compact_ledger_output_descriptor(0)
  let descriptor = empty_mpf_proof_descriptor()
  let next_schedule_hash = vm_witness.empty_resolution_schedule_hash()
  let schedule_hash = vm_witness.resolution_schedule_node_hash(
    0,
    key,
    next_schedule_hash,
  )
  let resolved_accumulator = vm_witness.resolved_input_accumulator_successor(
    vm_witness.initial_resolution_accumulator(),
    0,
    key,
    value,
  )
  let authorized_pending = vm_witness.encode_ledger_delta_pending_mutation_v1(
    vm_machine_types.LedgerDeltaPendingMutationV1 {
      authorization_stage: 0,
      operation_kind: 0,
      key,
      value: #"",
      descriptor,
      fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
        next_frame_index: -1,
        expected_next_cursor: 0,
        including_root: #"",
        excluding_root: #"",
      },
    },
  )
  let control = vm_machine_types.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(0),
    resolved_input_count: 1,
    resolved_inputs_accumulator: resolved_accumulator,
    replay_schedule_hash: schedule_hash,
    replay_remaining_schedule_hash: schedule_hash,
    current_ledger_root: singleton_root(key, value),
    pending_mutation_cbor: authorized_pending,
  }
  expect Some(fold_control) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor,
  )
  let post = compact_ledger_delta_successor(
    compact_ledger_delta_state(
      control,
      51,
      validation_merkle_v1.frontier_commitment(0, []),
    ),
    vm_machine_types.LedgerDeltaControlV1 {
      ..control,
      replay_cursor: 1,
      replay_accumulator: resolved_accumulator,
      replay_remaining_schedule_hash: next_schedule_hash,
      pending_mutation_cbor: vm_witness.encode_ledger_delta_pending_mutation_v1(
        vm_machine_types.LedgerDeltaPendingMutationV1 {
          authorization_stage: 1,
          operation_kind: 0,
          key,
          value,
          descriptor,
          fold_control,
        },
      ),
    },
  )
  let pre = compact_ledger_delta_state(
    control,
    51,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  vm_ledger_delta.verify_ledger_delta_replay_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: vm_witness.encode_ledger_delta_control_v1(control),
      claimed_successor: post,
    },
    0,
    key,
    next_schedule_hash,
    value,
  )
}

test ledger_delta_replay_finish_is_exact() {
  let control = compact_ledger_delta_control(0)
  let pre = compact_ledger_delta_state(
    control,
    52,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  vm_ledger_delta.verify_ledger_delta_replay_finish_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: vm_witness.encode_ledger_delta_control_v1(control),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        vm_machine_types.LedgerDeltaControlV1 { ..control, stage: 1 },
      ),
    },
  )
}

test ledger_delta_output_authenticates_descriptor_and_initializes_fold() {
  let output_index = 0
  let key = encode_midgard_tx_input(MidgardTxInput { tx_id: h_a, output_index })
  let value = compact_ledger_output_descriptor(output_index)
  let descriptor = empty_mpf_proof_descriptor()
  let descriptor_leaf = script_proof_v1.output_descriptor_leaf_hash(
    output_index,
    value,
  )
  let descriptor_frontier = validation_merkle_v1.build_frontier(
    [descriptor_leaf],
  )
  let authorized_pending = vm_witness.encode_ledger_delta_pending_mutation_v1(
    vm_machine_types.LedgerDeltaPendingMutationV1 {
      authorization_stage: 0,
      operation_kind: 1,
      key,
      value,
      descriptor,
      fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
        next_frame_index: -1,
        expected_next_cursor: 0,
        including_root: #"",
        excluding_root: #"",
      },
    },
  )
  let control = vm_machine_types.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(1),
    output_count: 1,
    output_descriptor_peaks: descriptor_frontier.peaks,
    current_ledger_root: null_hash,
    pending_mutation_cbor: authorized_pending,
  }
  expect Some(fold_control) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor,
  )
  let pre = compact_ledger_delta_state(
    control,
    53,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  vm_ledger_delta.verify_ledger_delta_output_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: vm_witness.encode_ledger_delta_control_v1(control),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        vm_machine_types.LedgerDeltaControlV1 {
          ..control,
          output_cursor: 1,
          pending_mutation_cbor: vm_witness.encode_ledger_delta_pending_mutation_v1(
            vm_machine_types.LedgerDeltaPendingMutationV1 {
              authorization_stage: 1,
              operation_kind: 1,
              key,
              value,
              descriptor,
              fold_control,
            },
          ),
        },
      ),
    },
    output_index,
    value,
    [],
  )
}

test ledger_delta_output_finish_is_exact() {
  let control = compact_ledger_delta_control(1)
  let pre = compact_ledger_delta_state(
    control,
    54,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  vm_ledger_delta.verify_ledger_delta_output_finish_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: vm_witness.encode_ledger_delta_control_v1(control),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        vm_machine_types.LedgerDeltaControlV1 { ..control, stage: 2 },
      ),
    },
  )
}

// One terminal Branch frame of a pending mutation of `operation_kind`. The
// claimed successor is the fold without the deletion rule (as for an
// insertion), so the step holds exactly when that rule does not refuse it.
fn ledger_delta_terminal_branch_frame(
  neighbors: ByteArray,
  operation_kind: Int,
  opening: ByteArray,
) -> Bool {
  let key = #"616273656e742d6b6579"
  let value = #"696e7365727465642d76616c7565"
  let frame = mpf_proof_fold_v1.ProofFrameV1 {
    version: 1,
    frame_index: 0,
    cursor: 0,
    next_cursor: 1,
    step: mpf.Branch { skip: 0, neighbors },
  }
  let frame_frontier = validation_merkle_v1.build_frontier(
    [mpf_proof_fold_v1.proof_frame_leaf_hash_v1(frame)],
  )
  let descriptor = mpf_proof_fold_v1.ProofDescriptorV1 {
    version: 1,
    frame_count: 1,
    terminal_cursor: 1,
    peaks: frame_frontier.peaks,
  }
  expect Some(fold_control) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor,
  )
  expect Some(next_fold_control) = mpf_proof_fold_v1.fold_proof_frame_v1(
    key,
    descriptor,
    fold_control,
    frame,
    [],
    None,
  )
  let pending = vm_machine_types.LedgerDeltaPendingMutationV1 {
    authorization_stage: 1,
    operation_kind,
    key,
    value,
    descriptor,
    fold_control,
  }
  let control = vm_machine_types.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(1),
    pending_mutation_cbor: vm_witness.encode_ledger_delta_pending_mutation_v1(
      pending,
    ),
  }
  let pre = compact_ledger_delta_state(
    control,
    55,
    validation_merkle_v1.frontier_commitment(0, []),
  )
  vm_ledger_delta.verify_ledger_delta_proof_frame_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: vm_witness.encode_ledger_delta_control_v1(control),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        vm_machine_types.LedgerDeltaControlV1 {
          ..control,
          pending_mutation_cbor: vm_witness.encode_ledger_delta_pending_mutation_v1(
            vm_machine_types.LedgerDeltaPendingMutationV1 {
              ..pending,
              fold_control: next_fold_control,
            },
          ),
        },
      ),
    },
    frame,
    [],
    opening,
  )
}

test ledger_delta_proof_frame_advances_only_authenticated_frame() {
  ledger_delta_terminal_branch_frame(
    h_a
      |> bytearray.concat(h_b)
      |> bytearray.concat(h_c)
      |> bytearray.concat(zero),
    1,
    #"",
  )
}

// A deletion's terminal Branch frame must keep two other children
// (`mpf_proof_v1.terminal_branch_keeps_two_children`). The lone n8 group below
// holds h_a and h_b in slots 8 and 9 and is opened down to them; the same
// group with one child is refused, whatever is sent. An insertion folds either
// frame without the rule.
test ledger_delta_deletion_frame_keeps_a_terminal_branch_with_two_children() {
  let lone_n8 = fn(pair) {
    combine(combine(pair, null_hash_2), null_hash_4)
      |> bytearray.concat(null_hash_4)
      |> bytearray.concat(null_hash_2)
      |> bytearray.concat(null_hash)
  }
  let two = lone_n8(combine(h_a, h_b))
  let one = lone_n8(combine(h_a, null_hash))
  let opening = bytearray.concat(#"0000", bytearray.concat(h_a, h_b))
  and {
    ledger_delta_terminal_branch_frame(two, 0, opening),
    !ledger_delta_terminal_branch_frame(two, 0, #""),
    !ledger_delta_terminal_branch_frame(one, 0, #""),
    !ledger_delta_terminal_branch_frame(
      one,
      0,
      bytearray.concat(#"0000", bytearray.concat(h_a, null_hash)),
    ),
    ledger_delta_terminal_branch_frame(one, 1, #""),
  }
}

test ledger_delta_finalize_applies_only_authenticated_root_transition() {
  let key = #"696e736572742d6b6579"
  let value = #"696e736572742d76616c7565"
  let descriptor = empty_mpf_proof_descriptor()
  expect Some(fold_control) = mpf_proof_fold_v1.initial_fold_control_v1(
    key,
    value,
    descriptor,
  )
  let pending = vm_machine_types.LedgerDeltaPendingMutationV1 {
    authorization_stage: 1,
    operation_kind: 1,
    key,
    value,
    descriptor,
    fold_control,
  }
  let operation_leaf = vm_shared.ledger_delta_operation_leaf_hash(
    1,
    key,
    value,
    descriptor,
  )
  let operation_frontier = validation_merkle_v1.build_frontier([operation_leaf])
  let control = vm_machine_types.LedgerDeltaControlV1 {
    ..compact_ledger_delta_control(1),
    current_ledger_root: fold_control.excluding_root,
    pending_mutation_cbor: vm_witness.encode_ledger_delta_pending_mutation_v1(
      pending,
    ),
  }
  let pre = compact_ledger_delta_state(
    control,
    56,
    validation_merkle_v1.frontier_commitment(
      operation_frontier.count,
      operation_frontier.peaks,
    ),
  )
  vm_ledger_delta.verify_ledger_delta_finalize_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: vm_witness.encode_ledger_delta_control_v1(control),
      claimed_successor: compact_ledger_delta_successor(
        pre,
        vm_machine_types.LedgerDeltaControlV1 {
          ..control,
          current_ledger_root: fold_control.including_root,
          operation_count: 1,
          operation_peaks: operation_frontier.peaks,
          pending_mutation_cbor: #"",
        },
      ),
    },
  )
}

test value_and_mint_final_equation_is_enforced_on_l1() {
  let tx = empty_v1_transaction()
  let native_control = empty_native_control_fixture(tx)
  let valid_control = vm_machine_types.ValueAndMintControlV1 {
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
    mint_cursor: 0,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
  let valid_work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(
    valid_control,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let valid_pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.ValueAndMint,
    40,
    valid_work_witness_cbor,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let valid_post = validation_trace_v1.ValidationMachineStateV1 {
    ..valid_pre,
    phase: validation_trace_v1.LedgerDelta,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      41,
      vm_witness.encode_ledger_delta_witness_v1(
        native_control,
        valid_pre.prior_ledger_root,
        valid_control.replay_schedule_hash,
      ),
    ),
  }
  let invalid_control = vm_machine_types.ValueAndMintControlV1 {
    ..valid_control,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 1,
      asset_root: null_hash,
      seen_asset_count: 0,
      nonzero_asset_count: 0,
    },
  }
  let invalid_work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(
    invalid_control,
  )
  let invalid_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..valid_pre,
    program_counter: 42,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      42,
      invalid_work_witness_cbor,
    ),
    // Re-based off `frontier_commitment(0, [])`: this pre-state backs a
    // rejection proof, so it must carry the realistic non-empty claimed delta.
    ledger_delta_root: non_empty_claimed_delta_root(),
  }
  let rejection_code = #"455f56414c55455f4e4f545f505245534552564544"
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..invalid_pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 43,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      43,
      vm_shared.encode_terminal_rejection_witness(
        rejection_code,
        invalid_pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  and {
    vm_one_step.verify_value_and_mint_one_step_v1(
      valid_pre,
      one_step_evidence(
        valid_work_witness_cbor,
        valid_post,
        vm_machine_types.NoAuxiliaryWitness,
      ),
    ),
    vm_one_step.verify_value_and_mint_one_step_v1(
      invalid_pre,
      one_step_evidence(
        invalid_work_witness_cbor,
        terminal,
        vm_machine_types.NoAuxiliaryWitness,
      ),
    ),
  }
}

test value_and_mint_uses_authenticated_native_mint_frontier() {
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
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    mint_count: mint_frontier.count,
    mint_peaks: mint_frontier.peaks,
  }
  let control = vm_machine_types.ValueAndMintControlV1 {
    native_control,
    stage: 1,
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
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(control)
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.ValueAndMint,
    40,
    work_witness_cbor,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      41,
      vm_witness.encode_value_and_mint_control_v1(
        vm_machine_types.ValueAndMintControlV1 { ..control, stage: 2 },
      ),
    ),
  }
  let valid_step = vm_one_step.verify_value_and_mint_one_step_v1(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  )
  let malformed_native_control = vm_machine_types.NativeScriptsControlV1 {
    ..native_control,
    mint_count: 0,
  }
  let malformed_control = vm_machine_types.ValueAndMintControlV1 {
    ..control,
    native_control: malformed_native_control,
  }
  let malformed_work_witness_cbor = vm_witness.encode_value_and_mint_control_v1(
    malformed_control,
  )
  let malformed_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      pre.program_counter,
      malformed_work_witness_cbor,
    ),
  }
  let malformed_post = validation_trace_v1.ValidationMachineStateV1 {
    ..post,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      post.program_counter,
      vm_witness.encode_value_and_mint_control_v1(
        vm_machine_types.ValueAndMintControlV1 { ..malformed_control, stage: 2 },
      ),
    ),
  }
  and {
    valid_step,
    !vm_one_step.verify_value_and_mint_one_step_v1(
      malformed_pre,
      one_step_evidence(
        malformed_work_witness_cbor,
        malformed_post,
        vm_machine_types.NoAuxiliaryWitness,
      ),
    ),
  }
}

test value_and_mint_replays_signed_burn_leaf_on_l1() {
  let tx = empty_v1_transaction()
  let policy_id = #"11111111111111111111111111111111111111111111111111111111"
  let asset_name = #"beef"
  let unit = policy_id |> bytearray.concat(asset_name)
  let leaf = vm_shared.mint_asset_leaf_hash(policy_id, asset_name, -5)
  let mint_frontier = validation_merkle_v1.build_frontier([leaf])
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
      asset_root: singleton_root(unit, cbor.serialise(5)),
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
    44,
    work_witness_cbor,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let next_control = vm_machine_types.ValueAndMintControlV1 {
    ..control,
    mint_cursor: 1,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: singleton_root(unit, cbor.serialise(0)),
      seen_asset_count: 1,
      nonzero_asset_count: 0,
    },
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 45,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      45,
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
    ),
  )
}

test value_and_mint_authenticates_output_descriptor_on_l1() {
  let fixture = value_output_descriptor_fixture()
  vm_one_step.verify_one_step_evidence(
    fixture.pre,
    one_step_evidence(
      fixture.initial_work,
      fixture.active_state,
      vm_machine_types.ValueOutputDescriptorWitness {
        output_index: 0,
        descriptor_cbor: fixture.descriptor_cbor,
        siblings: [],
      },
    ),
    inline_field_door(),
  )
}

test value_and_mint_replays_output_descriptor_asset_on_l1() {
  let fixture = value_output_descriptor_fixture()
  vm_one_step.verify_one_step_evidence(
    fixture.active_state,
    one_step_evidence(
      fixture.active_work,
      fixture.final_state,
      vm_machine_types.ValueOutputAssetWitness {
        output_index: 0,
        descriptor_cbor: fixture.descriptor_cbor,
        asset_index: 0,
        policy_id: fixture.policy_id,
        asset_name: fixture.asset_name,
        quantity: fixture.quantity,
        asset_peaks: fixture.asset_peaks,
        asset_siblings: [],
        mutation: vm_machine_types.ValueAssetMutationWitnessV1 {
          delta_was_present: False,
          old_delta: 0,
          delta_proof: [],
        },
      },
    ),
    inline_field_door(),
  )
}

// C49 (#540). The canonical parameterized minimum-fee rule is
// `body.fee >= min_fee_a * canonical_tx_size + min_fee_b`, evaluated by
// `static_rules_rejection` (validation-machine-v1.ak) over
// `native_tx_canonical_size_v1`. This selector instantiates it at the C70
// target snapshot (`minFeeA = 44`, `minFeeB = 155_381`) and pins the two
// ADJACENT transactions at the boundary: a fee exactly at the floor advances to
// InputSets, a fee one lovelace below it is an exact `E_MIN_FEE` rejection.
//
// The two negative legs are the attribution: neither disposition is provable
// against the other's transaction, so the min-fee comparison -- not some other
// conjunct of the StaticLedgerRules step -- is what separates them. The two
// transactions differ in exactly one lovelace of `body.fee`.
//
// NO ARBITRARY SUB-CAP: the accepted leg pays exactly `44 * size + 155_381`,
// not one lovelace more, so no Midgard-specific margin is stacked on top of the
// target-network floor.
test parameterized_min_fee_boundary_matches_target_snapshot() {
  let probe_size = target_snapshot_canonical_size(target_snapshot_min_fee_b)
  let boundary_fee = target_snapshot_min_fee_a * probe_size + target_snapshot_min_fee_b
  and {
    target_snapshot_min_fee_a == 44,
    target_snapshot_min_fee_b == 155_381,
    // The fixed point: the fee's CBOR width, and therefore the canonical size,
    // is the same for the probe, the boundary and the adjacent transaction.
    target_snapshot_canonical_size(boundary_fee) == probe_size,
    target_snapshot_canonical_size(boundary_fee - 1) == probe_size,
    target_snapshot_min_fee_step_is_provable(boundary_fee, True),
    target_snapshot_min_fee_step_is_provable(boundary_fee - 1, False),
    !target_snapshot_min_fee_step_is_provable(boundary_fee, False),
    !target_snapshot_min_fee_step_is_provable(boundary_fee - 1, True),
  }
}

// C49 (#540). The canonical parameterized minimum-Ada floor,
// `min_ada_lovelace_v1` (validation-machine-v1.ak), instantiated at the C70
// target snapshot `coinsPerUtxoByte = 4_310` over a concrete serialized ledger
// output. The boundary is pinned adjacently: an output funded at exactly the
// floor satisfies it, one lovelace below does not.
//
// NO ARBITRARY SUB-CAP: the last two conjuncts pin that the floor is exactly
// linear in the serialized size with slope `coins_per_utxo_byte` and intercept
// `coins_per_utxo_byte * 160` -- the target-network entry overhead -- with no
// additional Midgard margin at any size.
test parameterized_min_ada_boundary_matches_target_snapshot() {
  let output = MidgardTxOutput {
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
  let output_bytes = bytearray.length(encode_midgard_tx_output(output))
  let floor = vm_shared.min_ada_lovelace_v1(
    target_snapshot_coins_per_utxo_byte,
    output_bytes,
  )
  and {
    target_snapshot_coins_per_utxo_byte == 4_310,
    vm_shared.min_ada_output_overhead_bytes == 160,
    floor == target_snapshot_coins_per_utxo_byte * ( 160 + output_bytes ),
    vm_shared.output_meets_min_ada_v1(
      target_snapshot_coins_per_utxo_byte,
      output_bytes,
      floor,
    ),
    !vm_shared.output_meets_min_ada_v1(
      target_snapshot_coins_per_utxo_byte,
      output_bytes,
      floor - 1,
    ),
    vm_shared.output_meets_min_ada_v1(
      target_snapshot_coins_per_utxo_byte,
      output_bytes,
      floor + 1,
    ),
    vm_shared.min_ada_lovelace_v1(
      target_snapshot_coins_per_utxo_byte,
      output_bytes + 1,
    ) - floor == target_snapshot_coins_per_utxo_byte,
    vm_shared.min_ada_lovelace_v1(target_snapshot_coins_per_utxo_byte, 0) == 689_600,
  }
}

test min_ada_shared_canonical_outputs_serialize_to_their_pinned_bytes() {
  let enterprise = encode_midgard_tx_output(min_ada_probe_enterprise_output())
  let rich = encode_midgard_tx_output(min_ada_probe_rich_output())
  and {
    enterprise == #"a200581d60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa01821a001e8480a0",
    bytearray.length(enterprise) == 41,
    rich == #"a400583900aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb01821a0098967fa2581c11111111111111111111111111111111111111111111111111111111a242abcd075820ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff01581c22222222222222222222222222222222222222222222222222222222a140182a0243d879800382034e4d01000033222220051200120011",
    bytearray.length(rich) == 196,
    // The two floors those exact fixtures fund, as absolute lovelace.
    vm_shared.min_ada_lovelace_v1(
      target_snapshot_coins_per_utxo_byte,
      bytearray.length(enterprise),
    ) == 866_310,
    vm_shared.min_ada_lovelace_v1(
      target_snapshot_coins_per_utxo_byte,
      bytearray.length(rich),
    ) == 1_534_360,
  }
}

test min_ada_absolute_vectors_match_the_typescript_twin() {
  let enterprise_bytes = bytearray.length(
    encode_midgard_tx_output(min_ada_probe_enterprise_output()),
  )
  let rich_bytes = bytearray.length(
    encode_midgard_tx_output(min_ada_probe_rich_output()),
  )
  and {
    target_snapshot_coins_per_utxo_byte == 4_310,
    vm_shared.min_ada_output_overhead_bytes == 160,
    // 1. C70 snapshot rate, zero-byte intercept.
    min_ada_absolute_vector_is_exact(4_310, 0, 689_600),
    // 2. C70 snapshot rate, the 41-byte enterprise output holding 2 ADA.
    min_ada_absolute_vector_is_exact(4_310, enterprise_bytes, 866_310),
    min_ada_absolute_vector_is_exact(4_310, 41, 866_310),
    // 3. C70 snapshot rate, the 196-byte multi-asset/datum/script_ref output.
    min_ada_absolute_vector_is_exact(4_310, rich_bytes, 1_534_360),
    min_ada_absolute_vector_is_exact(4_310, 196, 1_534_360),
    // 4. Unit rate, zero-byte intercept -- the overhead standing alone.
    min_ada_absolute_vector_is_exact(1, 0, 160),
    // 5. Unit rate at the 41-byte canonical output.
    min_ada_absolute_vector_is_exact(1, 41, 201),
    // 6. A synthetic 5_000 rate at the 196-byte canonical output: the rate is a
    //    free parameter, so moving it moves the floor.
    min_ada_absolute_vector_is_exact(5_000, 196, 1_780_000),
    // 7. C70 snapshot rate at a 5_000-byte output -- no sub-cap at size.
    min_ada_absolute_vector_is_exact(4_310, 5_000, 22_239_600),
  }
}

// ---------------------------------------------------------------------------
// E_MIN_ADA wiring (#618 ruling 1; R8 of decision 0005; #627 owner ruling,
// option B). Everything above this line exercises the floor as ARITHMETIC. The
// vectors below exercise it as a CONVICTION: the ValueAndMint output-descriptor
// step, at `output_asset_cursor == 0`, now rejects an under-funded output with
// `E_MIN_ADA` instead of folding its lovelace into the accumulator.
// ---------------------------------------------------------------------------

/// The compiled deployment pin itself. The rate the output-descriptor step
/// convicts with is `env.coins_per_utxo_byte`, held identical in
/// `env/default.ak` and `env/testnet.ak` and mirrored by
/// `MIDGARD_CONSENSUS_LIMITS_V1.coinsPerUtxoByte`; no vector above pins its
/// absolute value now that the local test constant reads it.
///
/// ZERO-RATE: no on-chain guard rejects a zero rate, and one must not be added
/// -- at a compiled constant the rate cannot be zero at runtime, so the guard
/// would be unreachable code in the hottest loop of the machine (C74). The
/// hazard is real and is discharged here instead: the last two conjuncts show
/// that a zero rate silently accepts a zero-lovelace 16 KiB output while the
/// production rate rejects it, which is exactly what the absolute pin in the
/// first conjunct prevents from ever being deployed.
test min_ada_target_snapshot_rate_is_the_production_pin() {
  and {
    env.coins_per_utxo_byte == 4_310,
    target_snapshot_coins_per_utxo_byte == env.coins_per_utxo_byte,
    env.coins_per_utxo_byte > 0,
    vm_shared.output_meets_min_ada_v1(0, 16384, 0),
    !vm_shared.output_meets_min_ada_v1(env.coins_per_utxo_byte, 16384, 0),
  }
}

/// The absolute floor the wiring fixture below is funded at:
/// `4_310 * (160 + 16_384)`. Derived in the fixture, pinned here, so a moved
/// rate or intercept fails this vector rather than silently re-basing every
/// conviction vector with it.
test min_ada_wiring_floor_is_the_pinned_absolute_value() {
  and {
    value_output_descriptor_total_length == 16384,
    value_output_descriptor_min_ada_floor() == 71_304_640,
  }
}

/// FLOOR MET. An output funded at exactly the floor takes the advancing step,
/// and CANNOT be convicted with `E_MIN_ADA` -- the second conjunct is the
/// no-false-conviction control, without which the wiring could reject every
/// output and still pass the reject vectors below.
test value_and_mint_output_descriptor_admits_exact_min_ada_floor() {
  let f = value_output_descriptor_fixture_at(
    value_output_descriptor_min_ada_floor(),
  )
  and {
    vm_one_step.verify_one_step_evidence(
      f.pre,
      value_output_descriptor_advance_evidence(f),
      inline_field_door(),
    ),
    !vm_one_step.verify_one_step_evidence(
      f.pre,
      value_output_descriptor_rejection_evidence(f, reject_min_ada_code),
      inline_field_door(),
    ),
  }
}

/// FLOOR MISSED BY ONE LOVELACE. The adjacent boundary: the same descriptor,
/// one lovelace short, is convictable with `E_MIN_ADA` and -- second conjunct
/// -- is no longer provable as an advancing step, which is the clause that
/// makes the rejection mandatory rather than merely available to a challenger.
test value_and_mint_output_descriptor_rejects_min_ada_short_by_one_lovelace() {
  let f = value_output_descriptor_fixture_at(
    value_output_descriptor_min_ada_floor() - 1,
  )
  and {
    vm_one_step.verify_one_step_evidence(
      f.pre,
      value_output_descriptor_rejection_evidence(f, reject_min_ada_code),
      inline_field_door(),
    ),
    !vm_one_step.verify_one_step_evidence(
      f.pre,
      value_output_descriptor_advance_evidence(f),
      inline_field_door(),
    ),
  }
}

/// ZERO LOVELACE, RE-AUTHORED NOT SUPPRESSED (#618 ruling 1). This is the shape
/// the output-descriptor fixture used to carry (3 lovelace over a 16 KiB
/// output) taken to its limit. Before the wiring it advanced; now it convicts.
test value_and_mint_output_descriptor_rejects_zero_lovelace_output() {
  let f = value_output_descriptor_fixture_at(0)
  and {
    vm_one_step.verify_one_step_evidence(
      f.pre,
      value_output_descriptor_rejection_evidence(f, reject_min_ada_code),
      inline_field_door(),
    ),
    !vm_one_step.verify_one_step_evidence(
      f.pre,
      value_output_descriptor_advance_evidence(f),
      inline_field_door(),
    ),
  }
}

/// THE CODE IS PINNED. An under-funded output is convictable with `E_MIN_ADA`
/// and with nothing else: a challenger cannot launder a min-Ada violation into
/// the adjacent `E_VALUE_NOT_PRESERVED` conviction, which would settle the
/// dispute under the wrong rule and leave the manifest's rejection-code
/// evidence describing a code the machine never emits.
test value_and_mint_output_descriptor_min_ada_code_is_not_interchangeable() {
  let f = value_output_descriptor_fixture_at(
    value_output_descriptor_min_ada_floor() - 1,
  )
  !vm_one_step.verify_one_step_evidence(
    f.pre,
    value_output_descriptor_rejection_evidence(
      f,
      reject_value_not_preserved_code,
    ),
    inline_field_door(),
  )
}

// C49 (#540). Exact signed multi-asset conservation, including the Ada-specific
// rule, on the SAME numeric vector the TypeScript twin pins in
// demo/midgard-validation/tests/value-accounting.test.ts (`mint-aware value
// accounting`, over `valuePreservationDelta`):
//
//   mint vector  inputs {lovelace 5, asset 2}, fee 1, mint +3,
//                outputs {lovelace 4, asset 5}  ->  delta is zero
//   burn vector  inputs {lovelace 5, asset 5}, fee 1, mint -2,
//                outputs {lovelace 4, asset 3}  ->  delta is zero
//
// Aiken accumulates the same equation in two halves. The unit's delta reaches
// the mint step already carrying `inputs - outputs` (-3 for the mint vector,
// +2 for the burn vector), and the mint step folds the signed mint quantity in,
// which must land it on zero and drop `nonzero_asset_count` to zero. Ada is
// carried separately as `lovelace_delta = 5 - 4 = 1`, and stage five closes it
// against the authenticated `body.fee = 1`. Both languages therefore assert
// `inputs + mint - outputs - fee == 0` over the identical numbers; a change to
// either side's arithmetic breaks this selector.
//
// The negative legs are single-number perturbations of the same vector, each
// falsifying exactly one half of the equation: outputs of 5 lovelace instead of
// 4 leaves the Ada rule one lovelace short, and a +2 mint instead of +3 leaves
// the asset delta at -1. Each is an exact `E_VALUE_NOT_PRESERVED` rejection and
// is NOT provable as an advance, and the conserved vector is NOT provable as a
// rejection.
test parameterized_ada_and_multi_asset_conservation_matches_typescript() {
  and {
    // Mint vector: asset delta -3 (2 in, 5 out) plus a +3 mint settles to zero.
    conservation_mint_step_is_exact(3, -3),
    // Burn vector: asset delta +2 (5 in, 3 out) plus a -2 burn settles to zero.
    conservation_mint_step_is_exact(-2, 2),
    // Ada and every asset conserved: the step advances to LedgerDelta.
    conservation_final_step_is_provable(1, 0, True),
    !conservation_final_step_is_provable(1, 0, False),
    // Ada short by one lovelace (outputs 5 instead of 4).
    conservation_final_step_is_provable(0, 0, False),
    !conservation_final_step_is_provable(0, 0, True),
    // One asset unbalanced (a +2 mint instead of +3).
    conservation_final_step_is_provable(1, -1, False),
    !conservation_final_step_is_provable(1, -1, True),
  }
}

// C49 (#540). Mint and burn authorization. A ValueAndMint mint step may only
// move an asset delta for a policy the AUTHENTICATED native mint frontier
// commits to: `value_and_mint_stage_four` requires membership of
// `mint_asset_leaf_hash(policy_id, asset_name, quantity)` in
// `native_control.mint_peaks`. An unbacked policy -- one no native script
// authorized, so no leaf exists for it -- can therefore never move value, in
// either direction.
//
// ATTRIBUTION (post-#532). This is not a bare `test ... fail`: each leg is a
// full one-step evaluation and the fixture differs between the passing and
// failing legs in exactly one field, `policy_id` in the auxiliary witness.
// Every other conjunct of the stage-four arm survives that substitution --
// `mint_index == mint_cursor` (0 == 0), `length(policy_id) == 28` (h_28_b is
// also 28 bytes), `length(asset_name) <= 32`, `quantity != 0`, and the
// accumulator mutation, whose claimed successor is derived from the witnessed
// policy so it stays internally exact. The last two conjuncts below pin that
// directly: the authenticated leaf IS a member of the frontier and the unbacked
// one is NOT, which makes the membership check the sole falsifier.
test mint_and_burn_authorization_rejects_unbacked_policy() {
  let backed_mint_frontier = validation_merkle_v1.build_frontier(
    [vm_shared.mint_asset_leaf_hash(h_28_a, #"beef", 3)],
  )
  and {
    // Positive controls: the authorized policy mints and burns.
    mint_authorization_step_is_provable(h_28_a, h_28_a, 3),
    mint_authorization_step_is_provable(h_28_a, h_28_a, -3),
    // The same steps under an unbacked policy are unprovable, mint and burn.
    !mint_authorization_step_is_provable(h_28_a, h_28_b, 3),
    !mint_authorization_step_is_provable(h_28_a, h_28_b, -3),
    // Attribution: the frontier membership conjunct is what moved.
    validation_merkle_v1.verify_membership(
      backed_mint_frontier.count,
      backed_mint_frontier.peaks,
      0,
      vm_shared.mint_asset_leaf_hash(h_28_a, #"beef", 3),
      [],
    ),
    !validation_merkle_v1.verify_membership(
      backed_mint_frontier.count,
      backed_mint_frontier.peaks,
      0,
      vm_shared.mint_asset_leaf_hash(h_28_b, #"beef", 3),
      [],
    ),
  }
}

test value_asset_output_instruction_evidence_is_bounded() {
  let tx = empty_v1_transaction()
  let dense_frontier_four = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: zero },
    validation_merkle_v1.FrontierPeak { height: 1, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 2, hash: h_b },
    validation_merkle_v1.FrontierPeak { height: 3, hash: h_c },
  ]
  let dense_frontier_six = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: zero },
    validation_merkle_v1.FrontierPeak { height: 1, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 2, hash: h_b },
    validation_merkle_v1.FrontierPeak { height: 3, hash: h_c },
    validation_merkle_v1.FrontierPeak { height: 4, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 5, hash: h_b },
  ]
  let dense_frontier_seven = [
    validation_merkle_v1.FrontierPeak { height: 0, hash: zero },
    validation_merkle_v1.FrontierPeak { height: 1, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 2, hash: h_b },
    validation_merkle_v1.FrontierPeak { height: 3, hash: h_c },
    validation_merkle_v1.FrontierPeak { height: 4, hash: h_a },
    validation_merkle_v1.FrontierPeak { height: 5, hash: h_b },
    validation_merkle_v1.FrontierPeak { height: 6, hash: h_c },
  ]
  let native_control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    resolved_input_count: 127,
    spend_input_count: 63,
    resolved_item_peaks: dense_frontier_seven,
    signer_count: 63,
    source_count: 15,
    source_peaks: dense_frontier_four,
    redeemer_count: 15,
    redeemer_peaks: dense_frontier_four,
    purpose_count: 15,
    purpose_peaks: dense_frontier_four,
    output_count: 63,
    output_peaks: dense_frontier_six,
    output_descriptor_peaks: dense_frontier_six,
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: 15,
    execution_peaks: dense_frontier_four,
    execution_cursor: 15,
    language_bitmap: 3,
  }
  let control = vm_machine_types.ValueAndMintControlV1 {
    native_control,
    stage: 3,
    replay_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    replay_cursor: 0,
    replay_asset_cursor: 0,
    replay_value_hash: zero,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    output_cursor: 0,
    output_asset_cursor: 1,
    mint_cursor: 0,
    value_accumulator: vm_machine_types.ValueAccumulatorV1 {
      lovelace_delta: 0,
      asset_root: h_a,
      seen_asset_count: 127,
      nonzero_asset_count: 127,
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
    50,
    work_witness_cbor,
    h_b,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 51,
    work_root: h_c,
  }
  let maximum_branch = mpf.Branch {
    skip: 0,
    neighbors: bytearray.from_int_big_endian(0, 128),
  }
  let maximum_map_proof = list.repeat(maximum_branch, 16)
  let evidence = one_step_evidence(
    work_witness_cbor,
    post,
    vm_machine_types.ValueOutputAssetWitness {
      output_index: 63,
      descriptor_cbor: bytearray.from_int_big_endian(0, 512),
      asset_index: 127,
      policy_id: h_28_a,
      asset_name: bytearray.from_int_big_endian(0, 32),
      quantity: 9_223_372_036_854_775_807,
      asset_peaks: dense_frontier_seven,
      asset_siblings: list.repeat(zero, 7),
      mutation: vm_machine_types.ValueAssetMutationWitnessV1 {
        delta_was_present: True,
        old_delta: -9_223_372_036_854_775_808,
        delta_proof: maximum_map_proof,
      },
    },
  )
  let evidence_size = bytearray.length(cbor.serialise(evidence))
  evidence_size < 12_288
}
