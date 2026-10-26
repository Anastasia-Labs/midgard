use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_address_witness, encode_midgard_tx_input,
  encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput, MidgardVersionedScript,
  NativeCardanoScript, PlutusV3Script,
}
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1
use midgard/script_proof_v1
use midgard/validation_machine/input_sets as vm_input_sets
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/phase_a_native_scripts as vm_phase_a_native_scripts
use midgard/validation_machine/phase_a_script_preconditions as vm_preconditions
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/signatures as vm_signatures
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_a, h_b, h_c, test_l1_slot_config, validation_context_cbor, zero,
}
use midgard/validation_machine_tests/fixtures_input_resolution.{
  resolve_inputs_authenticated_membership_step,
  signatures_handoff_with_claimed_result,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  contains_non_native_script, duplicate_observer_field_carriage,
  empty_v1_transaction, exact_rejection_post, initial_signatures_witness,
  inline_field_carriage, inline_field_door, input_sets_pre,
  input_sets_scan_witness, next_phase_a_native_state, one_step_evidence,
  phase_a_native_scan_witness, phase_a_native_step_fixture,
  phase_a_script_preconditions_fixture, resolve_inputs_step_fixture,
  signatures_scan_witness, signatures_step_fixture, signer_frontier_for_tx,
  singleton_field_item_chunk_proof, static_rules_step_fixture, submitted_source,
  v1_plutus_script,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// POSITIVE CONTROL: a static-rules rejection is provable with the exact
// rejecting terminal as the successor.
test a_static_rules_rejection_is_provable() {
  let Pair(pre, work_witness_cbor) = static_rules_step_fixture(False)
  vm_one_step.verify_one_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(
        pre,
        rejection_reason_v1.NetworkIdMismatch,
      ),
    },
    inline_field_door(),
    test_l1_slot_config,
  )
}

// NEGATIVE CONTROL -- mutation. The rejecting terminal may not rewrite the
// prior ledger root, which is immutable context.
test rejection_successor_that_rewrites_the_prior_ledger_root_is_rejected() {
  let Pair(pre, work_witness_cbor) = static_rules_step_fixture(False)
  let rewritten_terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..exact_rejection_post(pre, rejection_reason_v1.NetworkIdMismatch),
    prior_ledger_root: validation_merkle_v1.frontier_commitment(0, []),
  }
  and {
    pre.prior_ledger_root != validation_merkle_v1.frontier_commitment(0, []),
    !vm_one_step.verify_one_step(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: rewritten_terminal,
      },
      inline_field_door(),
      test_l1_slot_config,
    ),
  }
}

// NEGATIVE CONTROL -- valid-block rejection. Here the transaction satisfies
// the static ledger rules, so its true one-step is an advance to `InputSets`;
// an otherwise well-formed rejecting terminal must be unprovable.
test a_valid_static_rules_transition_cannot_be_proven_rejected() {
  let Pair(pre, work_witness_cbor) = static_rules_step_fixture(True)
  !vm_one_step.verify_one_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(
        pre,
        rejection_reason_v1.NetworkIdMismatch,
      ),
    },
    inline_field_door(),
    test_l1_slot_config,
  )
}

test input_sets_rejects_a_successor_in_the_wrong_phase() {
  let tx = empty_v1_transaction()
  let work_witness_cbor = input_sets_scan_witness(
    tx,
    0,
    0,
    0,
    0,
    #"",
    -1,
    -1,
    #"",
    vm_witness.empty_resolution_schedule_hash(),
  )
  let input_pre = input_sets_pre(tx, 2, work_witness_cbor)
  let static_post = validation_trace_v1.ValidationMachineStateV1 {
    ..input_pre,
    phase: validation_trace_v1.StaticLedgerRules,
    program_counter: 3,
    work_root: h_c,
  }
  !vm_input_sets.verify_input_sets_empty_semantics_v1(
    input_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: static_post,
    },
  )
}

test input_sets_proves_an_empty_input_forced_transaction_is_an_exact_no_op() {
  let tx = empty_v1_transaction()
  let work_witness_cbor = input_sets_scan_witness(
    tx,
    0,
    0,
    0,
    0,
    #"",
    -1,
    -1,
    #"",
    vm_witness.empty_resolution_schedule_hash(),
  )
  let input_pre = input_sets_pre(tx, 2, work_witness_cbor)
  let rejection_code = rejection_reason_v1.EmptyInputs
  vm_input_sets.verify_input_sets_empty_semantics_v1(
    input_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(input_pre, rejection_code),
    },
  )
}

test input_sets_proves_a_malformed_validity_interval_after_the_final_item() {
  input_sets_proves_a_malformed_validity_interval(10, 9)
}

// `invalid_hereafter` is exclusive, so `[10, 10)` contains no slot and is
// malformed exactly like a reversed interval.
test input_sets_proves_an_empty_validity_interval_malformed() {
  input_sets_proves_a_malformed_validity_interval(10, 10)
}

fn input_sets_proves_a_malformed_validity_interval(
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> Bool {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [input],
      validity_interval_start,
      validity_interval_end,
    },
  }
  let work_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    0,
    0,
    0,
    #"",
    -1,
    -1,
    #"",
    vm_witness.empty_resolution_schedule_hash(),
  )
  let pre = input_sets_pre(tx, 2, work_witness_cbor)
  let carriage = inline_field_carriage([key])
  let rejection_code = rejection_reason_v1.ValidityIntervalMalformed
  vm_input_sets.verify_input_sets_item_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(pre, rejection_code),
    },
    inline_field_door(),
    carriage,
  )
}

test signatures_accepts_an_empty_required_signer_and_witness_set() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
  }
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(tx)
  let proof_source = submitted_source(tx)
  let signer_frontier = validation_merkle_v1.build_frontier([])
  let required_witness_cbor = signatures_scan_witness(
    tx,
    1,
    0,
    0,
    0,
    0,
    #"",
    signer_frontier,
    -1,
  )
  let required_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 5,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      5,
      required_witness_cbor,
    ),
  }
  let handoff_witness_cbor = signatures_scan_witness(
    tx,
    2,
    0,
    0,
    0,
    0,
    #"",
    signer_frontier,
    -1,
  )
  let handoff_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..required_pre,
    program_counter: 6,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      6,
      handoff_witness_cbor,
    ),
  }
  let next_work_witness_cbor = vm_witness.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    0,
    0,
    0,
    0,
    0,
    #"",
    0,
    #"",
    0,
    0,
    // A stage-0 phase-A native scan control MUST carry `result == -1`:
    // `phase_a_native_control_is_bound` rejects any other value at stage 0, and
    // `reset_phase_a_native_control` produces -1. Production
    // `verify_signatures_handoff` used to emit 0 here, which was a defect --
    // `result == 0` at a completed stage MEANS "native script evaluated false
    // -> reject", so the handoff asserted a verdict the machine had not
    // computed and produced an unusable successor. The emitter has been fixed;
    // do NOT "fix" this expectation to 0. See
    // `signatures_handoff_rejects_a_stage_zero_control_claiming_a_verdict`.
    -1,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
    -1,
    -1,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..handoff_pre,
    phase: validation_trace_v1.PhaseANativeScripts,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      7,
      next_work_witness_cbor,
    ),
  }
  and {
    vm_signatures.verify_signatures_advance_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: required_pre,
      },
    ),
    vm_signatures.verify_signatures_advance_semantics_v1(
      required_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: required_witness_cbor,
        claimed_successor: handoff_pre,
      },
    ),
    vm_signatures.verify_signatures_handoff_semantics_v1(
      handoff_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: handoff_witness_cbor,
        claimed_successor: post,
      },
    ),
  }
}

// NEGATIVE CONTROL -- mutation (GOAL_SPEC §3 invariant 9) for the sentinel fix.
// `result == 0` at a completed stage MEANS "this native script evaluated false
// -> reject" (`rejected_successor_is_exact` is invoked on it), and `result == 1`
// means "evaluated true". Neither is a legal stage-0 value: nothing has been
// evaluated yet. Both must be REJECTED. This is the control proving the defect
// was fixed at the EMITTER (`verify_signatures_handoff` now emits -1) and not
// by relaxing `phase_a_native_control_is_bound` to accept 0 -- a relaxation
// would diverge from the TypeScript reference (invariant 8) and make two
// encodings of the same semantic state both bindable.
test signatures_handoff_rejects_a_stage_zero_control_claiming_a_verdict() {
  and {
    !signatures_handoff_with_claimed_result(0),
    !signatures_handoff_with_claimed_result(1),
  }
}

// POSITIVE pair for the control above: the sentinel is what production emits.
test signatures_handoff_emits_the_not_yet_evaluated_sentinel() {
  signatures_handoff_with_claimed_result(-1)
}

test signatures_proves_a_missing_required_signer_is_an_exact_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      required_signers: [h_28_a],
    },
  }
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(tx)
  let signer_frontier = validation_merkle_v1.build_frontier([])
  let required_witness_cbor = signatures_scan_witness(
    tx,
    1,
    0,
    -1,
    0,
    0,
    #"",
    signer_frontier,
    -1,
  )
  let required_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 5,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      5,
      required_witness_cbor,
    ),
  }
  let rejection_code = rejection_reason_v1.RequiredSignerUnsigned {
    signer_index: 0,
  }
  and {
    vm_signatures.verify_signatures_advance_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: required_pre,
      },
    ),
    vm_signatures.verify_required_signer_item_semantics_v1(
      required_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: required_witness_cbor,
        claimed_successor: exact_rejection_post(required_pre, rejection_code),
      },
      inline_field_door(),
      inline_field_carriage([h_28_a]),
      vm_machine_types.EmptySignerSetProof { peaks: [] },
    ),
  }
}

/// `tx`'s signatures scan at its required-signer stage: `address_count`
/// address witnesses scanned into `signer_frontier`, `required_seen` of the
/// `required_count` required signers read. Returns the state and its work
/// witness.
fn required_signer_step(
  tx: MidgardTransaction,
  address_count: Int,
  required_count: Int,
  required_seen: Int,
  signer_frontier: validation_merkle_v1.BuiltFrontier,
  first_invalid_witness_index: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let Pair(pre, _) = signatures_step_fixture(tx)
  let witness_cbor = signatures_scan_witness(
    tx,
    1,
    address_count,
    required_count,
    address_count,
    required_seen,
    #"",
    #"",
    signer_frontier,
    first_invalid_witness_index,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..pre,
      program_counter: 5,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.Signatures,
        5,
        witness_cbor,
      ),
    },
    witness_cbor,
  )
}

/// The signatures-phase successor of `pre` whose work witness is
/// `witness_cbor`.
fn signatures_successor(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      pre.program_counter + 1,
      witness_cbor,
    ),
  }
}

/// The required-signer item semantic at `step`, the field `required_signers`
/// carried inline, claiming `post`.
fn required_signer_item_holds(
  step: Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray>,
  required_signers: List<ByteArray>,
  proof: vm_machine_types.SignerSetProofV1,
  post: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  let Pair(pre, work_witness_cbor) = step
  vm_signatures.verify_required_signer_item_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    inline_field_door(),
    inline_field_carriage(required_signers),
    proof,
  )
}

/// A transaction one zero-key address witness signs, requiring
/// `required_signers`.
fn zero_key_signed_tx(required_signers: List<ByteArray>) -> MidgardTransaction {
  let base = empty_v1_transaction()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      required_signers,
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: [
        MidgardAddressWitness {
          verification_key: zero,
          signature: zero |> bytearray.concat(zero),
        },
      ],
    },
  }
}

test signatures_completes_the_scan_on_a_present_last_required_signer() {
  let signer_hash = blake2b_224(zero)
  let tx = zero_key_signed_tx([signer_hash])
  let frontier = signer_frontier_for_tx(tx)
  let step = required_signer_step(tx, 1, -1, 0, frontier, -1)
  let Pair(pre, _) = step
  let proof = vm_machine_types.SignerMembershipProof {
    peaks: frontier.peaks,
    signer_index: 0,
    siblings: [],
  }
  and {
    required_signer_item_holds(
      step,
      [signer_hash],
      proof,
      signatures_successor(
        pre,
        signatures_scan_witness(tx, 2, 1, 1, 1, 1, #"", #"", frontier, -1),
      ),
    ),
    !required_signer_item_holds(
      step,
      [signer_hash],
      proof,
      signatures_successor(
        pre,
        signatures_scan_witness(tx, 1, 1, 1, 1, 1, #"", #"", frontier, -1),
      ),
    ),
  }
}

test signatures_rejects_an_invalid_address_signature_after_the_last_required_signer() {
  let signer_hash = blake2b_224(zero)
  let tx = zero_key_signed_tx([signer_hash])
  let frontier = signer_frontier_for_tx(tx)
  let step = required_signer_step(tx, 1, -1, 0, frontier, 0)
  let Pair(pre, _) = step
  let proof = vm_machine_types.SignerMembershipProof {
    peaks: frontier.peaks,
    signer_index: 0,
    siblings: [],
  }
  and {
    required_signer_item_holds(
      step,
      [signer_hash],
      proof,
      exact_rejection_post(
        pre,
        rejection_reason_v1.AddressWitnessSignatureInvalid { witness_index: 0 },
      ),
    ),
    !required_signer_item_holds(
      step,
      [signer_hash],
      proof,
      signatures_successor(
        pre,
        signatures_scan_witness(tx, 2, 1, 1, 1, 1, #"", #"", frontier, 0),
      ),
    ),
  }
}

test signatures_refuses_a_required_signer_membership_under_other_peaks() {
  // The required signer is not the one signer, so its membership under a
  // frontier holding only itself proves nothing about this transaction.
  let tx = zero_key_signed_tx([h_28_a])
  let frontier = signer_frontier_for_tx(tx)
  let forged = validation_merkle_v1.build_frontier(
    [script_proof_v1.signer_leaf_hash(h_28_a)],
  )
  let step = required_signer_step(tx, 1, -1, 0, frontier, -1)
  let Pair(pre, _) = step
  !required_signer_item_holds(
    step,
    [h_28_a],
    vm_machine_types.SignerMembershipProof {
      peaks: forged.peaks,
      signer_index: 0,
      siblings: [],
    },
    signatures_successor(
      pre,
      signatures_scan_witness(tx, 2, 1, 1, 1, 1, #"", #"", frontier, -1),
    ),
  )
}

/// The required-signer item semantic proving `required_signers[required_seen]`
/// of an unsigned transaction requiring `required_signers` unsigned, with the
/// control claiming `required_count` required signers.
fn proves_required_signer_unsigned(
  required_signers: List<ByteArray>,
  required_count: Int,
  required_seen: Int,
) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      required_signers,
    },
  }
  let step = required_signer_step(
    tx,
    0,
    required_count,
    required_seen,
    validation_merkle_v1.build_frontier([]),
    -1,
  )
  let Pair(pre, _) = step
  required_signer_item_holds(
    step,
    required_signers,
    vm_machine_types.EmptySignerSetProof { peaks: [] },
    exact_rejection_post(
      pre,
      rejection_reason_v1.RequiredSignerUnsigned { signer_index: required_seen },
    ),
  )
}

test signatures_refuses_a_required_signer_count_other_than_the_field_count() {
  and {
    proves_required_signer_unsigned([h_28_a], 1, 0),
    !proves_required_signer_unsigned([h_28_a], 2, 0),
  }
}

// Each twin of the first conjunct above, reading an item the field does not
// hold.
test signatures_refuses_a_required_signer_index_past_its_field() fail {
  proves_required_signer_unsigned([h_28_a], 1, 1)
}

test signatures_refuses_a_required_signer_item_of_an_empty_field() fail {
  proves_required_signer_unsigned([], 0, 0)
}

test signatures_proves_an_invalid_address_signature_is_an_exact_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: [
        MidgardAddressWitness {
          verification_key: zero,
          signature: zero |> bytearray.concat(zero),
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(tx)
  let address_witness = MidgardAddressWitness {
    verification_key: zero,
    signature: zero |> bytearray.concat(zero),
  }
  let address_witness_cbor = encode_midgard_address_witness(address_witness)
  let signer_hash = blake2b_224(address_witness.verification_key)
  let signer_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.signer_leaf_hash(signer_hash)],
  )
  let required_witness_cbor = signatures_scan_witness(
    tx,
    1,
    1,
    0,
    1,
    0,
    #"",
    signer_frontier,
    0,
  )
  let required_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 5,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      5,
      required_witness_cbor,
    ),
  }
  let carriage = inline_field_carriage([address_witness_cbor])
  let rejection_code = rejection_reason_v1.AddressWitnessSignatureInvalid {
    witness_index: 0,
  }
  and {
    vm_signatures.verify_signature_address_item_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: required_pre,
      },
      inline_field_door(),
      carriage,
    ),
    vm_signatures.verify_signatures_advance_semantics_v1(
      required_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: required_witness_cbor,
        claimed_successor: exact_rejection_post(required_pre, rejection_code),
      },
    ),
  }
}

test phase_a_native_scripts_evaluates_a_valid_timelock_on_l1() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      validity_interval_start: 100,
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: #"82041864",
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_native_step_fixture(tx)
  let item_cbor = encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: #"82041864",
    },
  )
  let carriage = inline_field_carriage([item_cbor])
  let chunk_proof = singleton_field_item_chunk_proof(6, item_cbor)
  let token_work_witness_cbor = phase_a_native_scan_witness(
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
  let token_pre = next_phase_a_native_state(pre, token_work_witness_cbor)
  let payload_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    7,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    5,
    #"",
    0,
    1,
    -1,
  )
  let payload_pre = next_phase_a_native_state(
    token_pre,
    payload_work_witness_cbor,
  )
  let finalize_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    2,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    bytearray.length(item_cbor),
    #"",
    0,
    1,
    1,
  )
  let finalize_pre = next_phase_a_native_state(
    payload_pre,
    finalize_work_witness_cbor,
  )
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let next_work_witness_cbor = vm_witness.encode_phase_a_script_preconditions_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    False,
    0,
    0,
    #"",
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..finalize_pre,
    phase: validation_trace_v1.PhaseAScriptPreconditions,
    program_counter: finalize_pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      finalize_pre.program_counter + 1,
      next_work_witness_cbor,
    ),
  }
  and {
    vm_phase_a_native_scripts.verify_phase_a_native_item_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: token_pre,
      },
      inline_field_door(),
      6,
      0,
      carriage,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_token_head_semantics_v1(
      token_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: token_work_witness_cbor,
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

test phase_a_native_scripts_resolves_a_nested_signature_one_step_at_a_time() {
  let base = empty_v1_transaction()
  let address_witness = MidgardAddressWitness {
    verification_key: zero,
    signature: zero |> bytearray.concat(zero),
  }
  let signer_hash = blake2b_224(address_witness.verification_key)
  let script_bytes = #"8201818200581c"
    |> bytearray.concat(signer_hash)
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes,
  }
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: [address_witness],
      script_tx_wits: [script],
    },
  }
  let Pair(pre, _initial_work_witness_cbor) = phase_a_native_step_fixture(tx)
  let item_cbor = encode_midgard_versioned_script(script)
  let chunk_proof = singleton_field_item_chunk_proof(6, item_cbor)
  let payload_offset = bytearray.length(item_cbor) - bytearray.length(
    script_bytes,
  )
  let child_offset = payload_offset + 3
  let item_length = bytearray.length(item_cbor)
  let item_commitment = bounded_item_v1.from_bytes(6, 0, item_cbor)
  let container_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    item_length,
    item_commitment,
    payload_offset,
    #"",
    0,
    0,
    -1,
  )
  let container_pre = next_phase_a_native_state(
    pre,
    container_work_witness_cbor,
  )
  let container_payload_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    4,
    1,
    0,
    0,
    item_length,
    item_commitment,
    payload_offset + 2,
    #"",
    0,
    1,
    -1,
  )
  let container_payload_pre = next_phase_a_native_state(
    container_pre,
    container_payload_work_witness_cbor,
  )
  let frame = native_script_scan_v1.NativeScriptFrameV1 {
    tail: #"",
    kind: native_script_scan_v1.all_node,
    child_count: 1,
    remaining: 1,
    valid_count: 0,
    required: 0,
  }
  let stack_root = native_script_scan_v1.hash_frame_v1(frame)
  let signature_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    item_length,
    item_commitment,
    child_offset,
    stack_root,
    1,
    1,
    -1,
  )
  let signature_pre = next_phase_a_native_state(
    container_payload_pre,
    signature_work_witness_cbor,
  )
  let signature_payload_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    3,
    1,
    0,
    0,
    item_length,
    item_commitment,
    child_offset + 2,
    stack_root,
    1,
    2,
    -1,
  )
  let signature_payload_pre = next_phase_a_native_state(
    signature_pre,
    signature_payload_work_witness_cbor,
  )
  let frame_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    2,
    1,
    0,
    0,
    item_length,
    item_commitment,
    item_length,
    stack_root,
    1,
    2,
    1,
  )
  let frame_pre = next_phase_a_native_state(
    signature_payload_pre,
    frame_work_witness_cbor,
  )
  let complete_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    2,
    1,
    0,
    0,
    item_length,
    item_commitment,
    item_length,
    #"",
    0,
    2,
    1,
  )
  let complete_pre = next_phase_a_native_state(
    frame_pre,
    complete_work_witness_cbor,
  )
  let signer_frontier = signer_frontier_for_tx(tx)
  and {
    vm_phase_a_native_scripts.verify_phase_a_native_token_head_semantics_v1(
      container_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: container_work_witness_cbor,
        claimed_successor: container_payload_pre,
      },
      chunk_proof,
      None,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_all_or_any_container_frame_payload_semantics_v1(
      container_payload_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: container_payload_work_witness_cbor,
        claimed_successor: signature_pre,
      },
      chunk_proof,
      None,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_token_head_semantics_v1(
      signature_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: signature_work_witness_cbor,
        claimed_successor: signature_payload_pre,
      },
      chunk_proof,
      None,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_signature_membership_payload_semantics_v1(
      signature_payload_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: signature_payload_work_witness_cbor,
        claimed_successor: frame_pre,
      },
      chunk_proof,
      None,
      signer_frontier.peaks,
      0,
      [],
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_frame_semantics_v1(
      frame_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: frame_work_witness_cbor,
        claimed_successor: complete_pre,
      },
      frame,
    ),
  }
}

test phase_a_native_scripts_proves_an_unsatisfied_script_is_an_exact_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: #"8200581c"
            |> bytearray.concat(h_28_a),
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_native_step_fixture(tx)
  let item_cbor = encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: #"8200581c"
        |> bytearray.concat(h_28_a),
    },
  )
  let carriage = inline_field_carriage([item_cbor])
  let chunk_proof = singleton_field_item_chunk_proof(6, item_cbor)
  let token_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    1,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    4,
    #"",
    0,
    0,
    -1,
  )
  let token_pre = next_phase_a_native_state(pre, token_work_witness_cbor)
  let payload_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    3,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    6,
    #"",
    0,
    1,
    -1,
  )
  let payload_pre = next_phase_a_native_state(
    token_pre,
    payload_work_witness_cbor,
  )
  let finalize_work_witness_cbor = phase_a_native_scan_witness(
    tx,
    2,
    1,
    0,
    0,
    bytearray.length(item_cbor),
    bounded_item_v1.from_bytes(6, 0, item_cbor),
    bytearray.length(item_cbor),
    #"",
    0,
    1,
    0,
  )
  let finalize_pre = next_phase_a_native_state(
    payload_pre,
    finalize_work_witness_cbor,
  )
  let rejection_code = rejection_reason_v1.WitnessNativeScriptFalse {
    script_index: 0,
  }
  // An exact rejection ends the machine with the rejected verdict and its
  // reason's code; `exact_rejection_post` is the canonical successor shape.
  let terminal = exact_rejection_post(finalize_pre, rejection_code)
  and {
    vm_phase_a_native_scripts.verify_phase_a_native_item_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: token_pre,
      },
      inline_field_door(),
      6,
      0,
      carriage,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_token_head_semantics_v1(
      token_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: token_work_witness_cbor,
        claimed_successor: payload_pre,
      },
      chunk_proof,
      None,
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_signature_empty_payload_semantics_v1(
      payload_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: payload_work_witness_cbor,
        claimed_successor: finalize_pre,
      },
      chunk_proof,
      None,
      [],
    ),
    vm_phase_a_native_scripts.verify_phase_a_native_advance_semantics_v1(
      finalize_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: finalize_work_witness_cbor,
        claimed_successor: terminal,
      },
    ),
  }
}

test phase_a_script_preconditions_advances_an_empty_bundle_to_resolution() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(tx)
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.ResolveInputs,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      7,
      vm_witness.encode_resolve_inputs_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        0,
        vm_witness.transaction_resolution_schedule_hash(
          tx.body.inputs,
          tx.body.reference_inputs,
        ),
        signer_frontier.count,
        validation_merkle_v1.frontier_commitment(
          signer_frontier.count,
          signer_frontier.peaks,
        ),
        None,
        vm_witness.transaction_resolution_schedule_hash(
          tx.body.inputs,
          tx.body.reference_inputs,
        ),
      ),
    ),
  }
  let witness = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  and {
    vm_preconditions.verify_phase_a_script_preconditions_finalize_semantics_v1(
      pre,
      witness,
    ),
    vm_one_step.verify_one_step_evidence(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: witness,
        auxiliary: vm_machine_types.NoAuxiliaryWitness,
      },
      inline_field_door(),
      test_l1_slot_config,
    ),
  }
}

test phase_a_script_preconditions_proves_duplicate_observers_are_a_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      required_observers: [h_28_a, h_28_a],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(tx)
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let next_work_witness_cbor = vm_witness.encode_phase_a_script_preconditions_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    contains_non_native_script(tx.witness_set.script_tx_wits),
    2,
    1,
    h_28_a,
  )
  let next = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.PhaseAScriptPreconditions,
    program_counter: 7,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      7,
      next_work_witness_cbor,
    ),
  }
  let rejection_code = rejection_reason_v1.ObserverOrderInvalid {
    observer_index: 1,
  }
  let terminal = exact_rejection_post(next, rejection_code)
  // The duplicate is now the field's own §5.1 preimage rather than two
  // openings over one frontier, and which of its two identical items a step
  // reads is the `item_index` it names.
  let observers_carriage = duplicate_observer_field_carriage()
  let first_evidence = one_step_evidence(
    work_witness_cbor,
    next,
    vm_machine_types.TransactionFieldChunkWitness {
      field_index: 3,
      item_index: 0,
      carriage: observers_carriage,
    },
  )
  let second_evidence = one_step_evidence(
    next_work_witness_cbor,
    terminal,
    vm_machine_types.TransactionFieldChunkWitness {
      field_index: 3,
      item_index: 1,
      carriage: observers_carriage,
    },
  )
  and {
    vm_preconditions.verify_phase_a_script_preconditions_item_semantics_v1(
      pre,
      first_evidence.transition,
      inline_field_door(),
      3,
      0,
      observers_carriage,
    ),
    vm_preconditions.verify_phase_a_script_preconditions_item_semantics_v1(
      next,
      second_evidence.transition,
      inline_field_door(),
      3,
      1,
      observers_carriage,
    ),
    !vm_preconditions.verify_phase_a_script_preconditions_finalize_semantics_v1(
      pre,
      first_evidence.transition,
    ),
    vm_one_step.verify_one_step_evidence(
      pre,
      first_evidence,
      inline_field_door(),
      test_l1_slot_config,
    ),
    vm_one_step.verify_one_step_evidence(
      next,
      second_evidence,
      inline_field_door(),
      test_l1_slot_config,
    ),
  }
}

test phase_a_script_preconditions_require_integrity_for_plutus_bytes() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: PlutusV3Script,
          script_bytes: v1_plutus_script(),
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(tx)
  let rejection_code = rejection_reason_v1.ScriptIntegrityHashMissing
  // Canonical exact-rejection successor.
  let terminal = exact_rejection_post(pre, rejection_code)
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      terminal,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
    test_l1_slot_config,
  )
}

test resolve_inputs_checks_validity_before_ledger_lookups() {
  resolve_inputs_passes_the_validity_check(-1, -1)
}

// The fixture's block slot is 100. `invalid_hereafter` is exclusive, so an
// upper bound of 101 admits slot 100 and the one-slot interval `[100, 101)`
// contains it.
test resolve_inputs_accepts_a_block_slot_one_below_the_upper_bound() {
  resolve_inputs_passes_the_validity_check(-1, 101)
}

test resolve_inputs_accepts_a_block_slot_inside_a_one_slot_interval() {
  resolve_inputs_passes_the_validity_check(100, 101)
}

fn resolve_inputs_passes_the_validity_check(
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      validity_interval_start,
      validity_interval_end,
    },
  }
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(tx, h_c, 0, 7)
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let resolution_schedule_hash = vm_witness.transaction_resolution_schedule_hash(
    tx.body.inputs,
    tx.body.reference_inputs,
  )
  let next_work_witness_cbor = vm_witness.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    1,
    resolution_schedule_hash,
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    None,
    resolution_schedule_hash,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      8,
      next_work_witness_cbor,
    ),
  }
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
    test_l1_slot_config,
  )
}

test resolve_inputs_begins_authenticated_membership_output() {
  resolve_inputs_authenticated_membership_step(0)
}

test resolve_inputs_empty_pending_witness_decodes_as_exact_array_10() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let Pair(_, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    null_hash,
    1,
    8,
  )
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let fields = builtin.un_list_data(data)
  expect Some(pending_data) = list.at(fields, 8)
  expect Some(schedule_data) = list.at(fields, 9)
  and {
    list.length(fields) == 10,
    builtin.un_b_data(pending_data) == #"00",
    builtin.un_b_data(schedule_data) == vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

test resolve_inputs_streams_authenticated_membership_output() {
  resolve_inputs_authenticated_membership_step(1)
}

test resolve_inputs_finalizes_authenticated_membership_output() {
  resolve_inputs_authenticated_membership_step(2)
}

test resolve_inputs_proves_non_membership_as_an_exact_no_op() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [input] },
  }
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    null_hash,
    1,
    8,
  )
  let rejection_code = rejection_reason_v1.InputNotFound {
    source_kind: 0,
    input_index: -1,
  }
  // Canonical exact-rejection successor.
  let terminal = exact_rejection_post(pre, rejection_code)
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      terminal,
      vm_machine_types.ScheduledLedgerNonMembershipWitness {
        source_kind: 0,
        item_index: 0,
        key,
        next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
        proof: [],
      },
    ),
    inline_field_door(),
    test_l1_slot_config,
  )
}

test resolve_inputs_rejects_an_invalid_validity_interval_exactly() {
  resolve_inputs_rejects_the_validity_interval_exactly(101, -1)
}

// The fixture's block slot is 100, which an exclusive upper bound of 100 does
// not contain.
test resolve_inputs_rejects_a_block_slot_at_the_exclusive_upper_bound() {
  resolve_inputs_rejects_the_validity_interval_exactly(-1, 100)
}

fn resolve_inputs_rejects_the_validity_interval_exactly(
  validity_interval_start: Int,
  validity_interval_end: Int,
) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      validity_interval_start,
      validity_interval_end,
    },
  }
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(tx, h_c, 0, 7)
  let rejection_code = rejection_reason_v1.ValidityIntervalExcludesBlockSlot
  let terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      8,
      vm_shared.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      terminal,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
    test_l1_slot_config,
  )
}

test resolve_inputs_finalizes_into_script_source_resolution() {
  let tx = empty_v1_transaction()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(tx, h_c, 1, 8)
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.ScriptSources,
    program_counter: 9,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      9,
      vm_witness.encode_script_sources_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        0,
        signer_frontier.count,
        validation_merkle_v1.frontier_commitment(
          signer_frontier.count,
          signer_frontier.peaks,
        ),
        validation_merkle_v1.empty_frontier(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        vm_witness.empty_resolution_schedule_hash(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        vm_witness.empty_receive_purpose_scan_control(),
        0,
        0,
        vm_witness.empty_observer_purpose_scan_control(),
        vm_witness.empty_mint_fold_control(),
        vm_witness.transaction_resolution_schedule_hash(
          tx.body.inputs,
          tx.body.reference_inputs,
        ),
      ),
    ),
  }
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
    test_l1_slot_config,
  )
}

/// The signer leaves of `addresses`, one per run of equal signers (field 7 is
/// in (signer, bytes) order, so a signer's items are adjacent).
fn cursor_signer_leaves(
  addresses: List<MidgardAddressWitness>,
  previous: ByteArray,
) -> List<ByteArray> {
  when addresses is {
    [] -> []
    [address, ..rest] -> {
      let signer = blake2b_224(address.verification_key)
      if signer == previous {
        cursor_signer_leaves(rest, previous)
      } else {
        [
          script_proof_v1.signer_leaf_hash(signer),
          ..cursor_signer_leaves(rest, signer)
        ]
      }
    }
  }
}

/// One Signatures address step over field 7 = `addresses`, from the control
/// after `seen` items to a claimed successor that consumed item `opened`.
/// Every fixture signature is invalid, so the first invalid witness index is
/// `pre_invalid` when set, else the consumed item's field-7 position.
fn signatures_cursor_row(
  addresses: List<MidgardAddressWitness>,
  seen: Int,
  pre_invalid: Int,
  opened: Int,
  post_invalid: Int,
) -> Bool {
  signatures_cursor_row_declaring(
    addresses,
    list.length(addresses),
    seen,
    pre_invalid,
    opened,
    post_invalid,
  )
}

/// `signatures_cursor_row` from a control (when `seen > 0`) that declares
/// `count` field-7 items, to the successor that declaration yields.
fn signatures_cursor_row_declaring(
  addresses: List<MidgardAddressWitness>,
  count: Int,
  seen: Int,
  pre_invalid: Int,
  opened: Int,
  post_invalid: Int,
) -> Bool {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: addresses,
    },
  }
  let consumed = list.take(addresses, seen)
  expect Some(opened_address) = list.at(addresses, opened)
  let previous_signer = when list.last(consumed) is {
    Some(address) -> blake2b_224(address.verification_key)
    None -> #""
  }
  let Pair(initial, initial_witness_cbor) = signatures_step_fixture(tx)
  let work_witness_cbor = if seen == 0 {
    initial_witness_cbor
  } else {
    signatures_scan_witness(
      tx,
      0,
      count,
      0,
      seen,
      0,
      previous_signer,
      validation_merkle_v1.build_frontier(cursor_signer_leaves(consumed, #"")),
      pre_invalid,
    )
  }
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..initial,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      initial.program_counter,
      work_witness_cbor,
    ),
  }
  let next_consumed = list.concat(consumed, [opened_address])
  let next = signatures_scan_witness(
    tx,
    if seen + 1 == count {
      1
    } else {
      0
    },
    count,
    0,
    seen + 1,
    0,
    if seen + 1 == count {
      #""
    } else {
      blake2b_224(opened_address.verification_key)
    },
    validation_merkle_v1.build_frontier(
      cursor_signer_leaves(next_consumed, #""),
    ),
    post_invalid,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      pre.program_counter + 1,
      next,
    ),
  }
  vm_signatures.verify_signature_address_item_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    inline_field_door(),
    inline_field_carriage(list.map(addresses, encode_midgard_address_witness)),
  )
}

fn cursor_address(
  key: ByteArray,
  signature_head: ByteArray,
) -> MidgardAddressWitness {
  MidgardAddressWitness {
    verification_key: key,
    signature: signature_head |> bytearray.concat(zero),
  }
}

// blake2b_224(h_a) < blake2b_224(zero), so [h_a key, zero key] is field 7 in
// canonical order and [zero key, h_a key] is not.
fn cursor_ascending_pair() -> List<MidgardAddressWitness> {
  [cursor_address(h_a, zero), cursor_address(zero, zero)]
}

// One signer, three items in ascending signature bytes.
fn cursor_one_signer_triple() -> List<MidgardAddressWitness> {
  [
    cursor_address(zero, zero),
    cursor_address(zero, h_a),
    cursor_address(zero, h_b),
  ]
}

test signatures_cursor_order_premise() {
  and {
    bytearray.compare(blake2b_224(h_a), blake2b_224(zero)) == Less,
    bytearray.compare(zero, h_a) == Less,
    bytearray.compare(h_a, h_b) == Less,
  }
}

test signatures_cursor_opens_the_item_at_address_seen() {
  signatures_cursor_row(cursor_ascending_pair(), 0, -1, 0, 0)
}

test signatures_refuses_an_alternative_authentic_first_choice() {
  !signatures_cursor_row(cursor_ascending_pair(), 0, -1, 1, 1)
}

// Before the cursor this successor was accepted: the step took the least
// (signer, bytes) key wherever it sat in field 7. It is now refused, so field
// 7's wire order is no longer the prover's choice.
test signatures_refuses_the_least_key_at_a_later_physical_index() {
  let reordered = [cursor_address(zero, zero), cursor_address(h_a, zero)]
  !signatures_cursor_row(reordered, 0, -1, 1, 1)
}

test signatures_cursor_opens_a_middle_item_and_keeps_one_signer_leaf() {
  signatures_cursor_row(cursor_one_signer_triple(), 1, 0, 1, 0)
}

// The skip successor names item 2 as the first invalid witness; the cursor
// derives item 1, so only the successor naming item 1 verifies.
test signatures_cursor_refuses_skipping_a_middle_item() {
  and {
    signatures_cursor_row(cursor_one_signer_triple(), 1, -1, 1, 1),
    !signatures_cursor_row(cursor_one_signer_triple(), 1, -1, 2, 2),
  }
}

test signatures_cursor_completes_on_the_last_item() {
  signatures_cursor_row(cursor_one_signer_triple(), 2, 0, 2, 0)
}

// The address step does not re-check the declared count: a control that
// declares no items, or a count other than field 7's, is refused by
// `active_count > 0` and `item_count == active_count`.
test signatures_cursor_refuses_a_declared_count_other_than_field_seven() {
  and {
    signatures_cursor_row_declaring(cursor_one_signer_triple(), 3, 1, 0, 1, 0),
    !signatures_cursor_row_declaring(cursor_one_signer_triple(), 0, 1, 0, 1, 0),
    !signatures_cursor_row_declaring(cursor_one_signer_triple(), 2, 1, 0, 1, 0),
    !signatures_cursor_row_declaring(cursor_one_signer_triple(), 4, 1, 0, 1, 0),
  }
}

// I10: the AddressWitnessSignatureInvalid witness_index is the field-7
// position of the first invalid item the cursor reaches.
test signatures_cursor_names_the_first_invalid_witness_by_position() {
  and {
    signatures_cursor_row(cursor_one_signer_triple(), 1, -1, 1, 1),
    !signatures_cursor_row(cursor_one_signer_triple(), 1, -1, 1, 0),
    !signatures_cursor_row(cursor_one_signer_triple(), 1, -1, 1, 2),
  }
}

fn signer_scan_step(
  tx: MidgardTransaction,
  stage: Int,
  address_count: Int,
  required_count: Int,
  address_seen: Int,
  required_seen: Int,
  signer_frontier: validation_merkle_v1.BuiltFrontier,
  first_invalid_witness_index: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let Pair(pre, _) = signatures_step_fixture(tx)
  let witness_cbor = signatures_scan_witness(
    tx,
    stage,
    address_count,
    required_count,
    address_seen,
    required_seen,
    #"",
    #"",
    signer_frontier,
    first_invalid_witness_index,
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..pre,
      program_counter: 5,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.Signatures,
        5,
        witness_cbor,
      ),
    },
    witness_cbor,
  )
}

test signatures_required_first_of_two_present_continues_at_stage_one() {
  let signer_hash = blake2b_224(zero)
  let tx = zero_key_signed_tx([signer_hash, signer_hash])
  let frontier = signer_frontier_for_tx(tx)
  let step = required_signer_step(tx, 1, -1, 0, frontier, -1)
  let Pair(pre, _) = step
  let proof = vm_machine_types.SignerMembershipProof {
    peaks: frontier.peaks,
    signer_index: 0,
    siblings: [],
  }
  and {
    required_signer_item_holds(
      step,
      [signer_hash, signer_hash],
      proof,
      signatures_successor(
        pre,
        signatures_scan_witness(tx, 1, 1, 2, 1, 1, #"", #"", frontier, -1),
      ),
    ),
    !required_signer_item_holds(
      step,
      [signer_hash, signer_hash],
      proof,
      signatures_successor(
        pre,
        signatures_scan_witness(tx, 2, 1, 2, 1, 1, #"", #"", frontier, -1),
      ),
    ),
  }
}

test signatures_required_invalid_address_signature_waits_for_the_last_required_signer() {
  let signer_hash = blake2b_224(zero)
  let tx = zero_key_signed_tx([signer_hash, signer_hash])
  let frontier = signer_frontier_for_tx(tx)
  let step = required_signer_step(tx, 1, -1, 0, frontier, 0)
  let Pair(pre, _) = step
  let proof = vm_machine_types.SignerMembershipProof {
    peaks: frontier.peaks,
    signer_index: 0,
    siblings: [],
  }
  and {
    required_signer_item_holds(
      step,
      [signer_hash, signer_hash],
      proof,
      signatures_successor(
        pre,
        signatures_scan_witness(tx, 1, 1, 2, 1, 1, #"", #"", frontier, 0),
      ),
    ),
    !required_signer_item_holds(
      step,
      [signer_hash, signer_hash],
      proof,
      exact_rejection_post(
        pre,
        rejection_reason_v1.AddressWitnessSignatureInvalid { witness_index: 0 },
      ),
    ),
  }
}

test signatures_required_unsigned_second_required_signer_names_its_index() {
  proves_required_signer_unsigned([h_28_a, h_28_a], 2, 1)
}

test signatures_required_item_refused_before_the_address_scan() {
  let tx = zero_key_signed_tx([h_28_a])
  let empty = validation_merkle_v1.build_frontier([])
  let step = signer_scan_step(tx, 0, -1, -1, 0, 0, empty, -1)
  let Pair(pre, _) = step
  !required_signer_item_holds(
    step,
    [h_28_a],
    vm_machine_types.EmptySignerSetProof { peaks: [] },
    exact_rejection_post(
      pre,
      rejection_reason_v1.RequiredSignerUnsigned { signer_index: 0 },
    ),
  )
}

test signatures_required_item_refused_on_an_unbound_control() {
  let tx = zero_key_signed_tx([h_28_a])
  let empty = validation_merkle_v1.build_frontier([])
  let step = signer_scan_step(tx, 1, 1, -1, 0, 0, empty, -1)
  let Pair(pre, _) = step
  !required_signer_item_holds(
    step,
    [h_28_a],
    vm_machine_types.EmptySignerSetProof { peaks: [] },
    exact_rejection_post(
      pre,
      rejection_reason_v1.RequiredSignerUnsigned { signer_index: 0 },
    ),
  )
}
