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
  h_28_a, h_a, h_b, h_c, network_mismatch_rejection_code,
  validation_context_cbor, zero,
}
use midgard/validation_machine_tests/fixtures_input_resolution.{
  resolve_inputs_authenticated_membership_step,
  signatures_handoff_with_claimed_result,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  contains_non_native_script, duplicate_observer_field_carriage,
  empty_v1_transaction, exact_rejection_post, initial_signatures_witness,
  inline_field_carriage, inline_field_door, input_sets_pre,
  input_sets_scan_witness, next_phase_a_native_state,
  non_empty_claimed_delta_root, one_step_evidence, phase_a_native_scan_witness,
  phase_a_native_step_fixture, phase_a_script_preconditions_fixture,
  resolve_inputs_step_fixture, signatures_scan_witness, signatures_step_fixture,
  signer_frontier_for_tx, singleton_field_item_chunk_proof,
  static_rules_delta_root_fixture, submitted_source, v1_program_envelope,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// POSITIVE CONTROL for the restored capability (GOAL_SPEC §3 invariant 9).
// This is the governing adversarial case: an operator commits an `Accepted`
// descriptor -- mandatory for every normal L2 source -- over a transaction that
// truly rejects, and therefore carries a real, NON-EMPTY claimed delta root.
// Before the fix no challenger could construct any winning rejection successor
// from such a pre-state, so a dishonest operator won every dispute by default.
// The test also pins that the claim is carried forward, not cleared.
test rejection_is_provable_from_a_non_empty_claimed_delta_root() {
  let claimed = non_empty_claimed_delta_root()
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    False,
    claimed,
  )
  let terminal = exact_rejection_post(pre, network_mismatch_rejection_code)
  and {
    claimed != validation_merkle_v1.frontier_commitment(0, []),
    terminal.ledger_delta_root == claimed,
    vm_one_step.verify_one_step(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      inline_field_door(),
    ),
  }
}

// NEGATIVE CONTROL -- mutation (clearing). The old, deleted production clause
// demanded exactly this successor shape. It must now be REJECTED, because the
// claimed delta root is immutable context. This is the control that stops
// anyone "restoring" the clearing clause, or exempting the field from
// `immutable_context_matches`, without turning a test red.
test rejection_successor_that_clears_the_claimed_delta_is_rejected() {
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    False,
    non_empty_claimed_delta_root(),
  )
  let cleared_terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..exact_rejection_post(pre, network_mismatch_rejection_code),
    ledger_delta_root: validation_merkle_v1.frontier_commitment(0, []),
  }
  !vm_one_step.verify_one_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: cleared_terminal,
    },
    inline_field_door(),
  )
}

// NEGATIVE CONTROL -- mutation (rewriting). Immutability holds in both
// directions: the successor may not substitute any other delta root either.
test rejection_successor_that_rewrites_the_claimed_delta_is_rejected() {
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    False,
    non_empty_claimed_delta_root(),
  )
  let rewritten_terminal = validation_trace_v1.ValidationMachineStateV1 {
    ..exact_rejection_post(pre, network_mismatch_rejection_code),
    ledger_delta_root: h_b,
  }
  !vm_one_step.verify_one_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: rewritten_terminal,
    },
    inline_field_door(),
  )
}

// NEGATIVE CONTROL -- valid-block rejection (GOAL_SPEC §3 invariant 9). THE
// critical control for the delta-root fix: deleting the clearing clause must
// not have made honest blocks challengeable. Here the transaction satisfies the
// static ledger rules, so its true one-step is an advance to `InputSets`; a
// claimed rejection terminal -- otherwise perfectly well-formed, and carrying
// the claimed delta forward exactly as the now-passing rejection tests do --
// must still be unprovable.
test a_valid_static_rules_transition_cannot_be_proven_rejected() {
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    True,
    non_empty_claimed_delta_root(),
  )
  !vm_one_step.verify_one_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(
        pre,
        network_mismatch_rejection_code,
      ),
    },
    inline_field_door(),
  )
}

// The ONE deliberately-empty claimed-delta case. An honest, self-consistent
// forced-invalid trace built by the TypeScript reference carries
// `hashMidgardValidationLedgerDeltaV1([]) == frontier_commitment(0, [])` on
// every state, so an empty claim is spec-legal in exactly this scenario. It is
// kept as a single labelled case rather than as the framing for the whole
// rejection surface: as the only framing it hid the defect, because it is the
// one pre-state in which the deleted clause was satisfiable.
test an_honest_forced_invalid_trace_may_carry_an_empty_claimed_delta() {
  let empty_claim = validation_merkle_v1.frontier_commitment(0, [])
  let Pair(pre, work_witness_cbor) = static_rules_delta_root_fixture(
    False,
    empty_claim,
  )
  let terminal = exact_rejection_post(pre, network_mismatch_rejection_code)
  and {
    pre.source_kind == validation_trace_v1.Forced,
    terminal.ledger_delta_root == empty_claim,
    vm_one_step.verify_one_step(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: terminal,
      },
      inline_field_door(),
    ),
  }
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
    vm_witness.empty_resolution_schedule_hash(),
  )
  let input_pre = input_sets_pre(tx, 2, work_witness_cbor)
  let rejection_code = #"455f454d5054595f494e50555453"
  vm_input_sets.verify_input_sets_empty_semantics_v1(
    input_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(input_pre, rejection_code),
    },
  )
}

test input_sets_accepts_bounded_disjoint_spend_and_reference_inputs() {
  let spend_input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let reference_input = MidgardTxInput { tx_id: h_b, output_index: 1 }
  let spend_key = encode_midgard_tx_input(spend_input)
  let reference_key = encode_midgard_tx_input(reference_input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [spend_input],
      reference_inputs: [reference_input],
    },
  }
  let initial_schedule = vm_witness.empty_resolution_schedule_hash()
  let first_schedule = vm_witness.resolution_schedule_node_hash(
    1,
    reference_key,
    initial_schedule,
  )
  let full_schedule = vm_witness.resolution_schedule_node_hash(
    0,
    spend_key,
    first_schedule,
  )
  let first_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    -1,
    0,
    0,
    #"",
    initial_schedule,
  )
  let first_pre = input_sets_pre(tx, 2, first_witness_cbor)
  let second_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    1,
    0,
    1,
    reference_key,
    first_schedule,
  )
  let second_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..first_pre,
    program_counter: 3,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.InputSets,
      3,
      second_witness_cbor,
    ),
  }
  let reference_carriage = inline_field_carriage([reference_key])
  let first_is_valid = vm_input_sets.verify_input_sets_item_semantics_v1(
    first_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: first_witness_cbor,
      claimed_successor: second_pre,
    },
    inline_field_door(),
    1,
    0,
    reference_carriage,
  )
  let signatures_witness_cbor = initial_signatures_witness(tx)
  let signatures_post = validation_trace_v1.ValidationMachineStateV1 {
    ..second_pre,
    phase: validation_trace_v1.Signatures,
    program_counter: 4,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      4,
      signatures_witness_cbor,
    ),
  }
  let spend_carriage = inline_field_carriage([spend_key])
  and {
    first_is_valid,
    vm_input_sets.verify_input_sets_item_semantics_v1(
      second_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: second_witness_cbor,
        claimed_successor: signatures_post,
      },
      inline_field_door(),
      0,
      0,
      spend_carriage,
    ),
    full_schedule == vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

test input_sets_proves_spend_reference_overlap_unilaterally() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [input],
      reference_inputs: [input],
    },
  }
  let initial_schedule = vm_witness.empty_resolution_schedule_hash()
  let first_schedule = vm_witness.resolution_schedule_node_hash(
    1,
    key,
    initial_schedule,
  )
  let first_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    -1,
    0,
    0,
    #"",
    initial_schedule,
  )
  let first_pre = input_sets_pre(tx, 2, first_witness_cbor)
  let second_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    1,
    0,
    1,
    key,
    first_schedule,
  )
  let second_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..first_pre,
    program_counter: 3,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.InputSets,
      3,
      second_witness_cbor,
    ),
  }
  let reference_carriage = inline_field_carriage([key])
  let spend_carriage = inline_field_carriage([key])
  let first_is_valid = vm_input_sets.verify_input_sets_item_semantics_v1(
    first_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: first_witness_cbor,
      claimed_successor: second_pre,
    },
    inline_field_door(),
    1,
    0,
    reference_carriage,
  )
  let rejection_code = #"455f4455504c49434154455f494e5055545f494e5f5458"
  and {
    first_is_valid,
    vm_input_sets.verify_input_sets_item_semantics_v1(
      second_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: second_witness_cbor,
        claimed_successor: exact_rejection_post(second_pre, rejection_code),
      },
      inline_field_door(),
      0,
      0,
      spend_carriage,
    ),
  }
}

test input_sets_proves_a_malformed_validity_interval_after_the_final_item() {
  let input = MidgardTxInput { tx_id: h_a, output_index: 0 }
  let key = encode_midgard_tx_input(input)
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [input],
      validity_interval_start: 10,
      validity_interval_end: 9,
    },
  }
  let work_witness_cbor = input_sets_scan_witness(
    tx,
    -1,
    0,
    0,
    0,
    #"",
    vm_witness.empty_resolution_schedule_hash(),
  )
  let pre = input_sets_pre(tx, 2, work_witness_cbor)
  let carriage = inline_field_carriage([key])
  let rejection_code = #"455f494e56414c49445f56414c49444954595f494e54455256414c5f464f524d4154"
  vm_input_sets.verify_input_sets_item_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: exact_rejection_post(pre, rejection_code),
    },
    inline_field_door(),
    0,
    0,
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
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(tx, h_a)
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
  let handoff_witness_cbor = signatures_scan_witness(
    tx,
    2,
    0,
    0,
    0,
    0,
    #"",
    #"",
    signer_frontier,
    0,
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
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(
    tx,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let signer_frontier = validation_merkle_v1.build_frontier([])
  let required_witness_cbor = signatures_scan_witness(
    tx,
    1,
    0,
    -1,
    0,
    0,
    #"",
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
  let rejection_code = #"455f4d495353494e475f52455155495245445f5749544e455353"
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
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(
    tx,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
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
    #"",
    signer_frontier,
    1,
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
  let rejection_code = #"455f494e56414c49445f5349474e4154555245"
  and {
    vm_signatures.verify_signature_address_item_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: required_pre,
      },
      inline_field_door(),
      7,
      0,
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
  let Pair(pre, work_witness_cbor) = phase_a_native_step_fixture(tx, h_a)
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
  let Pair(pre, _initial_work_witness_cbor) = phase_a_native_step_fixture(
    tx,
    h_a,
  )
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
  let Pair(pre, work_witness_cbor) = phase_a_native_step_fixture(
    tx,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
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
  let rejection_code = #"455f4e41544956455f5343524950545f494e56414c4944"
  // An exact rejection also clears the ledger delta to the empty frontier
  // commitment; `exact_rejection_post` is the canonical successor shape.
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
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(
    tx,
    h_a,
  )
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
        vm_witness.initial_resolution_accumulator(),
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
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(
    tx,
    validation_trace_v1.hash_ledger_delta(#"80"),
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
  let rejection_code = #"455f494e56414c49445f4649454c445f54595045"
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
    ),
    vm_one_step.verify_one_step_evidence(
      next,
      second_evidence,
      inline_field_door(),
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
          script_bytes: v1_program_envelope(),
        },
      ],
    },
  }
  let Pair(pre, work_witness_cbor) = phase_a_script_preconditions_fixture(
    tx,
    non_empty_claimed_delta_root(),
  )
  let rejection_code = #"455f494e56414c49445f4649454c445f54595045"
  // Canonical exact-rejection successor: it carries the non-empty claimed delta
  // forward. See the note on
  // `static_rules_prove_a_network_mismatch_is_an_exact_no_op`.
  let terminal = exact_rejection_post(pre, rejection_code)
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      terminal,
      vm_machine_types.NoAuxiliaryWitness,
    ),
    inline_field_door(),
  )
}

test resolve_inputs_checks_validity_before_ledger_lookups() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
    },
  }
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    h_c,
    0,
    accumulator,
    7,
  )
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
    accumulator,
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
  )
}

test resolve_inputs_begins_authenticated_membership_output() {
  resolve_inputs_authenticated_membership_step(0)
}

test resolve_inputs_empty_pending_witness_decodes_as_exact_array_11() {
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
    vm_witness.initial_resolution_accumulator(),
    8,
  )
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let fields = builtin.un_list_data(data)
  expect Some(pending_data) = list.at(fields, 9)
  expect Some(schedule_data) = list.at(fields, 10)
  and {
    list.length(fields) == 11,
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
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    null_hash,
    1,
    accumulator,
    8,
  )
  let rejection_code = #"455f494e5055545f4e4f545f464f554e44"
  // Canonical exact-rejection successor: it carries the non-empty claimed delta
  // (supplied by `resolve_inputs_step_fixture`) forward. See the note on
  // `static_rules_prove_a_network_mismatch_is_an_exact_no_op`.
  let terminal = exact_rejection_post(pre, rejection_code)
  vm_one_step.verify_one_step_evidence(
    pre,
    one_step_evidence(
      work_witness_cbor,
      terminal,
      vm_machine_types.ScheduledLedgerNonMembershipWitness {
        source_kind: 0,
        key,
        next_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
        proof: [],
      },
    ),
    inline_field_door(),
  )
}

test resolve_inputs_rejects_an_invalid_validity_interval_exactly() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      inputs: [MidgardTxInput { tx_id: h_a, output_index: 0 }],
      validity_interval_start: 101,
    },
  }
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    h_c,
    0,
    accumulator,
    7,
  )
  let rejection_code = #"455f56414c49444954595f494e54455256414c5f4d49534d41544348"
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
  )
}

test resolve_inputs_finalizes_into_script_source_resolution() {
  let tx = empty_v1_transaction()
  let accumulator = vm_witness.initial_resolution_accumulator()
  let Pair(pre, work_witness_cbor) = resolve_inputs_step_fixture(
    tx,
    h_c,
    1,
    accumulator,
    8,
  )
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
        accumulator,
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
        vm_witness.initial_resolution_accumulator(),
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
  )
}

test signatures_refuses_an_alternative_authentic_first_choice() {
  let base = empty_v1_transaction()
  let first = MidgardAddressWitness {
    verification_key: h_a,
    signature: zero |> bytearray.concat(zero),
  }
  let second = MidgardAddressWitness {
    verification_key: zero,
    signature: zero |> bytearray.concat(zero),
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: [first, second],
    },
  }
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(tx, h_a)
  let bytes = encode_midgard_address_witness(second)
  let signer_hash = blake2b_224(second.verification_key)
  let frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.signer_leaf_hash(signer_hash)],
  )
  let next = signatures_scan_witness(
    tx,
    0,
    2,
    0,
    1,
    0,
    signer_hash
      |> bytearray.concat(bytes)
      |> bytearray.concat(cbor.serialise(1)),
    signer_hash,
    frontier,
    1,
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
  !vm_signatures.verify_signature_address_item_semantics_v1(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    inline_field_door(),
    7,
    1,
    inline_field_carriage([encode_midgard_address_witness(first), bytes]),
  )
}

test signatures_accepts_the_authentic_minimum_at_reordered_physical_index() {
  let base = empty_v1_transaction()
  let first = MidgardAddressWitness {
    verification_key: zero,
    signature: zero |> bytearray.concat(zero),
  }
  let second = MidgardAddressWitness {
    verification_key: h_a,
    signature: zero |> bytearray.concat(zero),
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: [first, second],
    },
  }
  let Pair(pre, work_witness_cbor) = signatures_step_fixture(tx, h_a)
  let bytes = encode_midgard_address_witness(second)
  let signer_hash = blake2b_224(second.verification_key)
  let frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.signer_leaf_hash(signer_hash)],
  )
  let next = signatures_scan_witness(
    tx,
    0,
    2,
    0,
    1,
    0,
    signer_hash
      |> bytearray.concat(bytes)
      |> bytearray.concat(cbor.serialise(1)),
    signer_hash,
    frontier,
    1,
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
    7,
    1,
    inline_field_carriage([encode_midgard_address_witness(first), bytes]),
  )
}

fn signatures_accepts_duplicate_index_boundary(count: Int, index: Int) -> Bool {
  let base = empty_v1_transaction()
  let address = MidgardAddressWitness {
    verification_key: zero,
    signature: zero |> bytearray.concat(zero),
  }
  let addresses = list.repeat(address, count)
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      addr_tx_wits: addresses,
    },
  }
  let Pair(initial, _) = signatures_step_fixture(tx, h_a)
  let bytes = encode_midgard_address_witness(address)
  let signer_hash = blake2b_224(address.verification_key)
  let prefix = signer_hash |> bytearray.concat(bytes)
  let frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.signer_leaf_hash(signer_hash)],
  )
  let work_witness_cbor = signatures_scan_witness(
    tx,
    0,
    count,
    0,
    index,
    0,
    prefix |> bytearray.concat(cbor.serialise(index - 1)),
    signer_hash,
    frontier,
    1,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..initial,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      initial.program_counter,
      work_witness_cbor,
    ),
  }
  let next = signatures_scan_witness(
    tx,
    0,
    count,
    0,
    index + 1,
    0,
    prefix |> bytearray.concat(cbor.serialise(index)),
    signer_hash,
    frontier,
    1,
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
    7,
    index,
    inline_field_carriage(list.map(addresses, encode_midgard_address_witness)),
  )
}

test signatures_preserves_duplicate_index_cbor_boundary_24() {
  signatures_accepts_duplicate_index_boundary(318, 24)
}

test signatures_preserves_duplicate_index_cbor_boundary_256() {
  signatures_accepts_duplicate_index_boundary(318, 256)
}
