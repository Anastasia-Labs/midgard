use aiken/builtin
use aiken/cbor
use aiken/collection/list
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardVersionedScript, PlutusV3Script, native_tx_version_v1,
}
use midgard/native_tx_field_access_v1
use midgard/validation_machine/canonical_decode as vm_canonical_decode
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_a, h_c, validation_context_cbor, zero,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  canonical_step_fixture, empty_v1_transaction, exact_rejection_post,
  forced_pending_state, inline_field_carriage, inline_field_door,
  non_empty_claimed_delta_root, one_step_evidence,
  phase_a_script_preconditions_fixture, submitted_commitment, submitted_source,
}
use midgard/validation_trace_v1

test native_v1_optional_posix_time_sentinel_and_adjacent_invalid_are_exact() {
  and {
    !vm_shared.validity_interval_is_malformed(-1, -1),
    !vm_shared.validity_interval_is_malformed(0, 0),
    !vm_shared.validity_interval_is_malformed(0, 1),
    vm_shared.validity_interval_is_malformed(-2, -1),
    vm_shared.validity_interval_is_malformed(-1, -2),
    vm_shared.validity_interval_is_malformed(1, 0),
  }
}

test purpose_kind_to_redeemer_pointer_mapping_is_exact() {
  and {
    vm_shared.redeemer_tag_for_purpose_kind_v1(0) == Some(0),
    vm_shared.redeemer_tag_for_purpose_kind_v1(1) == Some(1),
    vm_shared.redeemer_tag_for_purpose_kind_v1(2) == Some(3),
    vm_shared.redeemer_tag_for_purpose_kind_v1(3) == Some(6),
    vm_shared.redeemer_tag_for_purpose_kind_v1(-1) == None,
    vm_shared.redeemer_tag_for_purpose_kind_v1(4) == None,
    vm_shared.redeemer_pointer_matches_purpose_v1(0, 7, 0, 7),
    vm_shared.redeemer_pointer_matches_purpose_v1(1, 7, 1, 7),
    vm_shared.redeemer_pointer_matches_purpose_v1(2, 7, 3, 7),
    vm_shared.redeemer_pointer_matches_purpose_v1(3, 7, 6, 7),
    !vm_shared.redeemer_pointer_matches_purpose_v1(0, 7, 0, 8),
    !vm_shared.redeemer_pointer_matches_purpose_v1(1, 7, 1, 8),
    !vm_shared.redeemer_pointer_matches_purpose_v1(2, 7, 3, 8),
    !vm_shared.redeemer_pointer_matches_purpose_v1(3, 7, 6, 8),
    !vm_shared.redeemer_pointer_matches_purpose_v1(-1, 7, 0, 7),
    !vm_shared.redeemer_pointer_matches_purpose_v1(4, 7, 6, 7),
    !vm_shared.redeemer_pointer_matches_purpose_v1(0, 7, -1, 7),
    !vm_shared.redeemer_pointer_matches_purpose_v1(0, 7, 2, 7),
    !vm_shared.redeemer_pointer_matches_purpose_v1(0, 7, 4, 7),
    !vm_shared.redeemer_pointer_matches_purpose_v1(0, 7, 5, 7),
    !vm_shared.redeemer_pointer_matches_purpose_v1(0, 7, 7, 7),
  }
}

test phase_a_script_preconditions_control_cbor_is_canonical() {
  let tx = empty_v1_transaction()
  let Pair(_pre, witness_cbor) = phase_a_script_preconditions_fixture(tx, h_a)
  when cbor.deserialise(witness_cbor) is {
    None -> False
    Some(data) -> list.length(builtin.un_list_data(data)) == 11
  }
}

test observer_purpose_scan_control_cbor_is_canonical() {
  let encoded = vm_witness.encode_observer_purpose_scan_control(
    vm_witness.empty_observer_purpose_scan_control(),
  )
  and {
    encoded == #"83004000",
    when cbor.deserialise(encoded) is {
      None -> False
      Some(data) -> list.length(builtin.un_list_data(data)) == 3
    },
  }
}

test receive_purpose_scan_control_cbor_is_canonical() {
  let encoded = vm_witness.encode_receive_purpose_scan_control(
    vm_witness.empty_receive_purpose_scan_control(),
  )
  encoded == #"86008000404080"
}

test canonical_v1_decode_is_independently_verified_on_l1() {
  let Pair(pre, evidence) = canonical_step_fixture()
  vm_one_step.verify_canonical_decode_one_step_v1(
    pre,
    evidence,
    inline_field_door(),
  )
}

test canonical_decode_authenticates_one_bounded_script_item() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"010203",
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = submitted_source(tx)
  let item_cbor = encode_midgard_versioned_script(script)
  let work_witness_cbor = vm_witness.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    6,
    0,
    0,
    -1,
    0,
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.CanonicalDecode,
    7,
    work_witness_cbor,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      8,
      vm_witness.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        7,
        0,
        0,
        -1,
        0,
      ),
    ),
  }
  vm_one_step.verify_canonical_decode_one_step_v1(
    pre,
    vm_machine_types.ValidationOneStepEvidenceV1 {
      transition: vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor,
        claimed_successor: post,
      },
      // §8.1 tier 1: the field-6 preimage travels in the evidence and the door
      // hashes it once against `script_tx_wits_hash`. The retired form asserted
      // a per-item collection opening against that same flat hash, which is the
      // unsatisfiable predicate #592 is about.
      auxiliary: vm_machine_types.TransactionFieldChunkWitness {
        field_index: 6,
        item_index: 0,
        carriage: inline_field_carriage([item_cbor]),
      },
    },
    inline_field_door(),
  )
}

test canonical_decode_rejects_a_legacy_field_seven_script_proof() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"010203",
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = submitted_source(tx)
  let item_cbor = encode_midgard_versioned_script(script)
  let work_witness_cbor = vm_witness.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    6,
    0,
    0,
    -1,
    0,
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.CanonicalDecode,
    7,
    work_witness_cbor,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      8,
      vm_witness.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        7,
        0,
        0,
        -1,
        0,
      ),
    ),
  }
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  let honest_carriage = inline_field_carriage([item_cbor])
  // C29, rewritten for #592. The substitution is now refused by the machine's
  // own field-index check rather than by domain-separated hashing. §4 removed
  // field-index domain separation — two fields holding the same items commit to
  // the same 32 bytes and a flat hash says nothing about which slot it came
  // from — so §8's door authenticates whichever slot the step's *control* names
  // and it is `field_index == control.field_index` inside
  // `verify_canonical_decode_chunk` that refuses evidence pointing at the
  // retired field-7 slot. Every conjunct below is pinned to its own measured
  // disposition, as the earlier `test ... fail` form could not be.
  and {
    // The substitution is a real one and not a collision: this transaction's
    // field-6 preimage does not commit to the same 32 bytes as its (empty)
    // field-7 slot.
    native_tx_field_access_v1.field_commitment(
      native_tx_field_access_v1.encode_field_preimage([item_cbor]),
    ) != native_tx_field_access_v1.empty_field_commitment,
    // Field-7-flavoured evidence does not authenticate a field-6 item on the
    // semantic path ...
    !vm_canonical_decode.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      7,
      0,
      honest_carriage,
    ),
    // ... nor on the deployed one-step path.
    !vm_one_step.verify_canonical_decode_one_step_v1(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition,
        auxiliary: vm_machine_types.TransactionFieldChunkWitness {
          field_index: 7,
          item_index: 0,
          carriage: honest_carriage,
        },
      },
      inline_field_door(),
    ),
    // And an item index the authenticated field does not have is refused for
    // the same reason: the index is checked against the control's cursor, never
    // taken from the evidence.
    !vm_canonical_decode.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      6,
      1,
      honest_carriage,
    ),
  }
}

// V-9 (#532). This was `test ... fail` over a single call to
// `verify_canonical_decode_one_step_v1`, so it went green as soon as any guard
// anywhere under that call was false and the name could not establish that the
// claimed successor is what fired. Single-conjunct mutation controls (each
// conjunct neutralized to `True` in turn, aiken-fork v1.1.23, this exact
// selector):
//
//   verify_canonical_decode_one_step_v1
//     1  pre.phase == CanonicalDecode                       -> pass
//     2  structural_transition_is_valid(pre, transition)    -> pass
//     3  verify_canonical_decode(pre, transition, aux)      -> FAIL
//
//   verify_canonical_decode_chunk (reached through conjunct 3)
//     1..8   control binding, non-empty field commitment, first/continuing
//            chunk, positive authenticated item count, the evidence's
//            field/item index equalities, the chunk-index bound  -> all pass
//     9      the claimed-successor exactness branch                -> FAIL
//
// (Conjunct numbering re-measured after #592: the retired `verify_item` /
// `verify_chunk` openings and the four proof length/commitment equalities they
// needed are gone, so the arm is shorter. The disposition is unchanged.)
//
// So the structural transition IS valid here — the vector differs from the
// passing sibling `canonical_decode_authenticates_one_bounded_script_item` in
// nothing but the claimed successor, and conjunct 9 of the chunk arm (the
// claimed-successor exactness branch, as the control above measures) is the sole
// falsifier. The dispositions are now pinned individually below, with that
// sibling's correct successor as the differential.
test canonical_decode_rejects_a_wrong_claimed_successor() {
  let base = empty_v1_transaction()
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: #"010203",
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = submitted_source(tx)
  let item_cbor = encode_midgard_versioned_script(script)
  let work_witness_cbor = vm_witness.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    6,
    0,
    0,
    -1,
    0,
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.CanonicalDecode,
    7,
    work_witness_cbor,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
  let rejection_code = #"455f4649454c445f505245494d4147455f53495a45"
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
  // The exact successor the sibling positive test claims for this same pre,
  // work witness, and auxiliary: the field-6 scan advancing to field 7.
  let correct_successor = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 8,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      8,
      vm_witness.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        validation_context_cbor,
        7,
        0,
        0,
        -1,
        0,
      ),
    ),
  }
  let carriage = inline_field_carriage([item_cbor])
  let auxiliary = vm_machine_types.TransactionFieldChunkWitness {
    field_index: 6,
    item_index: 0,
    carriage,
  }
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: terminal,
  }
  let correct_transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: correct_successor,
  }
  and {
    // Conjunct 1 of the one-step guard holds.
    pre.phase == validation_trace_v1.CanonicalDecode,
    // Conjunct 2 holds: the rejected successor is a structurally valid
    // successor state, so nothing about well-formedness, the work-root
    // binding, the program counter, the immutable context, the phase
    // succession, or the budget is what rejects it.
    vm_shared.structural_transition_is_valid(pre, transition),
    // Conjunct 3 is the sole falsifier, and it falsifies through the chunk arm.
    !vm_one_step.verify_canonical_decode_semantics_v1(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 { transition, auxiliary },
      inline_field_door(),
    ),
    !vm_canonical_decode.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      6,
      0,
      carriage,
    ),
    !vm_one_step.verify_canonical_decode_one_step_v1(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 { transition, auxiliary },
      inline_field_door(),
    ),
    // Differential: the identical pre, work witness and §8 carriage authenticate
    // once the claimed successor is the exact one, so the claimed successor is
    // the only thing this vector rejects.
    vm_one_step.verify_canonical_decode_one_step_v1(
      pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: correct_transition,
        auxiliary,
      },
      inline_field_door(),
    ),
  }
}

test compact_binding_is_independently_verified_on_l1() {
  let tx = empty_v1_transaction()
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let transaction_commitment = submitted_commitment(tx)
  let proof_source = submitted_source(tx)
  let context_cbor = validation_context_cbor
  let compact_witness_cbor = vm_witness.encode_compact_binding_witness(
    tx_id,
    transaction_commitment,
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    context_cbor,
  )
  let compact_pre = validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id: tx_id,
    transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase: validation_trace_v1.CompactBinding,
    program_counter: 9,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CompactBinding,
      9,
      compact_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: h_a,
  }
  let input_post = validation_trace_v1.ValidationMachineStateV1 {
    ..compact_pre,
    phase: validation_trace_v1.StaticLedgerRules,
    program_counter: compact_pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.StaticLedgerRules,
      compact_pre.program_counter + 1,
      vm_witness.encode_static_rules_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        context_cbor,
      ),
    ),
  }
  vm_one_step.verify_one_step(
    compact_pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor: compact_witness_cbor,
      claimed_successor: input_post,
    },
    inline_field_door(),
  )
}

test static_rules_accept_the_exact_proof_context_and_advance_to_input_sets() {
  let tx = empty_v1_transaction()
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
    validation_context_cbor,
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.StaticLedgerRules,
    2,
    work_witness_cbor,
    h_a,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
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
        validation_context_cbor,
        0,
        0,
        0,
        0,
        #"",
        vm_witness.empty_resolution_schedule_hash(),
      ),
    ),
  }
  vm_one_step.verify_static_ledger_rules_semantics_v1(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  )
}

test static_rules_prove_a_network_mismatch_is_an_exact_no_op() {
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, network_id: 1 },
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
    validation_context_cbor,
  )
  let rejection_code = #"455f4e4554574f524b5f49445f4d49534d41544348"
  let pre = forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.StaticLedgerRules,
    2,
    work_witness_cbor,
    // Deliberately a realistic NON-EMPTY claimed delta root. This test was RED
    // on a production defect: `rejected_successor_is_exact` demanded
    // `post.ledger_delta_root == frontier_commitment(0, [])` while
    // `immutable_context_matches` (reached via `structural_transition_is_valid`,
    // which the `verify_one_step` entry point applies) demands
    // `pre.ledger_delta_root == post.ledger_delta_root`. Those were jointly
    // unsatisfiable for any `pre` whose delta root was not already the empty
    // commitment -- i.e. for every real transaction. The clearing clause has
    // been deleted from production. Do NOT make any rejection test green by
    // setting `pre.ledger_delta_root` to `frontier_commitment(0, [])`: that is
    // the workaround that hid the defect, and it exercises the one pre-state in
    // which the defect is invisible.
    non_empty_claimed_delta_root(),
  )
  // An exact rejection carries the claimed ledger delta forward unchanged;
  // `exact_rejection_post` is the canonical successor shape.
  let terminal = exact_rejection_post(pre, rejection_code)
  vm_one_step.verify_one_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: terminal,
    },
    inline_field_door(),
  )
}
