use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use cardano/transaction.{OutputReference}
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/unused_script_witness/rule
use midgard/ledger_state
use midgard/rejection_reason_v1.{OutputNonCanonical, UnusedScriptWitness}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h28a = #"11111111111111111111111111111111111111111111111111111111"

const h28b = #"22222222222222222222222222222222222222222222222222222222"

const h32 = #"3333333333333333333333333333333333333333333333333333333333333333"

fn subject(direction: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction,
    source_kind: if direction == substrate.direction_wrongful_acceptance {
      0
    } else {
      1
    },
    transaction_id: tx_id,
    source_key: if direction == substrate.direction_wrongful_acceptance {
      #""
    } else {
      #"01"
    },
    rejection_reason: if direction == substrate.direction_wrongful_acceptance {
      None
    } else {
      Some(UnusedScriptWitness { script_index: 1 })
    },
  }
}

fn witness(
  direction: Int,
  source_hashes: List<ByteArray>,
  purpose_hashes: List<ByteArray>,
) -> rule.AuthenticatedWitnessV1 {
  let sources = validation_merkle_v1.build_frontier(source_hashes)
  let purposes = validation_merkle_v1.build_frontier(purpose_hashes)
  rule.AuthenticatedWitnessV1 {
    bound: rule.bind_witness_v1(subject(direction), h32, 1, 1),
    prior_ledger_root: h32,
    language_tag: 3,
    script_hash: h28a,
    script_total_length: 10,
    item_commitment: h32,
    source_count: sources.count,
    source_peaks: sources.peaks,
    purpose_count: purposes.count,
    purpose_peaks: purposes.peaks,
  }
}

fn purpose_leaf(kind: Int, index: Int, hash: ByteArray) -> ByteArray {
  script_proof_v1.purpose_leaf_hash(kind, index, hash, #"aa")
}

fn scan_without_alternates(
  direction: Int,
  hashes: List<ByteArray>,
) -> rule.ReverseScanV1 {
  let leaves =
    hashes
      |> list.indexed_map(fn(index, hash) { purpose_leaf(index % 4, 0, hash) })
  let frontier = validation_merkle_v1.build_frontier(leaves)
  let initial =
    rule.initial_reverse_scan_v1(witness(direction, [h32, h32], leaves))
  let source_leaf = script_proof_v1.inline_source_leaf_hash(0, 3, h28b, 10, h32)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf, h32])
  let with_sources =
    rule.AuthenticatedWitnessV1 {
      ..initial.witness,
      source_count: 2,
      source_peaks: source_frontier.peaks,
      purpose_count: frontier.count,
      purpose_peaks: frontier.peaks,
    }
  let after_source =
    rule.authenticate_earlier_source_v1(
      rule.ReverseScanV1 { ..initial, witness: with_sources },
      rule.SourceOpeningV1 {
        source_index: 0,
        language_tag: 3,
        script_hash: h28b,
        total_length: 10,
        item_commitment: h32,
        siblings: [h32],
      },
    )
  scan_hashes(after_source, hashes, leaves, 0)
}

fn scan_hashes(
  state: rule.ReverseScanV1,
  hashes: List<ByteArray>,
  leaves: List<ByteArray>,
  index: Int,
) -> rule.ReverseScanV1 {
  when hashes is {
    [] -> state
    [hash, ..rest] ->
      if state.used {
        state
      } else {
        scan_hashes(
          rule.scan_purpose_v1(
            state,
            rule.PurposeOpeningV1 {
              frontier_index: index,
              purpose_kind: index % 4,
              purpose_index: 0,
              script_hash: hash,
              purpose_subject: #"aa",
              siblings: proof(leaves, index),
            },
          ),
          rest,
          leaves,
          index + 1,
        )
      }
  }
}

fn proof(leaves: List<ByteArray>, index: Int) -> List<ByteArray> {
  when leaves is {
    [_] -> []
    [first, second] ->
      if index == 0 {
        [second]
      } else {
        [first]
      }
    [first, second, _third] ->
      if index == 0 {
        [second]
      } else if index == 1 {
        [first]
      } else {
        []
      }
    [first, second, third, fourth] ->
      when index is {
        0 -> [second, validation_merkle_v1.hash_branch(third, fourth)]
        1 -> [first, validation_merkle_v1.hash_branch(third, fourth)]
        2 -> [fourth, validation_merkle_v1.hash_branch(first, second)]
        3 -> [third, validation_merkle_v1.hash_branch(first, second)]
        _ -> fail
      }
    _ -> fail @"test proof helper supports one to four leaves"
  }
}

fn authenticate_exact_witness(
  supplied_script_hash: ByteArray,
  substitute_bound_root: Bool,
) -> rule.AuthenticatedWitnessV1 {
  let source_leaf = script_proof_v1.inline_source_leaf_hash(0, 3, h28a, 1, h32)
  let purpose_leaf = purpose_leaf(0, 0, h28b)
  let sources = validation_merkle_v1.build_frontier([source_leaf])
  let purposes = validation_merkle_v1.build_frontier([purpose_leaf])
  let executions = validation_merkle_v1.build_frontier([h32])
  let discovery =
    vm_machine_types.ScriptDiscoveryControlV1 {
      purpose_cursor: 1,
      source_cursor: 0,
      redeemer_cursor: 0,
      current_purpose_kind: -1,
      current_purpose_index: -1,
      current_script_hash: #"",
      current_subject: #"",
      matched_source_index: -1,
      matched_language_tag: -1,
      matched_source_leaf: #"",
      used_inline_bitmap: 0,
      used_redeemer_bitmap: 0,
      redeemer_item_control_hash: #"",
      execution_count: 1,
      execution_peaks: executions.peaks,
    }
  let control =
    vm_machine_types.ScriptSourcesControlV1 {
      compact_cbor: #"",
      witness_set_compact_cbor: #"",
      field_preimage_lengths_cbor: #"",
      context_cbor: #"",
      resolved_input_count: 0,
      resolved_inputs_accumulator: h32,
      resolved_item_peaks: [],
      signer_count: 0,
      signer_frontier_commitment: h32,
      stage: 11,
      source_count: 1,
      source_peaks: sources.peaks,
      redeemer_count: 0,
      redeemer_peaks: [],
      replay_cursor: 0,
      replay_accumulator: h32,
      replay_remaining_schedule_hash: h32,
      spend_index: 0,
      purpose_count: 1,
      purpose_peaks: purposes.peaks,
      output_cursor: 0,
      output_count: 0,
      output_peaks: [],
      output_total_count: 0,
      receive_scan: vm_witness.empty_receive_purpose_scan_control(),
      source_total_count: 1,
      redeemer_total_count: 0,
      observer_scan: vm_witness.empty_observer_purpose_scan_control(),
      discovery,
      output_proof: None,
      pending_source_cbor: #"",
      mint_fold: vm_witness.empty_mint_fold_control(),
      resolution_schedule_hash: h32,
    }
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let machine_state =
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: blake2b_256(cbor.serialise(event_key)),
      transaction_id: tx_id,
      transaction_commitment: native_tx_proof_commitment_v1(#"", #"", #""),
      validation_context_hash: validation_trace_v1.hash_validation_context(#""),
      source_kind: validation_trace_v1.Normal,
      prior_ledger_root: h32,
      phase: validation_trace_v1.ScriptSources,
      program_counter: 7,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        7,
        vm_witness.encode_script_sources_discovery_witness(
          control,
          11,
          control.discovery,
        ),
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
      ledger_delta_root: h32,
    }
  let state_hash = validation_trace_v1.hash_machine_state(machine_state)
  let descriptor =
    validation_trace_v1.ValidationTraceDescriptorV1 {
      schema_version: validation_trace_v1.descriptor_version,
      machine_version: validation_trace_v1.machine_version,
      trace_root: validation_trace_v1.trace_leaf_hash(state_hash),
      step_count: 0,
      initial_state_hash: state_hash,
      terminal_state_hash: state_hash,
      verdict: validation_trace_v1.Accepted,
      rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    }
  let phas_root =
    mpf.root(
      mpf.insert(
        mpf.empty,
        cbor.serialise(event_key),
        cbor.serialise(descriptor),
        [],
      ),
    )
  let membership =
    transition_trace.RootMembershipProof {
      domain: transition_trace.ValidationTracesRootDomain,
      root: transition_trace.commit_counted_root(
        transition_trace.ValidationTracesRootDomain,
        phas_root,
        1,
      ),
      phas_root,
      count: 1,
      key: event_key,
      value: descriptor,
      proof: [],
    }
  rule.authenticate_witness_v1(
    rule.bind_witness_v1(
      subject(substrate.direction_wrongful_acceptance),
      if substitute_bound_root {
        h32
      } else {
        membership.root
      },
      1,
      0,
    ),
    membership,
    machine_state,
    validation_trace_v1.ValidationTraceProof {
      state_index: 0,
      state_hash,
      siblings: [],
    },
    rule.TerminalScriptSourcesWitnessV1 {
      witness_cbor: vm_witness.encode_script_sources_discovery_witness(
        control,
        11,
        control.discovery,
      ),
    },
    3,
    supplied_script_hash,
    1,
    h32,
    [],
  )
}

test binds_exact_unused_reason_coordinate() {
  rule.bind_witness_v1(subject(1), h32, 1, 1).script_index == 1
}

test authenticates_full_header_trace_control_and_source_membership() {
  let exact = authenticate_exact_witness(h28a, False)
  and {
    exact.bound.script_index == 0,
    exact.script_hash == h28a,
    exact.source_count == 1,
    exact.purpose_count == 1,
  }
}

test refuses_substituted_committed_source_leaf() fail {
  authenticate_exact_witness(h28b, False).script_hash == h28b
}

test refuses_substituted_header_trace_root() fail {
  authenticate_exact_witness(h28a, True).script_hash == h28a
}

test refuses_substituted_unused_reason_coordinate() fail {
  expect _bound = rule.bind_witness_v1(subject(1), h32, 1, 0)
  True
}

test refuses_another_reason_constructor() fail {
  expect _bound =
    rule.bind_witness_v1(
      substrate.VerdictSubjectV1 {
        ..subject(1),
        rejection_reason: Some(OutputNonCanonical { output_index: 1 }),
      },
      h32,
      1,
      1,
    )
  True
}

test all_four_purpose_kinds_complete_as_unused() {
  let state = scan_without_alternates(0, [h28b, h28b, h28b, h28b])
  and {
    state.purpose_cursor == 4,
    rule.decision_v1(state).unused,
  }
}

test middle_reverse_match_marks_witness_used() {
  let state = scan_without_alternates(1, [h28b, h28a, h28b])
  and {
    state.used,
    !rule.decision_v1(state).unused,
  }
}

test partial_reverse_scan_cannot_finalize() fail {
  let partial = scan_without_alternates(0, [h28b])
  expect _decision =
    rule.decision_v1(
      rule.ReverseScanV1 {
        ..partial,
        witness: rule.AuthenticatedWitnessV1 {
          ..partial.witness,
          purpose_count: 2,
        },
      },
    )
  True
}

test wrongful_acceptance_unused_convicts() {
  rule.terminal_contradiction_v1(
    rule.decision_v1(scan_without_alternates(0, [h28b])),
  )
}

test wrongful_rejection_used_convicts() {
  rule.terminal_contradiction_v1(
    rule.decision_v1(scan_without_alternates(1, [h28a])),
  )
}

test honest_acceptance_used_refuses() fail {
  rule.terminal_contradiction_v1(
    rule.decision_v1(scan_without_alternates(0, [h28a])),
  )
}

test honest_rejection_unused_refuses() fail {
  rule.terminal_contradiction_v1(
    rule.decision_v1(scan_without_alternates(1, [h28b])),
  )
}

// Schema-derived bytes distinguish cursor and boolean field order.
test reverse_checkpoint_encoding_golden_vector() {
  rule.encode_reverse_scan_v1(
    rule.ReverseScanV1 {
      witness: golden_authenticated(0),
      alternate_cursor: 1,
      purpose_cursor: 0,
      shadowed: True,
      used: False,
      checkpoint_hash: h32,
    },
  ) == #"d8799fd8799fd8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff582033333333333333333333333333333333333333333333333333333333333333330101ff5820333333333333333333333333333333333333333333333333333333333333333303581c111111111111111111111111111111111111111111111111111111110a58203333333333333333333333333333333333333333333333333333333333333333029fd8799f0158203333333333333333333333333333333333333333333333333333333333333333ffff019fd8799f0058203333333333333333333333333333333333333333333333333333333333333333ffffff0100d87a80d8798058203333333333333333333333333333333333333333333333333333333333333333ff"
}

test alternate_source_membership_substitution_refuses() fail {
  let state = rule.initial_reverse_scan_v1(witness(0, [h32, h32], []))
  expect _next =
    rule.authenticate_earlier_source_v1(
      state,
      rule.SourceOpeningV1 {
        source_index: 0,
        language_tag: 3,
        script_hash: h28b,
        total_length: 10,
        item_commitment: h32,
        siblings: [],
      },
    )
  True
}

// ---------------------------------------------------------------------------
// Direction, descriptor, control and coordinate seams of step 02
// ---------------------------------------------------------------------------

const h28c = #"44444444444444444444444444444444444444444444444444444444"

const zero32 =
  #"0000000000000000000000000000000000000000000000000000000000000000"

fn order_key() -> OutputReference {
  OutputReference { transaction_id: h32, output_index: 0 }
}

/// A forced subject whose source key is the serialised order key, so the
/// forced event key of the retained descriptor matches it.
fn forced_subject(script_index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: cbor.serialise(order_key()),
    rejection_reason: Some(UnusedScriptWitness { script_index }),
  }
}

fn inline_leaf(index: Int) -> ByteArray {
  script_proof_v1.inline_source_leaf_hash(index, 3, h28a, 1, h32)
}

/// One step-02 authentication shape; every seam test mutates one field.
type Seam {
  direction: Int,
  stage: Int,
  work_stage: Int,
  source_count: Int,
  script_index: Int,
  supplied_hash: ByteArray,
  verdict: validation_trace_v1.ValidationVerdict,
  code_hash: ByteArray,
  event_key: ledger_state.EventKey,
}

fn accepted_seam() -> Seam {
  Seam {
    direction: substrate.direction_wrongful_acceptance,
    stage: 11,
    work_stage: 11,
    source_count: 2,
    script_index: 0,
    supplied_hash: h28a,
    verdict: validation_trace_v1.Accepted,
    code_hash: zero32,
    event_key: ledger_state.L2TransactionEventKey { tx_id },
  }
}

fn forced_seam() -> Seam {
  Seam {
    direction: substrate.direction_wrongful_rejection,
    stage: 12,
    work_stage: 12,
    source_count: 2,
    script_index: 1,
    supplied_hash: h28a,
    verdict: validation_trace_v1.Rejected,
    code_hash: validation_trace_v1.hash_rejection_code(
      rejection_reason_v1.reject_invalid_field_type,
    ),
    event_key: ledger_state.ForcedTransactionEventKey {
      tx_order_id: order_key(),
    },
  }
}

fn authenticate_seam(seam: Seam) -> rule.AuthenticatedWitnessV1 {
  let source_leaves =
    if seam.source_count == 1 {
      [inline_leaf(0)]
    } else {
      [inline_leaf(0), inline_leaf(1)]
    }
  let sources = validation_merkle_v1.build_frontier(source_leaves)
  let purposes = validation_merkle_v1.build_frontier([purpose_leaf(0, 0, h28b)])
  let executions = validation_merkle_v1.build_frontier([h32])
  let discovery =
    vm_machine_types.ScriptDiscoveryControlV1 {
      purpose_cursor: 1,
      source_cursor: if seam.stage == 11 {
        seam.script_index
      } else {
        seam.source_count
      },
      redeemer_cursor: 0,
      current_purpose_kind: -1,
      current_purpose_index: -1,
      current_script_hash: #"",
      current_subject: #"",
      matched_source_index: -1,
      matched_language_tag: -1,
      matched_source_leaf: #"",
      used_inline_bitmap: 0,
      used_redeemer_bitmap: 0,
      redeemer_item_control_hash: #"",
      execution_count: 1,
      execution_peaks: executions.peaks,
    }
  let control =
    vm_machine_types.ScriptSourcesControlV1 {
      compact_cbor: #"",
      witness_set_compact_cbor: #"",
      field_preimage_lengths_cbor: #"",
      context_cbor: #"",
      resolved_input_count: 0,
      resolved_inputs_accumulator: h32,
      resolved_item_peaks: [],
      signer_count: 0,
      signer_frontier_commitment: h32,
      stage: seam.stage,
      source_count: seam.source_count,
      source_peaks: sources.peaks,
      redeemer_count: 0,
      redeemer_peaks: [],
      replay_cursor: 0,
      replay_accumulator: h32,
      replay_remaining_schedule_hash: h32,
      spend_index: 0,
      purpose_count: 1,
      purpose_peaks: purposes.peaks,
      output_cursor: 0,
      output_count: 0,
      output_peaks: [],
      output_total_count: 0,
      receive_scan: vm_witness.empty_receive_purpose_scan_control(),
      source_total_count: seam.source_count,
      redeemer_total_count: 0,
      observer_scan: vm_witness.empty_observer_purpose_scan_control(),
      discovery,
      output_proof: None,
      pending_source_cbor: #"",
      mint_fold: vm_witness.empty_mint_fold_control(),
      resolution_schedule_hash: h32,
    }
  let supplied =
    vm_witness.encode_script_sources_discovery_witness(
      control,
      seam.stage,
      control.discovery,
    )
  let committed =
    vm_witness.encode_script_sources_discovery_witness(
      control,
      seam.work_stage,
      control.discovery,
    )
  let subject =
    if seam.direction == substrate.direction_wrongful_acceptance {
      subject(substrate.direction_wrongful_acceptance)
    } else {
      forced_subject(seam.script_index)
    }
  let machine_state =
    validation_trace_v1.ValidationMachineStateV1 {
      machine_version: validation_trace_v1.machine_version,
      event_key_hash: blake2b_256(cbor.serialise(seam.event_key)),
      transaction_id: tx_id,
      transaction_commitment: native_tx_proof_commitment_v1(#"", #"", #""),
      validation_context_hash: validation_trace_v1.hash_validation_context(#""),
      source_kind: if seam.direction == substrate.direction_wrongful_acceptance {
        validation_trace_v1.Normal
      } else {
        validation_trace_v1.Forced
      },
      prior_ledger_root: h32,
      phase: validation_trace_v1.ScriptSources,
      program_counter: 7,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        7,
        committed,
      ),
      execution_cpu: 0,
      execution_memory: 0,
      verdict: validation_trace_v1.Pending,
      rejection_code_hash: zero32,
      ledger_delta_root: h32,
    }
  let state_hash = validation_trace_v1.hash_machine_state(machine_state)
  let descriptor =
    validation_trace_v1.ValidationTraceDescriptorV1 {
      schema_version: validation_trace_v1.descriptor_version,
      machine_version: validation_trace_v1.machine_version,
      trace_root: validation_trace_v1.trace_leaf_hash(state_hash),
      step_count: 0,
      initial_state_hash: state_hash,
      terminal_state_hash: state_hash,
      verdict: seam.verdict,
      rejection_code_hash: seam.code_hash,
    }
  let phas_root =
    mpf.root(
      mpf.insert(
        mpf.empty,
        cbor.serialise(seam.event_key),
        cbor.serialise(descriptor),
        [],
      ),
    )
  let membership =
    transition_trace.RootMembershipProof {
      domain: transition_trace.ValidationTracesRootDomain,
      root: transition_trace.commit_counted_root(
        transition_trace.ValidationTracesRootDomain,
        phas_root,
        1,
      ),
      phas_root,
      count: 1,
      key: seam.event_key,
      value: descriptor,
      proof: [],
    }
  rule.authenticate_witness_v1(
    rule.bind_witness_v1(subject, membership.root, 1, seam.script_index),
    membership,
    machine_state,
    validation_trace_v1.ValidationTraceProof {
      state_index: 0,
      state_hash,
      siblings: [],
    },
    rule.TerminalScriptSourcesWitnessV1 { witness_cbor: supplied },
    3,
    seam.supplied_hash,
    1,
    h32,
    proof(source_leaves, seam.script_index),
  )
}

test authenticates_accepted_stage_eleven_audit_seam() {
  let exact = authenticate_seam(accepted_seam())
  and {
    exact.bound.script_index == 0,
    exact.source_count == 2,
    exact.purpose_count == 1,
  }
}

test authenticates_forced_stage_twelve_terminal() {
  let exact = authenticate_seam(forced_seam())
  and {
    exact.bound.subject.direction == substrate.direction_wrongful_rejection,
    exact.bound.script_index == 1,
    exact.source_count == 2,
  }
}

test refuses_accepted_direction_at_stage_twelve() fail {
  let exact =
    authenticate_seam(Seam { ..accepted_seam(), stage: 12, work_stage: 12 })
  exact.source_count == 2
}

test refuses_forced_direction_at_stage_eleven() fail {
  let exact =
    authenticate_seam(Seam { ..forced_seam(), stage: 11, work_stage: 11 })
  exact.source_count == 2
}

test refuses_out_of_range_coordinate() fail {
  let exact = authenticate_seam(Seam { ..accepted_seam(), script_index: 2 })
  exact.bound.script_index == 2
}

test refuses_forced_out_of_range_coordinate() fail {
  let exact = authenticate_seam(Seam { ..forced_seam(), script_index: 2 })
  exact.bound.script_index == 2
}

test refuses_accepted_descriptor_with_rejected_verdict() fail {
  let exact =
    authenticate_seam(
      Seam { ..accepted_seam(), verdict: validation_trace_v1.Rejected },
    )
  exact.source_count == 2
}

test refuses_forced_descriptor_with_accepted_verdict() fail {
  let exact =
    authenticate_seam(
      Seam { ..forced_seam(), verdict: validation_trace_v1.Accepted },
    )
  exact.source_count == 2
}

test refuses_forced_rejection_code_substitution() fail {
  let exact = authenticate_seam(Seam { ..forced_seam(), code_hash: zero32 })
  exact.source_count == 2
}

test refuses_substituted_event_key_kind() fail {
  let exact =
    authenticate_seam(
      Seam {
        ..forced_seam(),
        event_key: ledger_state.L2TransactionEventKey { tx_id },
      },
    )
  exact.source_count == 2
}

test refuses_substituted_retained_control() fail {
  let exact = authenticate_seam(Seam { ..accepted_seam(), work_stage: 12 })
  exact.source_count == 2
}

test refuses_substituted_source_hash_against_committed_frontier() fail {
  let exact = authenticate_seam(Seam { ..accepted_seam(), supplied_hash: h28b })
  exact.script_hash == h28b
}

// ---------------------------------------------------------------------------
// Alternate-source walk and reverse-match ordering
// ---------------------------------------------------------------------------

/// The scan state after the single earlier source `alternate_hash` and the
/// purpose hashes, for the accused witness `h28a` at coordinate 1.
fn scan_with_alternate(
  direction: Int,
  alternate_hash: ByteArray,
  hashes: List<ByteArray>,
) -> rule.ReverseScanV1 {
  let leaves =
    hashes
      |> list.indexed_map(fn(index, hash) { purpose_leaf(index % 4, 0, hash) })
  let frontier = validation_merkle_v1.build_frontier(leaves)
  let source_leaf =
    script_proof_v1.inline_source_leaf_hash(0, 3, alternate_hash, 10, h32)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf, h32])
  let initial =
    rule.initial_reverse_scan_v1(witness(direction, [source_leaf, h32], leaves))
  let with_sources =
    rule.AuthenticatedWitnessV1 {
      ..initial.witness,
      source_count: 2,
      source_peaks: source_frontier.peaks,
      purpose_count: frontier.count,
      purpose_peaks: frontier.peaks,
    }
  let after_source =
    rule.authenticate_earlier_source_v1(
      rule.ReverseScanV1 { ..initial, witness: with_sources },
      rule.SourceOpeningV1 {
        source_index: 0,
        language_tag: 3,
        script_hash: alternate_hash,
        total_length: 10,
        item_commitment: h32,
        siblings: [h32],
      },
    )
  scan_hashes(after_source, hashes, leaves, 0)
}

test alternate_walk_cannot_overrun_the_coordinate() fail {
  let complete = scan_with_alternate(0, h28b, [])
  expect _next =
    rule.authenticate_earlier_source_v1(
      complete,
      rule.SourceOpeningV1 {
        source_index: 1,
        language_tag: 3,
        script_hash: h28b,
        total_length: 10,
        item_commitment: h32,
        siblings: [h32],
      },
    )
  True
}

test purpose_scan_before_alternates_complete_refuses() fail {
  let leaves = [purpose_leaf(0, 0, h28b)]
  let initial = rule.initial_reverse_scan_v1(witness(0, [h32, h32], leaves))
  expect _next =
    rule.scan_purpose_v1(
      initial,
      rule.PurposeOpeningV1 {
        frontier_index: 0,
        purpose_kind: 0,
        purpose_index: 0,
        script_hash: h28b,
        purpose_subject: #"aa",
        siblings: [],
      },
    )
  True
}

test decision_requires_complete_alternate_walk() fail {
  let initial = rule.initial_reverse_scan_v1(witness(0, [h32, h32], []))
  expect _decision = rule.decision_v1(initial)
  True
}

/// An earlier inline source with the accused hash shadows the accused
/// coordinate: first-source precedence makes the later duplicate unused even
/// though a purpose selects that hash.
test shadowed_duplicate_witness_is_unused() {
  let state = scan_with_alternate(0, h28a, [h28a])
  and {
    state.shadowed,
    !state.used,
    rule.decision_v1(state).unused,
    rule.terminal_contradiction_v1(rule.decision_v1(state)),
  }
}

test checkpoint_binds_scanned_purpose() {
  scan_with_alternate(0, h28b, [h28b]).checkpoint_hash != scan_with_alternate(
    0,
    h28b,
    [h28c],
  ).checkpoint_hash
}

test checkpoint_binds_alternate_source() {
  scan_with_alternate(0, h28b, []).checkpoint_hash != scan_with_alternate(
    0,
    h28c,
    [],
  ).checkpoint_hash
}

// ---------------------------------------------------------------------------
// Consensus-bounded batch envelope (the per-transaction cost ceiling)
// ---------------------------------------------------------------------------

fn repeat(item: ByteArray, n: Int) -> List<ByteArray> {
  if n <= 0 {
    []
  } else {
    [item, ..repeat(item, n - 1)]
  }
}

fn fold_left(current: ByteArray, remaining: Int) -> ByteArray {
  if remaining == 0 {
    current
  } else {
    fold_left(validation_merkle_v1.hash_branch(current, h32), remaining - 1)
  }
}

/// Bottom-up siblings of any leaf in a perfect tree whose leaves are all
/// `leaf`: every sibling at level k is the level-k node itself.
fn identical_levels(leaf: ByteArray, height: Int) -> List<ByteArray> {
  if height == 0 {
    []
  } else {
    [
      leaf,
      ..identical_levels(
        validation_merkle_v1.hash_branch(leaf, leaf),
        height - 1,
      )
    ]
  }
}

fn identical_root(leaf: ByteArray, height: Int) -> ByteArray {
  if height == 0 {
    leaf
  } else {
    identical_root(validation_merkle_v1.hash_branch(leaf, leaf), height - 1)
  }
}

fn pairwise(level: List<ByteArray>) -> List<ByteArray> {
  when level is {
    [] -> []
    [left, right, ..rest] ->
      [validation_merkle_v1.hash_branch(left, right), ..pairwise(rest)]
    _ -> fail @"perfect tree levels have even width"
  }
}

/// Every level of a perfect tree over `leaves`, bottom-up, ending in the root.
fn tree_levels(level: List<ByteArray>) -> List<List<ByteArray>> {
  when level is {
    [_] -> [level]
    _ -> [level, ..tree_levels(pairwise(level))]
  }
}

fn tree_root(levels: List<List<ByteArray>>) -> ByteArray {
  expect Some([root]) = list.last(levels)
  root
}

/// Bottom-up siblings of `index` from precomputed tree levels.
fn level_proof(levels: List<List<ByteArray>>, index: Int) -> List<ByteArray> {
  when levels is {
    [] -> []
    [_] -> []
    [level, ..rest] -> {
      let sibling_index =
        if index % 2 == 0 {
          index + 1
        } else {
          index - 1
        }
      expect Some(sibling) = list.at(level, sibling_index)
      [sibling, ..level_proof(rest, index / 2)]
    }
  }
}

fn count_up(from: Int, to: Int) -> List<Int> {
  if from >= to {
    []
  } else {
    [from, ..count_up(from + 1, to)]
  }
}

/// No admissible transaction field exceeds 32,768 preimage bytes, so no
/// purpose or source frontier reaches 4,096 leaves: a 2,048-leaf frontier
/// (one peak of height 11, eleven siblings per opening) covers every
/// realisable purpose and source frontier. A full `maximum_scan_batch` of
/// purpose openings at that depth is the step-05 ExUnit ceiling the size plan
/// records beside the emulator rows.
test maximum_purpose_batch_at_consensus_bounded_frontier() {
  let leaf = purpose_leaf(0, 0, h28b)
  let count = 2048
  let initial =
    rule.initial_reverse_scan_v1(
      rule.AuthenticatedWitnessV1 {
        bound: rule.bind_witness_v1(subject(0), h32, 1, 0),
        prior_ledger_root: h32,
        language_tag: 3,
        script_hash: h28a,
        script_total_length: 10,
        item_commitment: h32,
        source_count: 1,
        source_peaks: [
          validation_merkle_v1.FrontierPeak { height: 0, hash: inline_leaf(0) },
        ],
        purpose_count: count,
        purpose_peaks: [
          validation_merkle_v1.FrontierPeak {
            height: 11,
            hash: identical_root(leaf, 11),
          },
        ],
      },
    )
  let siblings = identical_levels(leaf, 11)
  let scanned =
    list.foldl(
      count_up(0, rule.maximum_scan_batch),
      initial,
      fn(index, state) {
        rule.scan_purpose_v1(
          state,
          rule.PurposeOpeningV1 {
            frontier_index: index,
            purpose_kind: 0,
            purpose_index: 0,
            script_hash: h28b,
            purpose_subject: #"aa",
            siblings,
          },
        )
      },
    )
  and {
    scanned.purpose_cursor == rule.maximum_scan_batch,
    !scanned.used,
    !rule.reverse_scan_complete_v1(scanned),
  }
}

/// The step-04 twin: a full batch of alternate-source openings over a
/// 2,048-leaf source frontier (the 32 lowest leaves are distinct inline
/// descriptors, the rest filler subtrees), for an accused coordinate deep in
/// field 6.
test maximum_alternate_batch_at_consensus_bounded_frontier() {
  let levels = tree_levels(list.map(count_up(0, 32), inline_leaf))
  let count = 2048
  let initial =
    rule.initial_reverse_scan_v1(
      rule.AuthenticatedWitnessV1 {
        bound: rule.bind_witness_v1(subject(0), h32, 1, count - 1),
        prior_ledger_root: h32,
        language_tag: 3,
        script_hash: h28a,
        script_total_length: 10,
        item_commitment: h32,
        source_count: count,
        source_peaks: [
          validation_merkle_v1.FrontierPeak {
            height: 11,
            hash: fold_left(tree_root(levels), 6),
          },
        ],
        purpose_count: 1,
        purpose_peaks: [
          validation_merkle_v1.FrontierPeak {
            height: 0,
            hash: purpose_leaf(0, 0, h28b),
          },
        ],
      },
    )
  let walked =
    list.foldl(
      count_up(0, rule.maximum_scan_batch),
      initial,
      fn(index, state) {
        rule.authenticate_earlier_source_v1(
          state,
          rule.SourceOpeningV1 {
            source_index: index,
            language_tag: 3,
            script_hash: h28a,
            total_length: 1,
            item_commitment: h32,
            siblings: list.concat(level_proof(levels, index), repeat(h32, 6)),
          },
        )
      },
    )
  and {
    walked.alternate_cursor == rule.maximum_scan_batch,
    walked.shadowed,
    !rule.alternate_sources_complete_v1(walked),
  }
}

// ---------------------------------------------------------------------------
// Cross-language golden vectors (twin: unused-script-witness.test.ts)
// ---------------------------------------------------------------------------

fn golden_authenticated(direction: Int) -> rule.AuthenticatedWitnessV1 {
  rule.AuthenticatedWitnessV1 {
    bound: rule.bind_witness_v1(subject(direction), h32, 1, 1),
    prior_ledger_root: h32,
    language_tag: 3,
    script_hash: h28a,
    script_total_length: 10,
    item_commitment: h32,
    source_count: 2,
    source_peaks: [validation_merkle_v1.FrontierPeak { height: 1, hash: h32 }],
    purpose_count: 1,
    purpose_peaks: [validation_merkle_v1.FrontierPeak { height: 0, hash: h32 }],
  }
}

test bound_witness_golden_vector() {
  cbor.serialise(rule.bind_witness_v1(subject(0), h32, 1, 0)) == #"d8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff582033333333333333333333333333333333333333333333333333333333333333330100ff"
}

test authenticated_accepted_golden_vector() {
  cbor.serialise(golden_authenticated(0)) == #"d8799fd8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff582033333333333333333333333333333333333333333333333333333333333333330101ff5820333333333333333333333333333333333333333333333333333333333333333303581c111111111111111111111111111111111111111111111111111111110a58203333333333333333333333333333333333333333333333333333333333333333029fd8799f0158203333333333333333333333333333333333333333333333333333333333333333ffff019fd8799f0058203333333333333333333333333333333333333333333333333333333333333333ffffff"
}

test authenticated_forced_golden_vector() {
  cbor.serialise(golden_authenticated(1)) == #"d8799fd8799fd8799f0101015820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f4101d8799fd9051a9f01ffffff582033333333333333333333333333333333333333333333333333333333333333330101ff5820333333333333333333333333333333333333333333333333333333333333333303581c111111111111111111111111111111111111111111111111111111110a58203333333333333333333333333333333333333333333333333333333333333333029fd8799f0158203333333333333333333333333333333333333333333333333333333333333333ffff019fd8799f0058203333333333333333333333333333333333333333333333333333333333333333ffffff"
}

/// Also pins the domain-separated checkpoint seed across both languages.
test initial_reverse_scan_golden_vector() {
  rule.encode_reverse_scan_v1(
    rule.initial_reverse_scan_v1(golden_authenticated(0)),
  ) == #"d8799fd8799fd8799fd8799f0100005820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f40d87a80ff582033333333333333333333333333333333333333333333333333333333333333330101ff5820333333333333333333333333333333333333333333333333333333333333333303581c111111111111111111111111111111111111111111111111111111110a58203333333333333333333333333333333333333333333333333333333333333333029fd8799f0158203333333333333333333333333333333333333333333333333333333333333333ffff019fd8799f0058203333333333333333333333333333333333333333333333333333333333333333ffffff0000d87980d8798058200f7f6c7b0a0b8b3507c96310eb9a38807ead1a0832623262ed24aebe8aa7499cff"
}

test decision_golden_vector() {
  cbor.serialise(
    rule.DecisionV1 { subject: subject(1), script_index: 1, unused: False },
  ) == #"d8799fd8799f0101015820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f4101d8799fd9051a9f01ffffff01d87980ff"
}
