use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
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
  let leaves = hashes
    |> list.indexed_map(fn(index, hash) { purpose_leaf(index % 4, 0, hash) })
  let frontier = validation_merkle_v1.build_frontier(leaves)
  let initial = rule.initial_reverse_scan_v1(
    witness(direction, [h32, h32], leaves),
  )
  let source_leaf = script_proof_v1.inline_source_leaf_hash(0, 3, h28b, 10, h32)
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf, h32])
  let with_sources = rule.AuthenticatedWitnessV1 {
    ..initial.witness,
    source_count: 2,
    source_peaks: source_frontier.peaks,
    purpose_count: frontier.count,
    purpose_peaks: frontier.peaks,
  }
  let after_source = rule.authenticate_earlier_source_v1(
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
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
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
  let control = vm_machine_types.ScriptSourcesControlV1 {
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
  let machine_state = validation_trace_v1.ValidationMachineStateV1 {
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
  let descriptor = validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_leaf_hash(state_hash),
    step_count: 0,
    initial_state_hash: state_hash,
    terminal_state_hash: state_hash,
    verdict: validation_trace_v1.Accepted,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
  }
  let phas_root = mpf.root(
    mpf.insert(
      mpf.empty,
      cbor.serialise(event_key),
      cbor.serialise(descriptor),
      [],
    ),
  )
  let membership = transition_trace.RootMembershipProof {
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
  expect _bound = rule.bind_witness_v1(
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
  expect _decision = rule.decision_v1(
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

test reverse_checkpoint_is_deterministic() {
  rule.encode_reverse_scan_v1(scan_without_alternates(0, [h28b])) == rule.encode_reverse_scan_v1(
    scan_without_alternates(0, [h28b]),
  )
}

test alternate_source_membership_substitution_refuses() fail {
  let state = rule.initial_reverse_scan_v1(witness(0, [h32, h32], []))
  expect _next = rule.authenticate_earlier_source_v1(
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
