use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use midgard/fraud_proofs/missing_script_source/rule
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{OutputNonCanonical, ScriptSourceMissing}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h28a = #"11111111111111111111111111111111111111111111111111111111"

const h28b = #"22222222222222222222222222222222222222222222222222222222"

const h32 = #"3333333333333333333333333333333333333333333333333333333333333333"

const scan_hash = #"44444444444444444444444444444444444444444444444444444444"

const final_hash = #"55555555555555555555555555555555555555555555555555555555"

fn subject(direction: Int, kind: Int, index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction,
    source_kind: if direction == 0 {
      0
    } else {
      1
    },
    transaction_id: tx_id,
    source_key: if direction == 0 {
      #""
    } else {
      #"01"
    },
    rejection_reason: if direction == 0 {
      None
    } else {
      Some(ScriptSourceMissing { purpose_kind: kind, purpose_index: index })
    },
  }
}

fn source_leaf(index: Int, location: Int, hash: ByteArray) -> ByteArray {
  script_proof_v1.source_descriptor_leaf_hash(
    if location == 0 {
      0
    } else {
      1
    },
    if location == 0 {
      #"00"
    } else {
      #"825820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f190000"
    },
    3,
    hash,
    10,
    h32,
  )
}

fn descriptor(
  leaves: List<ByteArray>,
  index: Int,
  location: Int,
  hash: ByteArray,
) -> rule.SourceDescriptorV1 {
  rule.SourceDescriptorV1 {
    source_index: index,
    location_kind: location,
    source_key: if location == 0 {
      #"00"
    } else {
      #"825820000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f190000"
    },
    language_tag: 3,
    script_hash: hash,
    total_length: 10,
    item_commitment: h32,
    siblings: proof(leaves, index),
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
    _ -> fail @"test proof helper supports one to three leaves"
  }
}

fn initial(
  direction: Int,
  hashes: List<ByteArray>,
) -> Pair<rule.SourceScanStateV1, List<ByteArray>> {
  let leaves = hashes
    |> list.indexed_map(
        fn(index, hash) {
          source_leaf(
            index,
            if index == 0 {
              0
            } else {
              1
            },
            hash,
          )
        },
      )
  let frontier = validation_merkle_v1.build_frontier(leaves)
  let bound = rule.bind_purpose_v1(subject(direction, 0, 0), h32, 1, h32, 0, 0)
  let purpose = rule.AuthenticatedPurposeV1 {
    bound,
    prior_ledger_root: h32,
    required_script_hash: h28a,
    source_count: frontier.count,
    scan_limit: frontier.count,
    source_peaks: frontier.peaks,
  }
  let tx = rule.authenticate_transaction_sources_v1(
    purpose,
    if frontier.count > 0 {
      1
    } else {
      0
    },
  )
  let resolved = rule.authenticate_resolved_sources_v1(
    tx,
    if frontier.count > 1 {
      frontier.count - 1
    } else {
      0
    },
  )
  Pair(rule.initial_scan_v1(resolved, scan_hash), leaves)
}

fn complete(direction: Int, hashes: List<ByteArray>) -> rule.SourceScanStateV1 {
  let Pair(state, leaves) = initial(direction, hashes)
  scan_all(state, hashes, leaves, list.length(hashes))
}

fn scan_all(
  state: rule.SourceScanStateV1,
  hashes: List<ByteArray>,
  leaves: List<ByteArray>,
  total: Int,
) -> rule.SourceScanStateV1 {
  when hashes is {
    [] -> state
    [hash, ..rest] -> {
      let index = state.cursor
      let next = rule.scan_source_v1(
        state,
        descriptor(
          leaves,
          index,
          if index == 0 {
            0
          } else {
            1
          },
          hash,
        ),
        if index + 1 == total {
          final_hash
        } else {
          scan_hash
        },
      )
      scan_all(next, rest, leaves, total)
    }
  }
}

test binds_all_purpose_kinds() {
  [0, 1, 2, 3]
    |> list.all(
        fn(kind) {
          rule.bind_purpose_v1(subject(1, kind, 2), h32, 1, h32, kind, 2).purpose_kind == kind
        },
      )
}

test refuses_substituted_purpose_coordinate() fail {
  rule.bind_purpose_v1(subject(1, 2, 3), h32, 1, h32, 2, 4).purpose_index == 4
}

test refuses_another_reason_constructor() fail {
  let wrong = substrate.VerdictSubjectV1 {
    ..subject(1, 0, 0),
    rejection_reason: Some(OutputNonCanonical { output_index: 0 }),
  }
  rule.bind_purpose_v1(wrong, h32, 1, h32, 0, 0).purpose_index == 0
}

test complete_absence_scans_witness_and_resolved_reference_sources() {
  let state = complete(0, [h28b, h28b, h28b])
  and {
    state.cursor == 3,
    !state.found,
    rule.scan_complete_v1(state),
  }
}

test alternate_source_location_finds_required_hash() {
  [0, 1]
    |> list.all(
        fn(found_index) {
          let hashes = [0, 1, 2]
            |> list.map(
                fn(index) {
                  if index == found_index {
                    h28a
                  } else {
                    h28b
                  }
                },
              )
          complete(1, hashes).found
        },
      )
}

test wrongful_acceptance_absence_convicts() {
  rule.terminal_contradiction_v1(complete(0, [h28b, h28b, h28b]))
}

test wrongful_rejection_presence_convicts() {
  rule.terminal_contradiction_v1(complete(1, [h28b, h28a, h28b]))
}

test honest_acceptance_presence_refuses() fail {
  rule.terminal_contradiction_v1(complete(0, [h28a, h28b, h28b]))
}

test honest_rejection_absence_refuses() fail {
  rule.terminal_contradiction_v1(complete(1, [h28b, h28b, h28b]))
}

test partial_scan_cannot_finalize() fail {
  let Pair(state, _) = initial(0, [h28b, h28b, h28b])
  rule.terminal_contradiction_v1(state)
}

test substituted_source_membership_refuses() fail {
  let Pair(state, leaves) = initial(0, [h28b, h28b, h28b])
  rule.scan_source_v1(state, descriptor(leaves, 0, 0, h28a), scan_hash).cursor == 1
}

test alternate_source_index_refuses() fail {
  let Pair(state, leaves) = initial(0, [h28b, h28b, h28b])
  rule.scan_source_v1(state, descriptor(leaves, 1, 1, h28b), scan_hash).cursor == 1
}

test alternate_source_location_refuses() fail {
  let Pair(state, leaves) = initial(0, [h28b, h28b, h28b])
  rule.scan_source_v1(state, descriptor(leaves, 0, 1, h28b), scan_hash).cursor == 1
}

test malformed_checkpoint_refuses() fail {
  let Pair(state, leaves) = initial(0, [h28b, h28b, h28b])
  rule.scan_source_v1(
    rule.SourceScanStateV1 { ..state, checkpoint_hash: h32 },
    descriptor(leaves, 0, 0, h28b),
    scan_hash,
  ).cursor == 1
}

test wrong_successor_hash_refuses() fail {
  let Pair(state, leaves) = initial(0, [h28b, h28b, h28b])
  rule.scan_source_v1(state, descriptor(leaves, 0, 0, h28b), h32).cursor == 1
}

fn trace_authenticated_purpose(
  substitute_source_frontier: Bool,
) -> rule.AuthenticatedPurposeV1 {
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h28a, #"aa")
  let source = validation_merkle_v1.build_frontier([source_leaf(0, 0, h28b)])
  let purpose = validation_merkle_v1.build_frontier([purpose_leaf])
  let discovery = validation_machine_v1.ScriptDiscoveryControlV1 {
    purpose_cursor: 0,
    source_cursor: source.count,
    redeemer_cursor: 0,
    current_purpose_kind: 0,
    current_purpose_index: 0,
    current_script_hash: h28a,
    current_subject: #"aa",
    matched_source_index: -1,
    matched_language_tag: -1,
    matched_source_leaf: #"",
    used_inline_bitmap: 0,
    used_redeemer_bitmap: 0,
    redeemer_item_control_hash: #"",
    execution_count: 0,
    execution_peaks: [],
  }
  let canonical_control = validation_machine_v1.ScriptSourcesControlV1 {
    compact_cbor: #"",
    witness_set_compact_cbor: #"",
    field_preimage_lengths_cbor: #"",
    context_cbor: #"",
    resolved_input_count: 0,
    resolved_inputs_accumulator: h32,
    resolved_item_peaks: [],
    signer_count: 0,
    signer_frontier_commitment: h32,
    source_count: source.count,
    source_peaks: source.peaks,
    redeemer_count: 0,
    redeemer_peaks: [],
    purpose_count: purpose.count,
    purpose_peaks: purpose.peaks,
    output_count: 0,
    output_peaks: [],
    stage: 9,
    replay_cursor: 0,
    replay_accumulator: h32,
    replay_remaining_schedule_hash: h32,
    spend_index: 0,
    output_cursor: 0,
    output_total_count: 0,
    receive_scan: validation_machine_v1.empty_receive_purpose_scan_control(),
    source_total_count: source.count,
    redeemer_total_count: 0,
    observer_scan: validation_machine_v1.empty_observer_purpose_scan_control(),
    discovery,
    output_proof: None,
    pending_source_cbor: #"",
    mint_fold: validation_machine_v1.empty_mint_fold_control(),
    resolution_schedule_hash: h32,
  }
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let state = validation_trace_v1.ValidationMachineStateV1 {
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
      validation_machine_v1.encode_script_sources_discovery_witness(
        canonical_control,
        9,
        discovery,
      ),
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
    ledger_delta_root: h32,
  }
  let state_hash = validation_trace_v1.hash_machine_state(state)
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
  let supplied_control = if substitute_source_frontier {
    let fabricated = validation_merkle_v1.build_frontier([h32])
    validation_machine_v1.ScriptSourcesControlV1 {
      ..canonical_control,
      source_peaks: fabricated.peaks,
    }
  } else {
    canonical_control
  }
  rule.authenticate_purpose_frontier_v1(
    rule.bind_purpose_v1(
      subject(0, 0, 0),
      membership.root,
      1,
      blake2b_256(#""),
      0,
      0,
    ),
    membership,
    state,
    validation_trace_v1.ValidationTraceProof {
      state_index: 0,
      state_hash,
      siblings: [],
    },
    supplied_control,
    0,
    h28a,
    #"aa",
    [],
  )
}

test authenticates_header_trace_work_root_and_complete_frontiers() {
  let purpose = trace_authenticated_purpose(False)
  and {
    purpose.required_script_hash == h28a,
    purpose.source_count == 1,
  }
}

test fabricated_frontier_invalidates_committed_header_trace() fail {
  trace_authenticated_purpose(True).source_count == 1
}

test honest_canonical_header_with_present_source_refuses_conviction() fail {
  let purpose = trace_authenticated_purpose(False)
  let staged = rule.authenticate_transaction_sources_v1(purpose, 1)
  let universe = rule.authenticate_resolved_sources_v1(staged, 0)
  let source_hashes = [source_leaf(0, 0, h28a)]
  let scanned = rule.scan_source_v1(
    rule.initial_scan_v1(universe, scan_hash),
    descriptor(source_hashes, 0, 0, h28a),
    final_hash,
  )
  rule.terminal_contradiction_v1(scanned)
}
