use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/merkle_patricia_forestry as mpf
use aiken/primitive/bytearray
use midgard/fraud_proofs/missing_script_source/rule
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{OutputNonCanonical, ScriptSourceMissing}
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
  initial_with_limit(direction, hashes, list.length(hashes))
}

/// A scan state over the complete frontier of `hashes` whose authenticated
/// scan limit may stop short of it: the wrongful-rejection prefix shape.
fn initial_with_limit(
  direction: Int,
  hashes: List<ByteArray>,
  scan_limit: Int,
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
    scan_limit,
    source_peaks: frontier.peaks,
  }
  let tx = rule.AuthenticatedTransactionSourcesV1 {
    purpose,
    transaction_source_count: 1,
  }
  let resolved = rule.authenticate_resolved_sources_v1(tx)
  Pair(rule.initial_scan_v1(resolved, scan_hash), leaves)
}

fn descriptors(
  leaves: List<ByteArray>,
  hashes: List<ByteArray>,
  count: Int,
) -> List<rule.SourceDescriptorV1> {
  hashes
    |> list.take(count)
    |> list.indexed_map(
        fn(index, hash) {
          descriptor(
            leaves,
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
}

fn complete(direction: Int, hashes: List<ByteArray>) -> rule.SourceScanStateV1 {
  let Pair(state, leaves) = initial(direction, hashes)
  rule.advance_scan_v1(
    state,
    descriptors(leaves, hashes, list.length(hashes)),
    scan_hash,
    final_hash,
  )
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

test batch_advance_over_the_complete_frontier_finalizes() {
  let hashes = [h28b, h28b, h28b]
  let advanced = complete(0, hashes)
  and {
    advanced.cursor == 3,
    !advanced.found,
    advanced.next_expected_script_hash == final_hash,
    rule.scan_complete_v1(advanced),
    rule.terminal_contradiction_v1(advanced),
  }
}

test batch_advance_hands_the_scan_back_to_itself_before_the_limit() {
  let hashes = [h28b, h28b, h28b]
  let Pair(state, leaves) = initial(0, hashes)
  let advanced = rule.advance_scan_v1(
    state,
    descriptors(leaves, hashes, 2),
    scan_hash,
    final_hash,
  )
  and {
    advanced.cursor == 2,
    advanced.next_expected_script_hash == scan_hash,
    !rule.scan_complete_v1(advanced),
  }
}

test prefix_scan_finalizes_at_the_scan_limit_not_the_frontier_end() {
  // A wrongful rejection whose matching source is the second of three: the
  // authenticated prefix is two sources long and the finalizer is the
  // successor exactly when the walk reaches it, with the found flag set.
  let hashes = [h28b, h28a, h28b]
  let Pair(state, leaves) = initial_with_limit(1, hashes, 2)
  let advanced = rule.advance_scan_v1(
    state,
    descriptors(leaves, hashes, 2),
    scan_hash,
    final_hash,
  )
  and {
    advanced.cursor == 2,
    advanced.found,
    advanced.next_expected_script_hash == final_hash,
    rule.scan_complete_v1(advanced),
    rule.terminal_contradiction_v1(advanced),
  }
}

test prefix_scan_refuses_sources_beyond_the_scan_limit() fail {
  let hashes = [h28b, h28a, h28b]
  let Pair(state, leaves) = initial_with_limit(1, hashes, 2)
  rule.advance_scan_v1(
    state,
    descriptors(leaves, hashes, 3),
    scan_hash,
    final_hash,
  ).cursor == 3
}

test partial_scan_cannot_finalize() fail {
  let Pair(state, _) = initial(0, [h28b, h28b, h28b])
  rule.terminal_contradiction_v1(state)
}

test substituted_source_membership_refuses() fail {
  let Pair(state, leaves) = initial(0, [h28b, h28b, h28b])
  rule.advance_scan_v1(
    state,
    [descriptor(leaves, 0, 0, h28a)],
    scan_hash,
    final_hash,
  ).cursor == 1
}

test alternate_source_location_refuses() fail {
  let Pair(state, leaves) = initial(0, [h28b, h28b, h28b])
  rule.advance_scan_v1(
    state,
    [descriptor(leaves, 0, 1, h28b)],
    scan_hash,
    final_hash,
  ).cursor == 1
}

test batch_refuses_a_skipped_source_index() fail {
  // At cursor 1 the batch presents source 2, a genuine member at the same
  // location kind, skipping source 1.
  let hashes = [h28b, h28a, h28b]
  let Pair(state, leaves) = initial(0, hashes)
  let advanced = rule.advance_scan_v1(
    state,
    [descriptor(leaves, 0, 0, h28b), descriptor(leaves, 2, 1, h28b)],
    scan_hash,
    final_hash,
  )
  advanced.cursor == 2 && !advanced.found
}

test empty_batch_completes_an_authentic_complete_state() {
  let state = complete(0, [h28b])
  rule.scan_complete_v1(rule.advance_scan_v1(state, [], scan_hash, final_hash))
}

test empty_batch_cannot_complete_a_forged_state() {
  let state = complete(0, [h28b])
  !rule.scan_complete_v1(
    rule.advance_scan_v1(
      rule.SourceScanStateV1 { ..state, checkpoint_hash: h32 },
      [],
      scan_hash,
      final_hash,
    ),
  )
}

test batch_refuses_a_substituted_later_source() fail {
  // The batch checks every source, not only the first.
  let hashes = [h28b, h28b, h28b]
  let Pair(state, leaves) = initial(0, hashes)
  rule.advance_scan_v1(
    state,
    [
      descriptor(leaves, 0, 0, h28b),
      descriptor(leaves, 1, 1, h28a),
      descriptor(leaves, 2, 1, h28b),
    ],
    scan_hash,
    final_hash,
  ).cursor == 3
}

test batch_refuses_a_repeated_source_index() fail {
  let hashes = [h28b, h28b, h28b]
  let Pair(state, leaves) = initial(0, hashes)
  rule.advance_scan_v1(
    state,
    [descriptor(leaves, 0, 0, h28b), descriptor(leaves, 0, 0, h28b)],
    scan_hash,
    final_hash,
  ).cursor == 2
}

test batch_refuses_a_malformed_checkpoint() fail {
  let hashes = [h28b, h28b, h28b]
  let Pair(state, leaves) = initial(0, hashes)
  rule.advance_scan_v1(
    rule.SourceScanStateV1 { ..state, checkpoint_hash: h32 },
    descriptors(leaves, hashes, 2),
    scan_hash,
    final_hash,
  ).cursor == 2
}

test batch_advance_over_a_prefix_agrees_with_split_batches() {
  let hashes = [h28b, h28a, h28b]
  let Pair(state, leaves) = initial(1, hashes)
  let advanced = rule.advance_scan_v1(
    state,
    descriptors(leaves, hashes, 2),
    scan_hash,
    final_hash,
  )
  let split = rule.advance_scan_v1(
    rule.advance_scan_v1(
      state,
      [descriptor(leaves, 0, 0, h28b)],
      scan_hash,
      final_hash,
    ),
    [descriptor(leaves, 1, 1, h28a)],
    scan_hash,
    final_hash,
  )
  advanced == split && advanced.found
}

/// The stage-9 control a missing-script-source proof opens: a source frontier
/// over `leaves` whose first `inline_count` are the inline witnesses, and the
/// purpose (`h28a`) at lookup cursor `source_cursor`.
fn stage_nine_control(
  leaves: List<ByteArray>,
  inline_count: Int,
  source_cursor: Int,
) -> vm_machine_types.ScriptSourcesControlV1 {
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(0, 0, h28a, #"aa")
  let source = validation_merkle_v1.build_frontier(leaves)
  let purpose = validation_merkle_v1.build_frontier([purpose_leaf])
  let discovery = vm_machine_types.ScriptDiscoveryControlV1 {
    purpose_cursor: 0,
    source_cursor,
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
  vm_machine_types.ScriptSourcesControlV1 {
    compact_cbor: #"",
    witness_set_compact_cbor: #"",
    field_preimage_lengths_cbor: #"",
    context_cbor: #"",
    resolved_input_count: 0,
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
    replay_remaining_schedule_hash: h32,
    spend_index: 0,
    output_cursor: 0,
    output_total_count: 0,
    receive_scan: vm_witness.empty_receive_purpose_scan_control(),
    inline_source_count: inline_count,
    redeemer_total_count: 0,
    observer_scan: vm_witness.empty_observer_purpose_scan_control(),
    discovery,
    output_proof: None,
    pending_source_cbor: #"",
    mint_fold: vm_witness.empty_mint_fold_control(),
    resolution_schedule_hash: h32,
  }
}

/// The machine state whose work root commits `control`.
fn stage_nine_state(
  control: vm_machine_types.ScriptSourcesControlV1,
) -> validation_trace_v1.ValidationMachineStateV1 {
  stage_nine_state_committing(
    vm_witness.encode_script_sources_discovery_witness(
      control,
      9,
      control.discovery,
    ),
  )
}

/// The machine state whose work root commits the work witness `witness`.
fn stage_nine_state_committing(
  witness: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
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
      witness,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: #"0000000000000000000000000000000000000000000000000000000000000000",
  }
}

/// The trace state step 02 hands step 03 for `machine_state`, under a subject
/// of `direction`.
fn stage_nine_trace(
  direction: Int,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
) -> rule.AuthenticatedTraceStateV1 {
  rule.AuthenticatedTraceStateV1 {
    bound: rule.bind_purpose_v1(
      subject(direction, 0, 0),
      h32,
      1,
      blake2b_256(#""),
      0,
      0,
    ),
    machine_state,
  }
}

fn trace_authenticated_purpose(
  substitute_source_frontier: Bool,
) -> rule.AuthenticatedPurposeV1 {
  let canonical_control = stage_nine_control([source_leaf(0, 0, h28b)], 1, 1)
  let state = stage_nine_state(canonical_control)
  let event_key = ledger_state.L2TransactionEventKey { tx_id }
  let state_hash = validation_trace_v1.hash_machine_state(state)
  let terminal_hash = validation_trace_v1.trace_leaf_hash(state_hash)
  let descriptor = validation_trace_v1.ValidationTraceDescriptorV1 {
    schema_version: validation_trace_v1.descriptor_version,
    machine_version: validation_trace_v1.machine_version,
    trace_root: validation_trace_v1.trace_branch_hash(
      validation_trace_v1.trace_leaf_hash(state_hash),
      validation_trace_v1.trace_leaf_hash(terminal_hash),
    ),
    step_count: 1,
    initial_state_hash: state_hash,
    terminal_state_hash: terminal_hash,
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
    vm_machine_types.ScriptSourcesControlV1 {
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
      siblings: [validation_trace_v1.trace_leaf_hash(terminal_hash)],
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
  let staged = rule.AuthenticatedTransactionSourcesV1 {
    purpose,
    transaction_source_count: 1,
  }
  let universe = rule.authenticate_resolved_sources_v1(staged)
  let source_hashes = [source_leaf(0, 0, h28a)]
  let scanned = rule.advance_scan_v1(
    rule.initial_scan_v1(universe, scan_hash),
    [descriptor(source_hashes, 0, 0, h28a)],
    scan_hash,
    final_hash,
  )
  rule.terminal_contradiction_v1(scanned)
}

// ---------------------------------------------------------------------------
// Step 03's witness-located prefix
// ---------------------------------------------------------------------------

/// `control`'s stage-9 work witness with `inline_source_count` written into
/// its frame directly, so no encoder bound applies to it. The honest encodings
/// at counts 0 and 1 differ in exactly one byte, that field's head, which
/// locates it; the helper expects that, so the location cannot drift.
fn witness_with_inline_count(
  control: vm_machine_types.ScriptSourcesControlV1,
  inline_source_count: Int,
) -> ByteArray {
  let encode = fn(count) {
    vm_witness.encode_script_sources_discovery_witness(
      vm_machine_types.ScriptSourcesControlV1 {
        ..control,
        inline_source_count: count,
      },
      9,
      control.discovery,
    )
  }
  let at_zero = encode(0)
  let at_one = encode(1)
  let head = first_difference(at_zero, at_one, 0)
  expect bytearray.length(at_zero) == bytearray.length(at_one)
  expect bytearray.drop(at_zero, head + 1) == bytearray.drop(at_one, head + 1)
  let count_head = cbor.serialise(inline_source_count)
  expect bytearray.length(count_head) == 1
  bytearray.take(at_zero, head)
    |> bytearray.concat(count_head)
    |> bytearray.concat(bytearray.drop(at_zero, head + 1))
}

fn first_difference(left: ByteArray, right: ByteArray, index: Int) -> Int {
  if bytearray.at(left, index) == bytearray.at(right, index) {
    first_difference(left, right, index + 1)
  } else {
    index
  }
}

/// Step 03 over the stage-9 control of `leaves`, the first `inline_count` of
/// them inline witnesses, at lookup cursor `source_cursor`. The pre-state's
/// work root commits control bytes assembled past the encoder, so any bound
/// on `inline_count` is step 03's own.
fn staged_sources(
  direction: Int,
  leaves: List<ByteArray>,
  inline_count: Int,
  source_cursor: Int,
) -> rule.AuthenticatedTransactionSourcesV1 {
  let control = stage_nine_control(leaves, inline_count, source_cursor)
  rule.authenticate_transaction_sources_v1(
    stage_nine_trace(
      direction,
      stage_nine_state_committing(
        witness_with_inline_count(control, inline_count),
      ),
    ),
    control,
    0,
    h28a,
    #"aa",
    [],
  )
}

/// Steps 04 to the finalizer over `sources` in one batch.
fn staged_scan_convicts(
  staged: rule.AuthenticatedTransactionSourcesV1,
  sources: List<rule.SourceDescriptorV1>,
) -> Bool {
  rule.initial_scan_v1(rule.authenticate_resolved_sources_v1(staged), scan_hash)
    |> rule.advance_scan_v1(sources, scan_hash, final_hash)
    |> rule.terminal_contradiction_v1
}

/// One inline witness, then one resolved reference script; neither is `h28a`.
fn inline_then_resolved() -> List<ByteArray> {
  [source_leaf(0, 0, h28b), source_leaf(1, 1, h28b)]
}

/// At an honest count the hand-assembled witness is the encoding itself.
test witness_with_an_honest_inline_count_is_the_encoding() {
  let control = stage_nine_control(inline_then_resolved(), 1, 2)
  witness_with_inline_count(control, 1) == vm_witness.encode_script_sources_discovery_witness(
    control,
    9,
    control.discovery,
  )
}

/// The witness prefix is the control's inline count: source 0 is read at the
/// witness location and source 1 at the resolved one, so the absence of the
/// purpose's script is proven.
test witness_prefix_is_the_inline_source_count() {
  let leaves = inline_then_resolved()
  staged_sources(0, leaves, 1, 2)
    |> staged_scan_convicts(
        [descriptor(leaves, 0, 0, h28b), descriptor(leaves, 1, 1, h28b)],
      )
}

test witness_prefix_refuses_an_honest_acceptance_with_a_resolved_source() fail {
  let leaves = [source_leaf(0, 0, h28b), source_leaf(1, 1, h28a)]
  staged_sources(0, leaves, 1, 2)
    |> staged_scan_convicts(
        [descriptor(leaves, 0, 0, h28b), descriptor(leaves, 1, 1, h28a)],
      )
}

/// A wrongful-rejection limit may end inside the inline witnesses: lookup
/// cursor 0 over two inline witnesses limits the scan to source 0 while the
/// witness prefix stays two.
test wrongful_rejection_limit_within_the_witness_prefix_convicts() {
  let leaves = [source_leaf(0, 0, h28a), source_leaf(1, 0, h28b)]
  staged_sources(1, leaves, 2, 0)
    |> staged_scan_convicts([descriptor(leaves, 0, 0, h28a)])
}

/// A work root may commit more inline witnesses than sources, but step 03
/// refuses it: the control encoder that step 03 recomputes the work root with
/// expects the bound.
test witness_prefix_refuses_more_inline_sources_than_sources() fail {
  let leaves = inline_then_resolved()
  staged_sources(0, leaves, 3, 2).purpose.source_count == 2
}

/// The bound's other side: every source an inline witness.
test witness_prefix_may_hold_every_source() {
  let leaves = [source_leaf(0, 0, h28b), source_leaf(1, 0, h28b)]
  staged_sources(0, leaves, 2, 2)
    |> staged_scan_convicts(
        [descriptor(leaves, 0, 0, h28b), descriptor(leaves, 1, 0, h28b)],
      )
}

test witness_prefix_refuses_a_negative_inline_source_count() fail {
  let leaves = inline_then_resolved()
  staged_sources(0, leaves, -1, 2).purpose.source_count == 2
}

/// The bound's other side: no inline witness, every source resolved.
test witness_prefix_may_be_empty() {
  let leaves = [source_leaf(0, 1, h28b), source_leaf(1, 1, h28b)]
  staged_sources(0, leaves, 0, 2)
    |> staged_scan_convicts(
        [descriptor(leaves, 0, 1, h28b), descriptor(leaves, 1, 1, h28b)],
      )
}
