//// The phase-A native chunk window admits exactly one successor.
////
//// Two kinds of pre-state used to admit none. A late continuation whose
//// source is not field 6 item 0 (a reference output, or inline item 1 or
//// later) could never open a chunk, because the window checked the chunk's
//// coordinates against field 6 and `script_seen`. An end of item on a
//// 4095-aligned item could never open a chunk, because the chunk at
//// `cursor / 4095` does not exist. Each test below takes one pre-state and
//// offers several successors, and exactly the derived one is accepted.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, MidgardVersionedScript, NativeCardanoScript,
}
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{
  reject_invalid_field_type, reject_native_script_invalid,
}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as mt
use midgard/validation_machine/native_scripts as vm_native_scripts
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/phase_a_native_scripts as pans
use midgard/validation_machine_tests/constants.{h_a, h_b, zero}
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, exact_rejection_post, inline_field_carriage,
  inline_field_door, native_scripts_step_fixture, next_phase_a_native_state,
  phase_a_native_scan_witness, phase_a_native_step_fixture,
  singleton_field_item_chunk_proof,
}
use midgard/validation_machine_tests/fixtures_script_sources.{
  late_phase_a_native_scan_witness,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// ---------------------------------------------------------------------------
// Late continuation from any authenticated source coordinate
// ---------------------------------------------------------------------------

const inline_zero = 0

const inline_one = 1

const reference = 2

/// A genuine one-chunk proof of `bytes` as item `item_index` of `field_index`.
fn one_chunk_proof(
  field_index: Int,
  item_index: Int,
  bytes: ByteArray,
) -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index,
    item_index,
    total_length: bytearray.length(bytes),
    chunk_index: 0,
    chunk: bytes,
    frontier: [
      validation_merkle_v1.FrontierPeak {
        height: 0,
        hash: bounded_item_v1.hash_chunk(field_index, item_index, 0, bytes),
      },
    ],
    siblings: [],
  }
}

type LateWalk {
  pre: validation_trace_v1.ValidationMachineStateV1,
  work_witness_cbor: ByteArray,
  descriptor: mt.ValidationAuxiliaryWitnessV1,
  entry_pre: validation_trace_v1.ValidationMachineStateV1,
  entry_cbor: ByteArray,
  payload_pre: validation_trace_v1.ValidationMachineStateV1,
  payload_cbor: ByteArray,
  finalize_pre: validation_trace_v1.ValidationMachineStateV1,
  finalize_cbor: ByteArray,
  post: validation_trace_v1.ValidationMachineStateV1,
  /// The target's own chunk at its real coordinate.
  authentic: bounded_item_v1.ChunkProofV1,
  /// The target's bytes relabelled to another script coordinate.
  relabelled: bounded_item_v1.ChunkProofV1,
  /// A genuine chunk of the other script, `any []`.
  other_item: bounded_item_v1.ChunkProofV1,
  /// The successor the other script's bytes would derive at the entry cursor.
  other_successor: validation_trace_v1.ValidationMachineStateV1,
}

/// The late phase-A walk of native script `target` (TimelockStart(1)).
///
/// `mode` places its source: `inline_zero` as inline item 0 of
/// `[target, other]`, `inline_one` as inline item 1 of `[other, target]`,
/// `reference` as the script of reference output 3 with no inline scripts.
/// `other` is `any []`, whose head derives a different successor.
fn late_walk(mode: Int) -> LateWalk {
  let target = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"820401",
  }
  let other = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes: #"820280",
  }
  let base = empty_v1_transaction()
  let tx = MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, validity_interval_start: 5 },
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: if mode == inline_zero {
        [target, other]
      } else if mode == inline_one {
        [other, target]
      } else {
        []
      },
    },
  }
  let target_cbor = encode_midgard_versioned_script(target)
  let other_cbor = encode_midgard_versioned_script(other)
  let target_hash = script_proof_v1.versioned_script_hash(target)
  let length = bytearray.length(target_cbor)
  let origin_kind = if mode == reference {
    1
  } else {
    0
  }
  let source_key = if mode == reference {
    encode_midgard_tx_input(MidgardTxInput { tx_id: h_b, output_index: 3 })
  } else {
    cbor.serialise(mode)
  }
  let field = if mode == reference {
    2
  } else {
    6
  }
  let item = if mode == reference {
    3
  } else {
    mode
  }
  let other_index = if mode == inline_zero {
    1
  } else {
    0
  }
  let target_commitment = bounded_item_v1.from_bytes(field, item, target_cbor)
  let target_source = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    0,
    target_hash,
    length,
    target_commitment,
  )
  let other_source = script_proof_v1.inline_source_leaf_hash(
    other_index,
    0,
    script_proof_v1.versioned_script_hash(other),
    bytearray.length(other_cbor),
    bounded_item_v1.from_bytes(6, other_index, other_cbor),
  )
  let source_index = if mode == inline_one {
    1
  } else {
    0
  }
  let source_frontier = validation_merkle_v1.build_frontier(
    if mode == inline_zero {
      [target_source, other_source]
    } else if mode == inline_one {
      [other_source, target_source]
    } else {
      [target_source]
    },
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    target_hash,
    #"0102",
  )
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier(
    [script_proof_v1.execution_leaf_hash(0, purpose_leaf, target_source, #"")],
  )
  let Pair(pre, work_witness_cbor) = native_scripts_step_fixture(
    tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    0,
    0,
    30,
  )
  let Pair(post, _) = native_scripts_step_fixture(
    tx,
    source_frontier,
    empty_frontier,
    purpose_frontier,
    execution_frontier,
    1,
    0,
    34,
  )
  let payload_offset = length - bytearray.length(target.script_bytes)
  let authentic = one_chunk_proof(field, item, target_cbor)
  let late = fn(stage, cursor, node_count, result) {
    late_phase_a_native_scan_witness(
      tx,
      work_witness_cbor,
      stage,
      length,
      target_commitment,
      cursor,
      node_count,
      result,
    )
  }
  let entry_cbor = late(1, payload_offset, 0, -1)
  let entry_pre = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.PhaseANativeScripts,
    program_counter: 31,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      31,
      entry_cbor,
    ),
  }
  // TimelockStart heads at tag 4, stage 7; `any` heads at tag 2, stage 5.
  let payload_cbor = late(7, payload_offset + 2, 1, -1)
  let payload_pre = next_phase_a_native_state(entry_pre, payload_cbor)
  let finalize_cbor = late(2, length, 1, 1)
  let finalize_pre = next_phase_a_native_state(payload_pre, finalize_cbor)
  LateWalk {
    pre,
    work_witness_cbor,
    descriptor: mt.NativeExecutionDescriptorWitness {
      execution_index: 0,
      language_tag: 0,
      purpose_kind: 0,
      purpose_index: 0,
      script_hash: target_hash,
      subject: #"0102",
      purpose_siblings: [],
      source_index,
      origin_kind,
      source_key,
      script_total_length: length,
      script_item_commitment: target_commitment,
      source_siblings: if mode == reference {
        []
      } else {
        [other_source]
      },
      redeemer_leaf: #"",
      execution_siblings: [],
      first_chunk_proof: Some(authentic),
      signer_peaks: [],
    },
    entry_pre,
    entry_cbor,
    payload_pre,
    payload_cbor,
    finalize_pre,
    finalize_cbor,
    post,
    authentic,
    relabelled: one_chunk_proof(6, other_index, target_cbor),
    other_item: one_chunk_proof(6, other_index, other_cbor),
    other_successor: next_phase_a_native_state(
      entry_pre,
      late(5, payload_offset + 2, 1, -1),
    ),
  }
}

/// Whether the token head admits `successor` from the late entry state with
/// `chunk`, through the monolithic dispatcher and the split validator's
/// semantics. The two must agree, so a disagreement fails the test.
fn late_token_head_admits(
  walk: LateWalk,
  successor: validation_trace_v1.ValidationMachineStateV1,
  chunk: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let transition = mt.ValidationOneStepWitnessV1 {
    work_witness_cbor: walk.entry_cbor,
    claimed_successor: successor,
  }
  let monolith = vm_one_step.verify_one_step_evidence(
    walk.entry_pre,
    mt.ValidationOneStepEvidenceV1 {
      transition,
      auxiliary: mt.NativeScriptTokenWitness {
        chunk_proof: chunk,
        next_chunk_proof: None,
        signer_proof: mt.NoSignerSetProof,
      },
    },
    inline_field_door(),
  )
  let split = pans.verify_phase_a_native_token_head_semantics_v1(
    walk.entry_pre,
    transition,
    chunk,
    None,
  )
  expect monolith == split
  monolith
}

/// The whole late walk of the target from `mode`'s source: NativeScripts
/// enters it, the token head admits exactly the derived successor, and the
/// walk returns to NativeScripts with the execution cursor advanced.
fn late_walk_has_exactly_one_successor(mode: Int) -> Bool {
  let walk = late_walk(mode)
  expect
      mt.NativeExecutionDescriptorWitness {
        source_index,
        origin_kind,
        source_key,
        script_total_length,
        script_item_commitment,
        source_siblings,
        ..
      }
    = walk.descriptor
  let entry = mt.ValidationOneStepWitnessV1 {
    work_witness_cbor: walk.work_witness_cbor,
    claimed_successor: walk.entry_pre,
  }
  let arbitrary = validation_trace_v1.ValidationMachineStateV1 {
    ..walk.payload_pre,
    work_root: zero,
  }
  and {
    vm_one_step.verify_one_step_evidence(
      walk.pre,
      mt.ValidationOneStepEvidenceV1 {
        transition: entry,
        auxiliary: walk.descriptor,
      },
      inline_field_door(),
    ),
    vm_native_scripts.verify_native_scripts_native_semantics_v1(
      walk.pre,
      entry,
      0,
      0,
      0,
      script_proof_v1.versioned_script_hash(
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: #"820401",
        },
      ),
      #"0102",
      [],
      source_index,
      origin_kind,
      source_key,
      script_total_length,
      script_item_commitment,
      source_siblings,
      #"",
      [],
      walk.authentic,
      [],
    ),
    // The derived successor, from the source's own chunk.
    late_token_head_admits(walk, walk.payload_pre, walk.authentic),
    // Another script's genuine chunk derives another successor: refused.
    !late_token_head_admits(walk, walk.other_successor, walk.other_item),
    !late_token_head_admits(walk, walk.other_successor, walk.authentic),
    // The target's bytes under a coordinate its commitment does not hash.
    !late_token_head_admits(walk, walk.payload_pre, walk.relabelled),
    !late_token_head_admits(
      walk,
      exact_rejection_post(walk.entry_pre, reject_invalid_field_type),
      walk.authentic,
    ),
    !late_token_head_admits(walk, arbitrary, walk.authentic),
    pans.verify_phase_a_native_timelock_payload_semantics_v1(
      walk.payload_pre,
      mt.ValidationOneStepWitnessV1 {
        work_witness_cbor: walk.payload_cbor,
        claimed_successor: walk.finalize_pre,
      },
      walk.authentic,
      None,
    ),
    !pans.verify_phase_a_native_timelock_payload_semantics_v1(
      walk.payload_pre,
      mt.ValidationOneStepWitnessV1 {
        work_witness_cbor: walk.payload_cbor,
        claimed_successor: walk.finalize_pre,
      },
      walk.relabelled,
      None,
    ),
    pans.verify_phase_a_native_advance_semantics_v1(
      walk.finalize_pre,
      mt.ValidationOneStepWitnessV1 {
        work_witness_cbor: walk.finalize_cbor,
        claimed_successor: walk.post,
      },
    ),
  }
}

test late_native_source_at_inline_index_one_has_exactly_one_successor() {
  late_walk_has_exactly_one_successor(inline_one)
}

test late_native_reference_source_has_exactly_one_successor() {
  late_walk_has_exactly_one_successor(reference)
}

test late_native_source_at_inline_index_zero_keeps_exactly_one_successor() {
  late_walk_has_exactly_one_successor(inline_zero)
}

// ---------------------------------------------------------------------------
// End of item
// ---------------------------------------------------------------------------

const key28 = #"11111111111111111111111111111111111111111111111111111111"

fn repeat(bytes: ByteArray, n: Int) -> ByteArray {
  if n <= 0 {
    #""
  } else {
    bytearray.concat(bytes, repeat(bytes, n - 1))
  }
}

fn signature_child() -> ByteArray {
  bytearray.concat(#"8200581c", key28)
}

/// `all` declaring 130 children, of which 127 signatures and 2 timelocks are
/// present before the payload ends. The item is 4095 bytes.
fn boundary_payload() -> ByteArray {
  #"82019882"
    |> bytearray.concat(repeat(signature_child(), 127))
    |> bytearray.concat(repeat(#"82041b0000000100000000", 2))
}

/// `all` declaring 131 children, of which 127 signatures and three timelocks
/// are present, then a signature head `8200` whose key hash is missing. The
/// item is 4095 bytes.
fn boundary_signature_head_payload() -> ByteArray {
  #"82019883"
    |> bytearray.concat(repeat(signature_child(), 127))
    |> bytearray.concat(#"82041b0000000100000000")
    |> bytearray.concat(#"8204190100")
    |> bytearray.concat(#"82041818")
    |> bytearray.concat(#"8200")
}

/// `all` declaring 131 children, of which 127 signatures and 3 timelocks are
/// present. The item is 4094 bytes, not a chunk multiple.
fn non_boundary_payload() -> ByteArray {
  #"82019883"
    |> bytearray.concat(repeat(signature_child(), 127))
    |> bytearray.concat(repeat(#"82041a00010000", 3))
}

/// A payload whose item is exactly `item_length` bytes (a 256..65535-byte
/// payload takes a 5-byte versioned-script header): `all` declaring one more
/// child than it holds, cut inside its last signature child.
fn truncated_payload(item_length: Int) -> ByteArray {
  let children_bytes = item_length - 5 - 5
  let whole = children_bytes / 32
  #"820199"
    |> bytearray.concat(bytearray.from_int_big_endian(whole + 1, 2))
    |> bytearray.concat(repeat(signature_child(), whole))
    |> bytearray.concat(bytearray.take(signature_child(), children_bytes % 32))
}

fn tx_with(payload: ByteArray) -> MidgardTransaction {
  let base = empty_v1_transaction()
  MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [
        MidgardVersionedScript {
          language: NativeCardanoScript,
          script_bytes: payload,
        },
      ],
    },
  }
}

fn item_of(payload: ByteArray) -> ByteArray {
  encode_midgard_versioned_script(
    MidgardVersionedScript {
      language: NativeCardanoScript,
      script_bytes: payload,
    },
  )
}

fn frame(
  child_count: Int,
  remaining: Int,
) -> native_script_scan_v1.NativeScriptFrameV1 {
  native_script_scan_v1.NativeScriptFrameV1 {
    tail: #"",
    kind: 1,
    child_count,
    remaining,
    valid_count: 0,
    required: 0,
  }
}

type ScanState {
  tx: MidgardTransaction,
  item: ByteArray,
  frame: native_script_scan_v1.NativeScriptFrameV1,
  node_count: Int,
  pre: validation_trace_v1.ValidationMachineStateV1,
  work: ByteArray,
}

fn scan_witness(
  tx: MidgardTransaction,
  item: ByteArray,
  stage: Int,
  cursor: Int,
  f: native_script_scan_v1.NativeScriptFrameV1,
  node_count: Int,
  result: Int,
) -> ByteArray {
  phase_a_native_scan_witness(
    tx,
    stage,
    1,
    0,
    0,
    bytearray.length(item),
    bounded_item_v1.from_bytes(6, 0, item),
    cursor,
    native_script_scan_v1.hash_frame_v1(f),
    1,
    node_count,
    result,
  )
}

/// The early scan of the single field-6 item `payload`, at `stage` and
/// `cursor`, one frame deep.
fn scan_state(
  payload: ByteArray,
  stage: Int,
  cursor: Int,
  f: native_script_scan_v1.NativeScriptFrameV1,
  node_count: Int,
  result: Int,
  pc_offset: Int,
) -> ScanState {
  let tx = tx_with(payload)
  let item = item_of(payload)
  let Pair(pre0, _) = phase_a_native_step_fixture(tx, h_a)
  let work = scan_witness(tx, item, stage, cursor, f, node_count, result)
  let pc = pre0.program_counter + pc_offset
  ScanState {
    tx,
    item,
    frame: f,
    node_count,
    pre: validation_trace_v1.ValidationMachineStateV1 {
      ..pre0,
      program_counter: pc,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.PhaseANativeScripts,
        pc,
        work,
      ),
    },
    work,
  }
}

fn successor_of(
  state: ScanState,
  stage: Int,
  cursor: Int,
  f: native_script_scan_v1.NativeScriptFrameV1,
  node_count: Int,
  result: Int,
) -> validation_trace_v1.ValidationMachineStateV1 {
  next_phase_a_native_state(
    state.pre,
    scan_witness(state.tx, state.item, stage, cursor, f, node_count, result),
  )
}

/// The end-of-item state at `stage` for the single field-6 item `payload`.
fn end_state(payload: ByteArray, stage: Int) -> ScanState {
  let length = bytearray.length(item_of(payload))
  scan_state(payload, stage, length, frame(2, 1), 5, -1, 3)
}

fn chunk_hashes(item: ByteArray, index: Int, count: Int) -> List<ByteArray> {
  if index >= count {
    []
  } else {
    let offset = index * bounded_item_v1.chunk_bytes
    let length = bounded_item_v1.expected_chunk_length(
      bytearray.length(item),
      index,
    )
    [
      bounded_item_v1.hash_chunk(
        6,
        0,
        index,
        bytearray.slice(item, offset, offset + length - 1),
      ),
      ..chunk_hashes(item, index + 1, count)
    ]
  }
}

/// The genuine proof of the last chunk of the single field-6 item `item`, of
/// at most three chunks.
fn last_chunk_proof(item: ByteArray) -> bounded_item_v1.ChunkProofV1 {
  let length = bytearray.length(item)
  let count = bounded_item_v1.chunk_count(length)
  expect count <= 3
  let hashes = chunk_hashes(item, 0, count)
  let last = count - 1
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: 6,
    item_index: 0,
    total_length: length,
    chunk_index: last,
    chunk: bytearray.drop(item, last * bounded_item_v1.chunk_bytes),
    frontier: validation_merkle_v1.build_frontier(hashes).peaks,
    // With two chunks the last is the right leaf of the one peak; with one
    // or three it is a peak of its own.
    siblings: if count == 2 {
      list.take(hashes, 1)
    } else {
      []
    },
  }
}

/// The chunk the old window demanded at an aligned end: index `chunk_count`,
/// which no proof can open.
fn past_the_end_chunk(item: ByteArray) -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    ..last_chunk_proof(item),
    chunk_index: bounded_item_v1.chunk_count(bytearray.length(item)),
    chunk: #"",
  }
}

fn token_witnesses(item: ByteArray) -> List<mt.ValidationAuxiliaryWitnessV1> {
  [
    mt.NativeScriptTokenWitness {
      chunk_proof: last_chunk_proof(item),
      next_chunk_proof: None,
      signer_proof: mt.NoSignerSetProof,
    },
    mt.NativeScriptTokenWitness {
      chunk_proof: past_the_end_chunk(item),
      next_chunk_proof: None,
      signer_proof: mt.NoSignerSetProof,
    },
  ]
}

/// Every auxiliary witness a prover can present at a stage-1 or payload
/// stage: the token witnesses, and the constructors of the other stages.
fn every_witness(state: ScanState) -> List<mt.ValidationAuxiliaryWitnessV1> {
  [
    mt.NoAuxiliaryWitness,
    mt.TransactionFieldChunkWitness {
      field_index: 6,
      item_index: 0,
      carriage: inline_field_carriage([state.item]),
    },
    mt.NativeScriptFrameWitness { frame: state.frame },
    ..token_witnesses(state.item)
  ]
}

fn monolith_admits(
  state: ScanState,
  successor: validation_trace_v1.ValidationMachineStateV1,
  auxiliary: mt.ValidationAuxiliaryWitnessV1,
) -> Bool {
  pans.verify_phase_a_native_scripts_scan(
    state.pre,
    mt.ValidationOneStepWitnessV1 {
      work_witness_cbor: state.work,
      claimed_successor: successor,
    },
    auxiliary,
    inline_field_door(),
  )
}

fn any_witness_admits(
  state: ScanState,
  successor: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  every_witness(state)
    |> list_any(fn(auxiliary) { monolith_admits(state, successor, auxiliary) })
}

fn list_any(items: List<a>, predicate: fn(a) -> Bool) -> Bool {
  when items is {
    [] -> False
    [item, ..rest] -> predicate(item) || list_any(rest, predicate)
  }
}

fn list_all(items: List<a>, predicate: fn(a) -> Bool) -> Bool {
  when items is {
    [] -> True
    [item, ..rest] -> predicate(item) && list_all(rest, predicate)
  }
}

/// The successors a prover could claim from an end-of-item state other than
/// the derived reject.
fn wrong_successors(
  state: ScanState,
  stage: Int,
) -> List<validation_trace_v1.ValidationMachineStateV1> {
  let length = bytearray.length(state.item)
  [
    exact_rejection_post(state.pre, reject_native_script_invalid),
    successor_of(state, 2, length, state.frame, state.node_count, 1),
    successor_of(state, 2, length, state.frame, state.node_count, 0),
    successor_of(state, stage, length, state.frame, state.node_count + 1, -1),
  ]
}

/// From the end-of-item state at `stage`, the monolithic router admits the
/// invalid-field-type reject with any token witness, and no witness admits
/// any other successor.
fn end_of_item_has_exactly_one_successor(payload: ByteArray, stage: Int) -> Bool {
  let state = end_state(payload, stage)
  let reject = exact_rejection_post(state.pre, reject_invalid_field_type)
  and {
    token_witnesses(state.item)
      |> list_all(fn(auxiliary) { monolith_admits(state, reject, auxiliary) }),
    !monolith_admits(state, reject, mt.NoAuxiliaryWitness),
    !monolith_admits(
      state,
      reject,
      mt.NativeScriptFrameWitness { frame: state.frame },
    ),
    wrong_successors(state, stage)
      |> list_all(fn(successor) { !any_witness_admits(state, successor) }),
  }
}

/// The split validators for `stage` each admit exactly the reject from the
/// end-of-item state, with the chunk the old window demanded.
fn split_end_of_item_has_exactly_one_successor(
  payload: ByteArray,
  stage: Int,
) -> Bool {
  let state = end_state(payload, stage)
  let chunk = past_the_end_chunk(state.item)
  let verdicts = fn(successor) {
    let transition = mt.ValidationOneStepWitnessV1 {
      work_witness_cbor: state.work,
      claimed_successor: successor,
    }
    if stage == 1 {
      [
        pans.verify_phase_a_native_token_head_semantics_v1(
          state.pre,
          transition,
          chunk,
          None,
        ),
      ]
    } else if stage == 3 {
      [
        pans.verify_phase_a_native_signature_empty_payload_semantics_v1(
          state.pre,
          transition,
          chunk,
          None,
          [],
        ),
      ]
    } else if stage == 4 || stage == 5 {
      [
        pans.verify_phase_a_native_all_or_any_container_frame_payload_semantics_v1(
          state.pre,
          transition,
          chunk,
          None,
        ),
        pans.verify_phase_a_native_all_or_any_empty_container_payload_semantics_v1(
          state.pre,
          transition,
          chunk,
          None,
        ),
      ]
    } else if stage == 6 {
      [
        pans.verify_phase_a_native_at_least_container_frame_payload_semantics_v1(
          state.pre,
          transition,
          chunk,
          None,
        ),
        pans.verify_phase_a_native_at_least_empty_container_payload_semantics_v1(
          state.pre,
          transition,
          chunk,
          None,
        ),
      ]
    } else {
      [
        pans.verify_phase_a_native_timelock_payload_semantics_v1(
          state.pre,
          transition,
          chunk,
          None,
        ),
      ]
    }
  }
  and {
    verdicts(exact_rejection_post(state.pre, reject_invalid_field_type))
      |> list_all(fn(admits) { admits }),
    wrong_successors(state, stage)
      |> list_all(
          fn(successor) {
            verdicts(successor) |> list_all(fn(admits) { !admits })
          },
        ),
  }
}

test end_of_item_reached_at_4095_by_a_frame_step() {
  // Stage 7 at 4086 reads the second timelock and moves to stage 2 at 4095;
  // the frame step then returns to stage 1 at 4095, the end of the item.
  let payload = boundary_payload()
  let s7 = scan_state(payload, 7, 4086, frame(130, 2), 130, -1, 1)
  let s2 = scan_state(payload, 2, 4095, frame(130, 2), 130, 0, 2)
  let reached = successor_of(s2, 1, 4095, frame(130, 1), 130, -1)
  let end = scan_state(payload, 1, 4095, frame(130, 1), 130, -1, 3)
  and {
    bytearray.length(s7.item) == 4095,
    successor_of(s7, 2, 4095, frame(130, 2), 130, 0) == s2.pre,
    monolith_admits(
      s7,
      s2.pre,
      mt.NativeScriptTokenWitness {
        chunk_proof: singleton_field_item_chunk_proof(6, s7.item),
        next_chunk_proof: None,
        signer_proof: mt.NoSignerSetProof,
      },
    ),
    monolith_admits(
      s2,
      reached,
      mt.NativeScriptFrameWitness { frame: s2.frame },
    ),
    reached == end.pre,
  }
}

test end_of_item_reached_at_4095_by_a_signature_head() {
  // The head `8200` at 4093 moves to stage 3 at 4095, the end of the item.
  let payload = boundary_signature_head_payload()
  let s1 = scan_state(payload, 1, 4093, frame(131, 1), 131, -1, 2)
  let end = scan_state(payload, 3, 4095, frame(131, 1), 132, -1, 3)
  and {
    bytearray.length(s1.item) == 4095,
    monolith_admits(
      s1,
      end.pre,
      mt.NativeScriptTokenWitness {
        chunk_proof: singleton_field_item_chunk_proof(6, s1.item),
        next_chunk_proof: None,
        signer_proof: mt.NoSignerSetProof,
      },
    ),
  }
}

test aligned_end_of_item_has_exactly_one_successor_at_stage_one() {
  and {
    end_of_item_has_exactly_one_successor(boundary_payload(), 1),
    end_of_item_has_exactly_one_successor(truncated_payload(8190), 1),
    end_of_item_has_exactly_one_successor(truncated_payload(12285), 1),
    split_end_of_item_has_exactly_one_successor(boundary_payload(), 1),
    split_end_of_item_has_exactly_one_successor(truncated_payload(8190), 1),
    split_end_of_item_has_exactly_one_successor(truncated_payload(12285), 1),
  }
}

test aligned_end_of_item_has_exactly_one_successor_at_stage_three() {
  and {
    end_of_item_has_exactly_one_successor(boundary_signature_head_payload(), 3),
    end_of_item_has_exactly_one_successor(truncated_payload(8190), 3),
    end_of_item_has_exactly_one_successor(truncated_payload(12285), 3),
    split_end_of_item_has_exactly_one_successor(
      boundary_signature_head_payload(),
      3,
    ),
    split_end_of_item_has_exactly_one_successor(truncated_payload(8190), 3),
    split_end_of_item_has_exactly_one_successor(truncated_payload(12285), 3),
  }
}

test aligned_end_of_item_has_exactly_one_successor_at_every_payload_stage() {
  [4, 5, 6, 7, 8]
    |> list_all(
        fn(stage) {
          and {
            end_of_item_has_exactly_one_successor(boundary_payload(), stage),
            split_end_of_item_has_exactly_one_successor(
              boundary_payload(),
              stage,
            ),
          }
        },
      )
}

test unaligned_end_of_item_has_exactly_one_successor() {
  let payload = non_boundary_payload()
  and {
    bytearray.length(item_of(payload)) == 4094,
    end_of_item_has_exactly_one_successor(payload, 1),
    end_of_item_has_exactly_one_successor(payload, 3),
  }
}

test aligned_items_have_their_lengths_and_genuine_last_chunks() {
  [
    Pair(boundary_payload(), 4095),
    Pair(boundary_signature_head_payload(), 4095),
    Pair(truncated_payload(8190), 8190),
    Pair(truncated_payload(12285), 12285),
  ]
    |> list_all(
        fn(entry) {
          let Pair(payload, length) = entry
          let item = item_of(payload)
          let commitment = bounded_item_v1.from_bytes(6, 0, item)
          and {
            bytearray.length(item) == length,
            bounded_item_v1.verify_chunk(commitment, last_chunk_proof(item)),
            !bounded_item_v1.verify_chunk(commitment, past_the_end_chunk(item)),
          }
        },
      )
}
