//// The late phase-A native walk of one native script, as a pure fixture:
//// every state of the walk from the native execution entry through the
//// finalize, keyed by the authenticated purpose of the execution under
//// evaluation. Shared by the one-step semantic tests and by the
//// transaction-level dispatcher tests.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_tx/components.{
  encode_midgard_tx_input, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTransactionWitnessSet,
  MidgardTxInput, MidgardVersionedScript, NativeCardanoScript,
}
use midgard/script_proof_v1
use midgard/validation_machine/machine_types as mt
use midgard/validation_machine_tests/constants.{h_b}
use midgard/validation_machine_tests/fixtures_phase_a.{
  empty_v1_transaction, native_scripts_step_fixture, next_phase_a_native_state,
}
use midgard/validation_machine_tests/fixtures_script_sources.{
  late_phase_a_native_scan_witness,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

// ---------------------------------------------------------------------------
// Late continuation from any authenticated source coordinate
// ---------------------------------------------------------------------------

pub const inline_zero = 0

pub const inline_one = 1

pub const reference = 2

/// A genuine one-chunk proof of `bytes` as item `item_index` of `field_index`.
pub fn one_chunk_proof(
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

pub type LateWalk {
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
  /// The entry state with the purpose index moved by one.
  rekeyed_entry_pre: validation_trace_v1.ValidationMachineStateV1,
  /// The entry state with the witness-mode purpose key `-1, -1`.
  sentinel_entry_pre: validation_trace_v1.ValidationMachineStateV1,
  /// The finalize state with the purpose index moved by one.
  rekeyed_finalize_pre: validation_trace_v1.ValidationMachineStateV1,
  /// The finalize state of an evaluation that ended false (result 0).
  false_finalize_pre: validation_trace_v1.ValidationMachineStateV1,
  false_finalize_cbor: ByteArray,
}

/// The late phase-A walk of native script `target` (TimelockStart(1)).
///
/// `mode` places its source: `inline_zero` as inline item 0 of
/// `[target, other]`, `inline_one` as inline item 1 of `[other, target]`,
/// `reference` as the script of reference output 3 with no inline scripts.
/// `other` is `any []`, whose head derives a different successor.
pub fn late_walk(mode: Int) -> LateWalk {
  late_walk_keyed(mode, 0, 0)
}

/// `late_walk` for an execution whose authenticated purpose key is
/// `(purpose_kind, purpose_index)`.
pub fn late_walk_keyed(
  mode: Int,
  purpose_kind: Int,
  purpose_index: Int,
) -> LateWalk {
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
    purpose_kind,
    purpose_index,
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
  let late_keyed = fn(stage, cursor, node_count, result, kind, index) {
    late_phase_a_native_scan_witness(
      tx,
      work_witness_cbor,
      stage,
      length,
      target_commitment,
      cursor,
      node_count,
      result,
      kind,
      index,
    )
  }
  let late = fn(stage, cursor, node_count, result) {
    late_keyed(stage, cursor, node_count, result, purpose_kind, purpose_index)
  }
  let entry_state = fn(cbor) {
    validation_trace_v1.ValidationMachineStateV1 {
      ..pre,
      phase: validation_trace_v1.PhaseANativeScripts,
      program_counter: 31,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.PhaseANativeScripts,
        31,
        cbor,
      ),
    }
  }
  let entry_cbor = late(1, payload_offset, 0, -1)
  let entry_pre = entry_state(entry_cbor)
  // TimelockStart heads at tag 4, stage 7; `any` heads at tag 2, stage 5.
  let payload_cbor = late(7, payload_offset + 2, 1, -1)
  let payload_pre = next_phase_a_native_state(entry_pre, payload_cbor)
  let finalize_cbor = late(2, length, 1, 1)
  let finalize_pre = next_phase_a_native_state(payload_pre, finalize_cbor)
  let false_finalize_cbor = late(2, length, 1, 0)
  LateWalk {
    pre,
    work_witness_cbor,
    descriptor: mt.NativeExecutionDescriptorWitness {
      execution_index: 0,
      language_tag: 0,
      purpose_kind,
      purpose_index,
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
    rekeyed_entry_pre: entry_state(
      late_keyed(1, payload_offset, 0, -1, purpose_kind, purpose_index + 1),
    ),
    sentinel_entry_pre: entry_state(
      late_keyed(1, payload_offset, 0, -1, -1, -1),
    ),
    rekeyed_finalize_pre: next_phase_a_native_state(
      payload_pre,
      late_keyed(2, length, 1, 1, purpose_kind, purpose_index + 1),
    ),
    false_finalize_pre: next_phase_a_native_state(
      payload_pre,
      false_finalize_cbor,
    ),
    false_finalize_cbor,
  }
}
