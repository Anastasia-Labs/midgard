//// Narrow execution-source binding and native structural scan rules.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_script_decoding/engine as scan_engine
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/native_tx/components.{decode_midgard_tx_input_cbor}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{ExecutionNativeScriptFalse}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine/machine_types.{NativeScriptsControlV1}
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub const result_pending: Int = -1

pub const result_no_fault: Int = -2

pub const result_malformed: Int = 0

pub const result_node_limit: Int = 1

pub const result_depth_limit: Int = 2

const checkpoint_domain: ByteArray =
  "midgard/fraud-proofs/execution-native-script-invalid/checkpoint-v1"

pub type BoundExecutionV1 {
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  execution_index: Int,
  accused_class: Int,
  prior_ledger_root: ByteArray,
  compact_cbor: ByteArray,
}

pub type AuthenticatedExecutionSourceV1 {
  bound: BoundExecutionV1,
  prior_ledger_root: ByteArray,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  language_tag: Int,
  script_hash: ByteArray,
  total_length: Int,
  item_commitment: ByteArray,
  compact_cbor: ByteArray,
}

pub type ExecutionSourceScanStateV1 {
  source: AuthenticatedExecutionSourceV1,
  control_cbor: ByteArray,
  next_expected_script_hash: ByteArray,
  checkpoint_hash: ByteArray,
  result_class: Int,
}

fn class_is_fault(value: Int) -> Bool {
  and {
    value >= result_malformed,
    value <= result_depth_limit,
  }
}

pub fn bind_execution_v1(
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  execution_index: Int,
  prior_ledger_root: ByteArray,
  compact_cbor: ByteArray,
) -> BoundExecutionV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect bytearray.length(validation_traces_root) == 32
  expect validation_trace_count > 0
  expect execution_index >= 0
  expect bytearray.length(prior_ledger_root) == 32
  expect bytearray.length(compact_cbor) > 0
  let accused_class = if subject.direction == substrate.direction_wrongful_rejection {
    when substrate.rejection_reason_of_v1(subject) is {
      ExecutionNativeScriptFalse { execution_index: actual } -> {
        expect actual == execution_index
        result_pending
      }
      _ -> fail @"reason outside execution-native-script-invalid"
    }
  } else {
    result_pending
  }
  BoundExecutionV1 {
    subject,
    validation_traces_root,
    validation_trace_count,
    execution_index,
    accused_class,
    prior_ledger_root,
    compact_cbor,
  }
}

fn event_key_matches_subject(
  event_key: ledger_state.EventKey,
  subject: substrate.VerdictSubjectV1,
) -> Bool {
  when event_key is {
    ledger_state.L2TransactionEventKey { tx_id } -> and {
        subject.source_kind == substrate.source_kind_accepted,
        tx_id == subject.transaction_id,
      }
    ledger_state.ForcedTransactionEventKey { tx_order_id } -> and {
        subject.source_kind == substrate.source_kind_forced,
        cbor.serialise(tx_order_id) == subject.source_key,
      }
    _ -> False
  }
}

pub fn authenticate_execution_source_v1(
  bound: BoundExecutionV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: NativeScriptsControlV1,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  purpose_subject: ByteArray,
  purpose_siblings: List<ByteArray>,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  language_tag: Int,
  total_length: Int,
  item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
  redeemer_leaf: ByteArray,
  execution_siblings: List<ByteArray>,
) -> AuthenticatedExecutionSourceV1 {
  let descriptor = trace_membership.value
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    purpose_kind,
    purpose_index,
    script_hash,
    purpose_subject,
  )
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    language_tag,
    script_hash,
    total_length,
    item_commitment,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    language_tag,
    purpose_leaf,
    source_leaf,
    redeemer_leaf,
  )
  expect and {
      event_key_matches_subject(trace_membership.key, bound.subject),
      machine_state.event_key_hash == blake2b_256(
        cbor.serialise(trace_membership.key),
      ),
      transition_trace.verify_root_membership_with_bytes(
        trace_membership,
        transition_trace.ValidationTracesRootDomain,
        bound.validation_traces_root,
        bound.validation_trace_count,
        cbor.serialise(trace_membership.key),
        cbor.serialise(descriptor),
      ),
      machine_state.transaction_id == bound.subject.transaction_id,
      machine_state.prior_ledger_root == bound.prior_ledger_root,
      control.compact_cbor == bound.compact_cbor,
      if bound.subject.source_kind == substrate.source_kind_accepted {
        and {
          machine_state.source_kind == validation_trace_v1.Normal,
          descriptor.verdict == validation_trace_v1.Accepted,
        }
      } else {
        and {
          machine_state.source_kind == validation_trace_v1.Forced,
          descriptor.verdict == validation_trace_v1.Rejected,
          descriptor.rejection_code_hash == validation_trace_v1.hash_rejection_code(
            rejection_reason_v1.rejection_code_of(
              substrate.rejection_reason_of_v1(bound.subject),
            ),
          ),
        }
      },
      machine_state.phase == validation_trace_v1.NativeScripts,
      machine_state.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.NativeScripts,
        machine_state.program_counter,
        vm_witness.encode_native_scripts_control_v1(control),
      ),
      trace_proof.state_hash == validation_trace_v1.hash_machine_state(
        machine_state,
      ),
      validation_trace_v1.verify_trace_proof(descriptor, trace_proof),
      bound.execution_index < control.execution_count,
      control.execution_count == control.purpose_count,
      validation_merkle_v1.verify_membership(
        control.purpose_count,
        control.purpose_peaks,
        bound.execution_index,
        purpose_leaf,
        purpose_siblings,
      ),
      validation_merkle_v1.verify_membership(
        control.source_count,
        control.source_peaks,
        source_index,
        source_leaf,
        source_siblings,
      ),
      validation_merkle_v1.verify_membership(
        control.execution_count,
        control.execution_peaks,
        bound.execution_index,
        execution_leaf,
        execution_siblings,
      ),
      language_tag == 0,
      redeemer_leaf == #"",
      total_length > 0,
      bytearray.length(item_commitment) == 32,
    }
  AuthenticatedExecutionSourceV1 {
    bound,
    prior_ledger_root: machine_state.prior_ledger_root,
    source_index,
    origin_kind,
    source_key,
    language_tag,
    script_hash,
    total_length,
    item_commitment,
    compact_cbor: control.compact_cbor,
  }
}

pub fn source_item_coordinate(
  source: AuthenticatedExecutionSourceV1,
) -> Pair<Int, Int> {
  if source.origin_kind == 0 {
    Pair(6, source.source_index)
  } else {
    expect source.origin_kind == 1
    let MidgardTxInput { output_index, .. } = decode_midgard_tx_input_cbor(
      source.source_key,
    )
    Pair(2, output_index)
  }
}

pub fn checkpoint_v1(
  source: AuthenticatedExecutionSourceV1,
  control_cbor: ByteArray,
  next_expected_script_hash: ByteArray,
) -> ByteArray {
  checkpoint_domain
    |> bytearray.concat(
        substrate.encode_verdict_subject_v1(source.bound.subject),
      )
    |> bytearray.concat(cbor.serialise(source.bound.execution_index))
    |> bytearray.concat(cbor.serialise(source.source_index))
    |> bytearray.concat(cbor.serialise(source.origin_kind))
    |> bytearray.concat(encode_definite_bytes(source.source_key))
    |> bytearray.concat(cbor.serialise(source.total_length))
    |> bytearray.concat(encode_definite_bytes(source.item_commitment))
    |> bytearray.concat(encode_definite_bytes(control_cbor))
    |> bytearray.concat(encode_definite_bytes(next_expected_script_hash))
    |> blake2b_256
}

fn state_with(
  source: AuthenticatedExecutionSourceV1,
  control_cbor: ByteArray,
  next_expected_script_hash: ByteArray,
  result_class: Int,
) -> ExecutionSourceScanStateV1 {
  ExecutionSourceScanStateV1 {
    source,
    control_cbor,
    next_expected_script_hash,
    checkpoint_hash: checkpoint_v1(
      source,
      control_cbor,
      next_expected_script_hash,
    ),
    result_class,
  }
}

fn chunk_matches(
  source: AuthenticatedExecutionSourceV1,
  proof: bounded_item_v1.ChunkProofV1,
  expected_index: Int,
) -> Bool {
  let Pair(field_index, item_index) = source_item_coordinate(source)
  and {
    proof.field_index == field_index,
    proof.item_index == item_index,
    proof.total_length == source.total_length,
    proof.chunk_index == expected_index,
    bounded_item_v1.verify_chunk(source.item_commitment, proof),
  }
}

pub fn bind_exact_item_v1(
  source: AuthenticatedExecutionSourceV1,
  first_chunk: bounded_item_v1.ChunkProofV1,
  scan_script_hash: ByteArray,
) -> ExecutionSourceScanStateV1 {
  expect chunk_matches(source, first_chunk, 0)
  when
    native_script_scan_v1.versioned_script_header_v1(
      first_chunk.chunk,
      source.total_length,
    )
  is {
    None -> state_with(source, #"", scan_script_hash, result_malformed)
    Some(header) ->
      if header.language_tag != 0 {
        state_with(source, #"", scan_script_hash, result_no_fault)
      } else {
        let control = native_script_scan_v1.initial_structure_control_v1(
          header.payload_offset,
          header.payload_length,
        )
        state_with(
          source,
          native_script_scan_v1.encode_structure_control_v1(control),
          scan_script_hash,
          result_pending,
        )
      }
  }
}

pub fn state_is_authentic_v1(state: ExecutionSourceScanStateV1) -> Bool {
  and {
    bytearray.length(state.source.item_commitment) == 32,
    bytearray.length(state.next_expected_script_hash) == 28,
    state.checkpoint_hash == checkpoint_v1(
      state.source,
      state.control_cbor,
      state.next_expected_script_hash,
    ),
  }
}

pub fn authenticated_window_v1(
  state: ExecutionSourceScanStateV1,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  proof: bounded_item_v1.ChunkProofV1,
  next_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> scan_engine.ScanWindowV1 {
  let index = control.cursor / bounded_item_v1.chunk_bytes
  expect chunk_matches(state.source, proof, index)
  if index + 1 < bounded_item_v1.chunk_count(state.source.total_length) {
    expect Some(next) = next_proof
    expect chunk_matches(state.source, next, index + 1)
    scan_engine.ScanWindowV1 {
      bytes: bytearray.concat(proof.chunk, next.chunk),
      start_offset: index * bounded_item_v1.chunk_bytes,
    }
  } else {
    expect None = next_proof
    scan_engine.ScanWindowV1 {
      bytes: proof.chunk,
      start_offset: index * bounded_item_v1.chunk_bytes,
    }
  }
}

pub fn advanced_state_v1(
  state: ExecutionSourceScanStateV1,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  next_hash: ByteArray,
) -> ExecutionSourceScanStateV1 {
  state_with(
    state.source,
    native_script_scan_v1.encode_structure_control_v1(control),
    next_hash,
    result_pending,
  )
}

pub fn closed_state_v1(
  state: ExecutionSourceScanStateV1,
  result_class: Int,
  final_hash: ByteArray,
) -> ExecutionSourceScanStateV1 {
  expect or {
      result_class == result_no_fault,
      class_is_fault(result_class),
    }
  state_with(state.source, state.control_cbor, final_hash, result_class)
}

pub fn mapped_refusal_class_v1(scan_class: Int) -> Int {
  if scan_class == scan_engine.refusal_class_malformed {
    result_malformed
  } else if scan_class == scan_engine.refusal_class_node_limit {
    result_node_limit
  } else {
    expect scan_class == scan_engine.refusal_class_depth_limit
    result_depth_limit
  }
}

pub fn terminal_contradiction_v1(state: ExecutionSourceScanStateV1) -> Bool {
  expect state_is_authentic_v1(state)
  expect state.result_class != result_pending
  if state.source.bound.subject.direction == substrate.direction_wrongful_rejection {
    state.result_class != state.source.bound.accused_class
  } else {
    substrate.terminal_contradiction_v1(
      state.source.bound.subject,
      class_is_fault(state.result_class),
    )
  }
}

pub fn encode_bound_v1(value: BoundExecutionV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_source_v1(value: AuthenticatedExecutionSourceV1) -> ByteArray {
  cbor.serialise(value)
}

pub fn encode_scan_state_v1(value: ExecutionSourceScanStateV1) -> ByteArray {
  cbor.serialise(value)
}
