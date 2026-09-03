//// Direct, trace-authenticated reverse-use rule for `unusedRedeemer` (`30`).

use aiken/builtin
use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/canonical_cbor_scan_v1
use midgard/fraud_proofs/native_tx/compact.{native_tx_proof_commitment_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/redeemer_item_proof_v1
use midgard/rejection_reason_v1.{UnusedRedeemer, rejection_code_of}
use midgard/script_proof_v1
use midgard/transition_trace
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/witness as vm_witness
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub const maximum_scan_batch: Int = 16

pub type BoundRedeemerV1 {
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  redeemer_index: Int,
}

pub type AuthenticatedRedeemerV1 {
  bound: BoundRedeemerV1,
  purpose_tag: Int,
  pointer_index: Int,
  item_count: Int,
  item_length: Int,
  item_commitment: ByteArray,
  redeemer_leaf: ByteArray,
  purpose_count: Int,
  purpose_peaks: List<validation_merkle_v1.FrontierPeak>,
  execution_count: Int,
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub type AuthenticatedDescriptorV1 {
  bound: BoundRedeemerV1,
  event_key_hash: ByteArray,
  descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
}

pub type AuthenticatedControlV1 {
  bound: BoundRedeemerV1,
  program_counter: Int,
  stage: Int,
  expected_item_control_hash: ByteArray,
  used_redeemer_bitmap: Int,
  current_purpose_kind: Int,
  current_purpose_index: Int,
  redeemer_count: Int,
  purpose_count: Int,
  purpose_peaks: List<validation_merkle_v1.FrontierPeak>,
  execution_count: Int,
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub type AuthenticatedItemHeaderV1 {
  authenticated: AuthenticatedControlV1,
  item_index: Int,
  item_count: Int,
  total_length: Int,
  item_commitment: ByteArray,
  purpose_tag: Int,
  pointer_index: Int,
  data_offset: Int,
  data_length: Int,
}

pub type ReverseScanV1 {
  authenticated: AuthenticatedRedeemerV1,
  cursor: Int,
  used: Bool,
  checkpoint_hash: ByteArray,
}

pub type SelectionOpeningV1 {
  frontier_index: Int,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  purpose_subject: ByteArray,
  purpose_siblings: List<ByteArray>,
  language_tag: Int,
  source_leaf: ByteArray,
  redeemer_leaf: ByteArray,
  execution_siblings: List<ByteArray>,
}

pub type DecisionV1 {
  subject: substrate.VerdictSubjectV1,
  redeemer_index: Int,
  unused: Bool,
}

pub fn redeemer_tag_for_purpose_kind_v1(kind: Int) -> Option<Int> {
  if kind == 0 {
    Some(0)
  } else if kind == 1 {
    Some(1)
  } else if kind == 2 {
    Some(3)
  } else if kind == 3 {
    Some(6)
  } else {
    None
  }
}

pub fn bind_redeemer_v1(
  subject: substrate.VerdictSubjectV1,
  validation_traces_root: ByteArray,
  validation_trace_count: Int,
  redeemer_index: Int,
) -> BoundRedeemerV1 {
  expect and {
      substrate.subject_is_canonical_v1(subject),
      bytearray.length(validation_traces_root) == 32,
      validation_trace_count > 0,
      redeemer_index >= 0,
    }
  expect _reason = if subject.direction == substrate.direction_wrongful_rejection {
    Some(
      substrate.bind_exact_rejection_reason_v1(
        subject,
        UnusedRedeemer { redeemer_index },
      ),
    )
  } else {
    None
  }
  BoundRedeemerV1 {
    subject,
    validation_traces_root,
    validation_trace_count,
    redeemer_index,
  }
}

fn event_key_matches_subject(
  key: ledger_state.EventKey,
  subject: substrate.VerdictSubjectV1,
) -> Bool {
  when key is {
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

fn descriptor_matches_subject(
  descriptor: validation_trace_v1.ValidationTraceDescriptorV1,
  subject: substrate.VerdictSubjectV1,
) -> Bool {
  if subject.direction == substrate.direction_wrongful_acceptance {
    and {
      descriptor.verdict == validation_trace_v1.Accepted,
      descriptor.rejection_code_hash == #"0000000000000000000000000000000000000000000000000000000000000000",
    }
  } else {
    and {
      descriptor.verdict == validation_trace_v1.Rejected,
      descriptor.rejection_code_hash == validation_trace_v1.hash_rejection_code(
        rejection_code_of(substrate.rejection_reason_of_v1(subject)),
      ),
    }
  }
}

pub fn authenticate_descriptor_v1(
  bound: BoundRedeemerV1,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
) -> AuthenticatedDescriptorV1 {
  let descriptor = trace_membership.value
  expect and {
      event_key_matches_subject(trace_membership.key, bound.subject),
      descriptor_matches_subject(descriptor, bound.subject),
      transition_trace.verify_root_membership_with_bytes(
        trace_membership,
        transition_trace.ValidationTracesRootDomain,
        bound.validation_traces_root,
        bound.validation_trace_count,
        cbor.serialise(trace_membership.key),
        cbor.serialise(descriptor),
      ),
    }
  AuthenticatedDescriptorV1 {
    bound,
    event_key_hash: blake2b_256(cbor.serialise(trace_membership.key)),
    descriptor,
  }
}

pub fn authenticate_control_v1(
  authenticated: AuthenticatedDescriptorV1,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: vm_machine_types.ScriptSourcesControlV1,
) -> AuthenticatedControlV1 {
  let bound = authenticated.bound
  let discovery = control.discovery
  let control_cbor = vm_witness.encode_script_sources_discovery_witness(
    control,
    control.stage,
    discovery,
  )
  expect and {
      machine_state.event_key_hash == authenticated.event_key_hash,
      machine_state.transaction_id == bound.subject.transaction_id,
      machine_state.source_kind == if bound.subject.source_kind == substrate.source_kind_accepted {
        validation_trace_v1.Normal
      } else {
        validation_trace_v1.Forced
      },
      machine_state.phase == validation_trace_v1.ScriptSources,
      machine_state.program_counter == trace_proof.state_index,
      machine_state.program_counter < authenticated.descriptor.step_count,
      machine_state.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        machine_state.program_counter,
        control_cbor,
      ),
      trace_proof.state_hash == validation_trace_v1.hash_machine_state(
        machine_state,
      ),
      validation_trace_v1.verify_trace_proof(
        authenticated.descriptor,
        trace_proof,
      ),
      machine_state.transaction_commitment == native_tx_proof_commitment_v1(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
      ),
      machine_state.validation_context_hash == validation_trace_v1.hash_validation_context(
        control.context_cbor,
      ),
      bound.redeemer_index < control.redeemer_count,
      control.purpose_count == discovery.execution_count,
      validation_merkle_v1.frontier_is_well_formed(
        control.purpose_count,
        control.purpose_peaks,
      ),
      validation_merkle_v1.frontier_is_well_formed(
        discovery.execution_count,
        discovery.execution_peaks,
      ),
      control.stage == 12,
      discovery.redeemer_cursor == bound.redeemer_index,
      discovery.redeemer_item_control_hash != #"",
    }
  AuthenticatedControlV1 {
    bound,
    program_counter: machine_state.program_counter,
    stage: control.stage,
    expected_item_control_hash: discovery.redeemer_item_control_hash,
    used_redeemer_bitmap: discovery.used_redeemer_bitmap,
    current_purpose_kind: discovery.current_purpose_kind,
    current_purpose_index: discovery.current_purpose_index,
    redeemer_count: control.redeemer_count,
    purpose_count: control.purpose_count,
    purpose_peaks: control.purpose_peaks,
    execution_count: discovery.execution_count,
    execution_peaks: discovery.execution_peaks,
  }
}

fn bit(index: Int) -> Int {
  if index == 0 {
    1
  } else {
    2 * bit(index - 1)
  }
}

fn chunk_matches(
  item_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  chunk_index: Int,
  proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  and {
    proof.field_index == 8,
    proof.item_index == item_index,
    proof.total_length == total_length,
    proof.chunk_index == chunk_index,
    bounded_item_v1.verify_chunk(item_commitment, proof),
  }
}

fn open_span(
  item_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  start: Int,
  length: Int,
  first: bounded_item_v1.ChunkProofV1,
  second: Option<bounded_item_v1.ChunkProofV1>,
) -> ByteArray {
  expect and {
      length > 0,
      start >= 0,
      start + length <= total_length,
    }
  let first_index = start / bounded_item_v1.chunk_bytes
  let last_index = ( start + length - 1 ) / bounded_item_v1.chunk_bytes
  expect and {
      last_index <= first_index + 1,
      chunk_matches(
        item_index,
        total_length,
        item_commitment,
        first_index,
        first,
      ),
    }
  let bytes = if last_index == first_index {
    expect second == None
    first.chunk
  } else {
    expect Some(next) = second
    expect
      chunk_matches(item_index, total_length, item_commitment, last_index, next)
    first.chunk |> bytearray.concat(next.chunk)
  }
  builtin.slice_bytearray(
    start - first_index * bounded_item_v1.chunk_bytes,
    length,
    bytes,
  )
}

pub fn authenticate_item_header_v1(
  authenticated: AuthenticatedControlV1,
  item_control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> AuthenticatedItemHeaderV1 {
  expect
    redeemer_item_proof_v1.hash_control_v1(item_control) == authenticated.expected_item_control_hash
  expect and {
      item_control.version == 1,
      item_control.mode == 0,
      item_control.stage == 0,
      item_control.item_index == authenticated.bound.redeemer_index,
      item_control.item_count == authenticated.redeemer_count,
      item_control.total_length > 0,
      bytearray.length(item_control.item_commitment) == 32,
      item_control.expected_purpose_tag == -1,
      item_control.expected_pointer_index == -1,
    }
  let header_length = if item_control.total_length < 28 {
    item_control.total_length
  } else {
    28
  }
  let bytes = open_span(
    item_control.item_index,
    item_control.total_length,
    item_control.item_commitment,
    0,
    header_length,
    chunk_proof,
    next_chunk_proof,
  )
  expect Some(outer) = canonical_cbor_scan_v1.head_at_v1(bytes, 0, 4)
  expect outer.value == 4
  expect Some(purpose) = canonical_cbor_scan_v1.head_at_v1(
    bytes,
    outer.next_offset,
    0,
  )
  expect Some(pointer) = canonical_cbor_scan_v1.head_at_v1(
    bytes,
    purpose.next_offset,
    0,
  )
  expect Some(data) = canonical_cbor_scan_v1.head_at_v1(
    bytes,
    pointer.next_offset,
    2,
  )
  expect and {
      purpose.value == 0 || purpose.value == 1 || purpose.value == 3 || purpose.value == 6,
      pointer.value >= 0,
      data.next_offset > 0,
      data.value > 0,
      data.next_offset + data.value < item_control.total_length,
      item_control.total_length - data.next_offset - data.value <= 19,
    }
  AuthenticatedItemHeaderV1 {
    authenticated,
    item_index: item_control.item_index,
    item_count: item_control.item_count,
    total_length: item_control.total_length,
    item_commitment: item_control.item_commitment,
    purpose_tag: purpose.value,
    pointer_index: pointer.value,
    data_offset: data.next_offset,
    data_length: data.value,
  }
}

pub fn authenticate_item_tail_v1(
  header: AuthenticatedItemHeaderV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> AuthenticatedRedeemerV1 {
  let authenticated = header.authenticated
  let tail_start = header.data_offset + header.data_length
  let tail_length = header.total_length - tail_start
  let bytes = open_span(
    header.item_index,
    header.total_length,
    header.item_commitment,
    tail_start,
    tail_length,
    chunk_proof,
    next_chunk_proof,
  )
  expect Some(outer) = canonical_cbor_scan_v1.head_at_v1(bytes, 0, 4)
  expect outer.value == 2
  expect Some(memory) = canonical_cbor_scan_v1.head_at_v1(
    bytes,
    outer.next_offset,
    0,
  )
  expect Some(steps) = canonical_cbor_scan_v1.head_at_v1(
    bytes,
    memory.next_offset,
    0,
  )
  expect and {
      steps.next_offset == bytearray.length(bytes),
      memory.value >= 0,
      steps.value >= 0,
    }
  expect
    if authenticated.bound.subject.direction == substrate.direction_wrongful_acceptance {
      authenticated.used_redeemer_bitmap / bit(header.item_index) % 2 == 0
    } else {
      authenticated.used_redeemer_bitmap / bit(header.item_index) % 2 == 1
    }
  let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
    header.item_index,
    header.item_commitment,
  )
  AuthenticatedRedeemerV1 {
    bound: authenticated.bound,
    purpose_tag: header.purpose_tag,
    pointer_index: header.pointer_index,
    item_count: header.item_count,
    item_length: header.total_length,
    item_commitment: header.item_commitment,
    redeemer_leaf,
    purpose_count: authenticated.purpose_count,
    purpose_peaks: authenticated.purpose_peaks,
    execution_count: authenticated.execution_count,
    execution_peaks: authenticated.execution_peaks,
  }
}

pub fn initial_reverse_scan_v1(
  authenticated: AuthenticatedRedeemerV1,
) -> ReverseScanV1 {
  let checkpoint = #"4d696467617264556e7573656452656465656d65725363616e5631"
    |> bytearray.concat(authenticated.bound.subject.transaction_id)
    |> bytearray.concat(cbor.serialise(authenticated.bound.redeemer_index))
    |> bytearray.concat(authenticated.redeemer_leaf)
    |> bytearray.concat(cbor.serialise(authenticated.purpose_count))
    |> blake2b_256
  ReverseScanV1 {
    authenticated,
    cursor: 0,
    used: False,
    checkpoint_hash: checkpoint,
  }
}

pub fn scan_selection_v1(
  state: ReverseScanV1,
  opening: SelectionOpeningV1,
) -> ReverseScanV1 {
  let authenticated = state.authenticated
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    opening.purpose_kind,
    opening.purpose_index,
    opening.script_hash,
    opening.purpose_subject,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    opening.language_tag,
    purpose_leaf,
    opening.source_leaf,
    opening.redeemer_leaf,
  )
  expect and {
      !state.used,
      state.cursor < authenticated.purpose_count,
      opening.frontier_index == state.cursor,
      validation_merkle_v1.verify_membership(
        authenticated.purpose_count,
        authenticated.purpose_peaks,
        opening.frontier_index,
        purpose_leaf,
        opening.purpose_siblings,
      ),
      validation_merkle_v1.verify_membership(
        authenticated.execution_count,
        authenticated.execution_peaks,
        opening.frontier_index,
        execution_leaf,
        opening.execution_siblings,
      ),
    }
  expect Some(expected_tag) = redeemer_tag_for_purpose_kind_v1(
    opening.purpose_kind,
  )
  let used = and {
    expected_tag == authenticated.purpose_tag,
    opening.purpose_index == authenticated.pointer_index,
    opening.redeemer_leaf == authenticated.redeemer_leaf,
  }
  let next_cursor = state.cursor + 1
  ReverseScanV1 {
    ..state,
    cursor: next_cursor,
    used,
    checkpoint_hash: state.checkpoint_hash
      |> bytearray.concat(cbor.serialise(next_cursor))
      |> bytearray.concat(purpose_leaf)
      |> bytearray.concat(execution_leaf)
      |> bytearray.concat(cbor.serialise(used))
      |> blake2b_256,
  }
}

pub fn reverse_scan_complete_v1(state: ReverseScanV1) -> Bool {
  state.used || state.cursor == state.authenticated.purpose_count
}

pub fn decision_v1(state: ReverseScanV1) -> DecisionV1 {
  expect reverse_scan_complete_v1(state)
  DecisionV1 {
    subject: state.authenticated.bound.subject,
    redeemer_index: state.authenticated.bound.redeemer_index,
    unused: !state.used,
  }
}

pub fn terminal_contradiction_v1(decision: DecisionV1) -> Bool {
  substrate.terminal_contradiction_v1(decision.subject, decision.unused)
}

pub fn encode_reverse_scan_v1(state: ReverseScanV1) -> ByteArray {
  cbor.serialise(state)
}
