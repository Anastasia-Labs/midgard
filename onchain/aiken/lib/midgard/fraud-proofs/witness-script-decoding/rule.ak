//// Narrow state and semantic rules for field-6 witness-script decoding.

use aiken/cbor
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_script_decoding/engine as scan_engine
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{
  WitnessNativeScriptDepthLimit, WitnessNativeScriptMalformed,
  WitnessNativeScriptNodeLimit, WitnessScriptHeaderMalformed,
}

pub const field_index: Int = 6

pub const result_pending: Int = -1

pub const result_no_fault: Int = -2

pub const result_header_malformed: Int = 0

pub const result_native_malformed: Int = 1

pub const result_node_limit: Int = 2

pub const result_depth_limit: Int = 3

const checkpoint_domain: ByteArray =
  "midgard/fraud-proofs/witness-script-decoding/checkpoint-v1"

pub type BoundWitnessScriptV1 {
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  script_index: Int,
  accused_class: Int,
}

pub type WitnessScriptScanStateV1 {
  bound: BoundWitnessScriptV1,
  total_length: Int,
  item_commitment: ByteArray,
  control_cbor: ByteArray,
  next_expected_script_hash: ByteArray,
  checkpoint_hash: ByteArray,
  result_class: Int,
}

fn class_is_fault(value: Int) -> Bool {
  and {
    value >= result_header_malformed,
    value <= result_depth_limit,
  }
}

pub fn bind_subject_v1(
  subject: substrate.VerdictSubjectV1,
  witness_set_hash: ByteArray,
  script_index: Int,
) -> BoundWitnessScriptV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect bytearray.length(witness_set_hash) == 32
  expect script_index >= 0
  let accused_class = if subject.direction == substrate.direction_wrongful_rejection {
    when substrate.rejection_reason_of_v1(subject) is {
      WitnessScriptHeaderMalformed { script_index: actual } -> {
        expect actual == script_index
        result_header_malformed
      }
      WitnessNativeScriptMalformed { script_index: actual } -> {
        expect actual == script_index
        result_native_malformed
      }
      WitnessNativeScriptNodeLimit { script_index: actual } -> {
        expect actual == script_index
        result_node_limit
      }
      WitnessNativeScriptDepthLimit { script_index: actual } -> {
        expect actual == script_index
        result_depth_limit
      }
      _ -> fail @"reason outside witness-script-decoding"
    }
  } else {
    result_pending
  }
  BoundWitnessScriptV1 {
    subject,
    witness_set_hash,
    script_index,
    accused_class,
  }
}

pub fn checkpoint_v1(
  bound: BoundWitnessScriptV1,
  total_length: Int,
  item_commitment: ByteArray,
  control_cbor: ByteArray,
  next_expected_script_hash: ByteArray,
) -> ByteArray {
  checkpoint_domain
    |> bytearray.concat(substrate.encode_verdict_subject_v1(bound.subject))
    |> bytearray.concat(cbor.serialise(bound.script_index))
    |> bytearray.concat(cbor.serialise(total_length))
    |> bytearray.concat(encode_definite_bytes(item_commitment))
    |> bytearray.concat(encode_definite_bytes(control_cbor))
    |> bytearray.concat(encode_definite_bytes(next_expected_script_hash))
    |> blake2b_256
}

fn state_with(
  bound: BoundWitnessScriptV1,
  total_length: Int,
  item_commitment: ByteArray,
  control_cbor: ByteArray,
  next_expected_script_hash: ByteArray,
  result_class: Int,
) -> WitnessScriptScanStateV1 {
  WitnessScriptScanStateV1 {
    bound,
    total_length,
    item_commitment,
    control_cbor,
    next_expected_script_hash,
    checkpoint_hash: checkpoint_v1(
      bound,
      total_length,
      item_commitment,
      control_cbor,
      next_expected_script_hash,
    ),
    result_class,
  }
}

pub fn authenticate_item_v1(
  bound: BoundWitnessScriptV1,
  item: ByteArray,
  scan_script_hash: ByteArray,
) -> WitnessScriptScanStateV1 {
  let total_length = bytearray.length(item)
  let item_commitment = bounded_item_v1.from_bytes(
    field_index,
    bound.script_index,
    item,
  )
  when native_script_scan_v1.versioned_script_header_v1(item, total_length) is {
    None ->
      state_with(
        bound,
        total_length,
        item_commitment,
        #"",
        scan_script_hash,
        result_header_malformed,
      )
    Some(header) ->
      if header.language_tag != 0 {
        state_with(
          bound,
          total_length,
          item_commitment,
          #"",
          scan_script_hash,
          result_no_fault,
        )
      } else {
        let control = native_script_scan_v1.initial_structure_control_v1(
          header.payload_offset,
          header.payload_length,
        )
        state_with(
          bound,
          total_length,
          item_commitment,
          native_script_scan_v1.encode_structure_control_v1(control),
          scan_script_hash,
          result_pending,
        )
      }
  }
}

pub fn state_is_authentic_v1(state: WitnessScriptScanStateV1) -> Bool {
  and {
    state.total_length >= 0,
    bytearray.length(state.item_commitment) == 32,
    bytearray.length(state.next_expected_script_hash) == 28,
    state.checkpoint_hash == checkpoint_v1(
      state.bound,
      state.total_length,
      state.item_commitment,
      state.control_cbor,
      state.next_expected_script_hash,
    ),
  }
}

fn chunk_matches(
  state: WitnessScriptScanStateV1,
  proof: bounded_item_v1.ChunkProofV1,
  expected_index: Int,
) -> Bool {
  and {
    proof.field_index == field_index,
    proof.item_index == state.bound.script_index,
    proof.total_length == state.total_length,
    proof.chunk_index == expected_index,
    bounded_item_v1.verify_chunk(state.item_commitment, proof),
  }
}

pub fn authenticated_window_v1(
  state: WitnessScriptScanStateV1,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  proof: bounded_item_v1.ChunkProofV1,
  next_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> scan_engine.ScanWindowV1 {
  let index = control.cursor / bounded_item_v1.chunk_bytes
  expect chunk_matches(state, proof, index)
  if index + 1 < bounded_item_v1.chunk_count(state.total_length) {
    expect Some(next) = next_proof
    expect chunk_matches(state, next, index + 1)
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
  state: WitnessScriptScanStateV1,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  next_expected_script_hash: ByteArray,
) -> WitnessScriptScanStateV1 {
  state_with(
    state.bound,
    state.total_length,
    state.item_commitment,
    native_script_scan_v1.encode_structure_control_v1(control),
    next_expected_script_hash,
    result_pending,
  )
}

pub fn closed_state_v1(
  state: WitnessScriptScanStateV1,
  result_class: Int,
  final_script_hash: ByteArray,
) -> WitnessScriptScanStateV1 {
  expect or {
      result_class == result_no_fault,
      class_is_fault(result_class),
    }
  state_with(
    state.bound,
    state.total_length,
    state.item_commitment,
    state.control_cbor,
    final_script_hash,
    result_class,
  )
}

pub fn mapped_refusal_class_v1(scan_class: Int) -> Int {
  if scan_class == scan_engine.refusal_class_malformed {
    result_native_malformed
  } else if scan_class == scan_engine.refusal_class_node_limit {
    result_node_limit
  } else {
    expect scan_class == scan_engine.refusal_class_depth_limit
    result_depth_limit
  }
}

pub fn terminal_contradiction_v1(state: WitnessScriptScanStateV1) -> Bool {
  expect state_is_authentic_v1(state)
  expect state.result_class != result_pending
  let fault_holds = class_is_fault(state.result_class)
  if state.bound.subject.direction == substrate.direction_wrongful_rejection {
    expect state.bound.accused_class >= result_header_malformed
    // The exact accused predicate is false only when the authenticated decoder
    // result is a different class or no decoding fault at all.
    state.result_class != state.bound.accused_class
  } else {
    substrate.terminal_contradiction_v1(state.bound.subject, fault_holds)
  }
}

pub fn encode_bound_v1(bound: BoundWitnessScriptV1) -> ByteArray {
  cbor.serialise(bound)
}

pub fn encode_scan_state_v1(state: WitnessScriptScanStateV1) -> ByteArray {
  cbor.serialise(state)
}
