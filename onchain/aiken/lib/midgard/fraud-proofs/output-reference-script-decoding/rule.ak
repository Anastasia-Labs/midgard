use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_256}
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/fraud_proofs/native_script_decoding/engine as scan_engine
use midgard/fraud_proofs/native_tx/codec.{encode_definite_bytes}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_output_scan_v1
use midgard/native_script_scan_v1
use midgard/rejection_reason_v1.{
  OutputReferenceScriptDepthLimit, OutputReferenceScriptMalformed,
  OutputReferenceScriptNodeLimit,
}

pub const outputs_field_index: Int = 2

pub const max_output_bytes: Int = 16384

pub const output_scanning: Int = 0

pub const output_canonical: Int = 1

pub const output_non_canonical: Int = 2

pub const result_pending: Int = -1

pub const result_no_fault: Int = -2

pub const result_malformed: Int = 0

pub const result_node_limit: Int = 1

pub const result_depth_limit: Int = 2

const output_chunk_bytes: Int = 4095

const checkpoint_domain: ByteArray =
  "midgard/fraud-proofs/output-reference-script-decoding/checkpoint-v1"

pub type BoundOutputV1 {
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
  accused_class: Int,
}

pub type OutputDescriptorStateV1 {
  bound: BoundOutputV1,
  item_length: Int,
  item_hash: ByteArray,
  chunk_hashes: List<ByteArray>,
  control: ledger_output_scan_v1.LedgerOutputScanControlV1,
  outcome: Int,
}

pub type ReferenceScriptScanStateV1 {
  bound: BoundOutputV1,
  total_length: Int,
  item_commitment: ByteArray,
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

pub fn bind_output_v1(
  subject: substrate.VerdictSubjectV1,
  output_index: Int,
) -> BoundOutputV1 {
  expect substrate.subject_is_canonical_v1(subject)
  expect output_index >= 0
  let accused_class = if subject.direction == substrate.direction_wrongful_rejection {
    when substrate.rejection_reason_of_v1(subject) is {
      OutputReferenceScriptMalformed { output_index: actual } -> {
        expect actual == output_index
        result_malformed
      }
      OutputReferenceScriptNodeLimit { output_index: actual } -> {
        expect actual == output_index
        result_node_limit
      }
      OutputReferenceScriptDepthLimit { output_index: actual } -> {
        expect actual == output_index
        result_depth_limit
      }
      _ -> fail @"reason outside output-reference-script-decoding"
    }
  } else {
    result_pending
  }
  BoundOutputV1 { subject, output_index, accused_class }
}

fn minimum(left: Int, right: Int) -> Int {
  if left < right {
    left
  } else {
    right
  }
}

fn output_chunk_hashes(
  item: ByteArray,
  offset: Int,
  total: Int,
) -> List<ByteArray> {
  if offset >= total {
    []
  } else {
    let length = minimum(output_chunk_bytes, total - offset)
    [
      builtin.blake2b_256(bytearray.slice(item, offset, offset + length - 1)),
      ..output_chunk_hashes(item, offset + length, total)
    ]
  }
}

pub fn initial_output_scan_v1(
  bound: BoundOutputV1,
  item: ByteArray,
) -> OutputDescriptorStateV1 {
  let item_length = bytearray.length(item)
  expect item_length > 0 && item_length <= max_output_bytes
  OutputDescriptorStateV1 {
    bound,
    item_length,
    item_hash: builtin.blake2b_256(item),
    chunk_hashes: output_chunk_hashes(item, 0, item_length),
    control: ledger_output_scan_v1.initial_control_v1(),
    outcome: output_scanning,
  }
}

pub fn output_state_is_authentic_v1(state: OutputDescriptorStateV1) -> Bool {
  and {
    state.item_length > 0,
    state.item_length <= max_output_bytes,
    bytearray.length(state.item_hash) == 32,
    list.length(state.chunk_hashes) == (
      state.item_length + output_chunk_bytes - 1
    ) / output_chunk_bytes,
    list.all(state.chunk_hashes, fn(hash) { bytearray.length(hash) == 32 }),
    ledger_output_scan_v1.control_is_well_formed(state.control),
    state.outcome >= output_scanning,
    state.outcome <= output_non_canonical,
  }
}

pub fn advance_output_scan_v1(
  state: OutputDescriptorStateV1,
  window: ByteArray,
) -> OutputDescriptorStateV1 {
  expect output_state_is_authentic_v1(state) && state.outcome == output_scanning
  let control = state.control
  let next = if and {
    control.stage == ledger_output_scan_v1.stage_optional_field,
    control.cursor == state.item_length,
  } {
    ledger_output_scan_v1.finish_v1(control, state.item_length)
  } else if control.cursor < state.item_length {
    let chunk_start = control.cursor - control.cursor % output_chunk_bytes
    let chunk_index = control.cursor / output_chunk_bytes
    let current_length = minimum(
      output_chunk_bytes,
      state.item_length - chunk_start,
    )
    expect Some(current_hash) = list.at(state.chunk_hashes, chunk_index)
    expect
      builtin.blake2b_256(bytearray.slice(window, 0, current_length - 1)) == current_hash
    let token_stage = control.stage <= ledger_output_scan_v1.stage_optional_field
    let next_length = minimum(
      output_chunk_bytes,
      state.item_length - chunk_start - current_length,
    )
    let window_length = current_length + if token_stage && next_length > 0 {
      next_length
    } else {
      0
    }
    expect bytearray.length(window) == window_length
    expect _next_hash = if window_length > current_length {
      expect Some(next_hash) = list.at(state.chunk_hashes, chunk_index + 1)
      expect
        builtin.blake2b_256(
          bytearray.slice(window, current_length, window_length - 1),
        ) == next_hash
      next_hash
    } else {
      current_hash
    }
    ledger_output_scan_v1.step_v1(
      control,
      state.item_length,
      window,
      control.cursor - chunk_start,
    )
  } else {
    None
  }
  when next is {
    None -> OutputDescriptorStateV1 { ..state, outcome: output_non_canonical }
    Some(next_control) ->
      OutputDescriptorStateV1 {
        ..state,
        control: next_control,
        outcome: if ledger_output_scan_v1.terminal_is_exact_v1(
          next_control,
          state.item_length,
        ) {
          output_canonical
        } else {
          output_scanning
        },
      }
  }
}

pub fn checkpoint_v1(
  bound: BoundOutputV1,
  total_length: Int,
  item_commitment: ByteArray,
  control_cbor: ByteArray,
  next_expected_script_hash: ByteArray,
) -> ByteArray {
  checkpoint_domain
    |> bytearray.concat(substrate.encode_verdict_subject_v1(bound.subject))
    |> bytearray.concat(cbor.serialise(bound.output_index))
    |> bytearray.concat(cbor.serialise(bound.accused_class))
    |> bytearray.concat(cbor.serialise(total_length))
    |> bytearray.concat(encode_definite_bytes(item_commitment))
    |> bytearray.concat(encode_definite_bytes(control_cbor))
    |> bytearray.concat(encode_definite_bytes(next_expected_script_hash))
    |> blake2b_256
}

fn scan_state_with(
  bound: BoundOutputV1,
  total_length: Int,
  item_commitment: ByteArray,
  control_cbor: ByteArray,
  next_expected_script_hash: ByteArray,
  result_class: Int,
) -> ReferenceScriptScanStateV1 {
  ReferenceScriptScanStateV1 {
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

pub fn bind_reference_script_v1(
  output_state: OutputDescriptorStateV1,
  output_item: ByteArray,
  scan_script_hash: ByteArray,
) -> ReferenceScriptScanStateV1 {
  expect and {
      output_state_is_authentic_v1(output_state),
      output_state.outcome == output_canonical,
      bytearray.length(output_item) == output_state.item_length,
      builtin.blake2b_256(output_item) == output_state.item_hash,
    }
  let offset = output_state.control.reference_script_item_offset
  if output_state.control.reference_script_language == -1 {
    scan_state_with(
      output_state.bound,
      0,
      bounded_item_v1.from_bytes(
        outputs_field_index,
        output_state.bound.output_index,
        #"",
      ),
      #"",
      scan_script_hash,
      result_no_fault,
    )
  } else {
    expect offset >= 0 && offset < output_state.item_length
    let item = bytearray.slice(
      output_item,
      offset,
      output_state.item_length - 1,
    )
    let total_length = bytearray.length(item)
    let commitment = bounded_item_v1.from_bytes(
      outputs_field_index,
      output_state.bound.output_index,
      item,
    )
    when
      native_script_scan_v1.versioned_script_header_v1(item, total_length)
    is {
      None ->
        scan_state_with(
          output_state.bound,
          total_length,
          commitment,
          #"",
          scan_script_hash,
          result_malformed,
        )
      Some(header) ->
        if header.language_tag != 0 {
          scan_state_with(
            output_state.bound,
            total_length,
            commitment,
            #"",
            scan_script_hash,
            result_no_fault,
          )
        } else {
          let control = native_script_scan_v1.initial_structure_control_v1(
            header.payload_offset,
            header.payload_length,
          )
          scan_state_with(
            output_state.bound,
            total_length,
            commitment,
            native_script_scan_v1.encode_structure_control_v1(control),
            scan_script_hash,
            result_pending,
          )
        }
    }
  }
}

pub fn scan_state_is_authentic_v1(state: ReferenceScriptScanStateV1) -> Bool {
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
  state: ReferenceScriptScanStateV1,
  proof: bounded_item_v1.ChunkProofV1,
  expected_index: Int,
) -> Bool {
  and {
    proof.field_index == outputs_field_index,
    proof.item_index == state.bound.output_index,
    proof.total_length == state.total_length,
    proof.chunk_index == expected_index,
    bounded_item_v1.verify_chunk(state.item_commitment, proof),
  }
}

pub fn authenticated_window_v1(
  state: ReferenceScriptScanStateV1,
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

pub fn advanced_scan_state_v1(
  state: ReferenceScriptScanStateV1,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  next_hash: ByteArray,
) -> ReferenceScriptScanStateV1 {
  scan_state_with(
    state.bound,
    state.total_length,
    state.item_commitment,
    native_script_scan_v1.encode_structure_control_v1(control),
    next_hash,
    result_pending,
  )
}

pub fn closed_scan_state_v1(
  state: ReferenceScriptScanStateV1,
  result_class: Int,
  final_hash: ByteArray,
) -> ReferenceScriptScanStateV1 {
  expect result_class == result_no_fault || class_is_fault(result_class)
  scan_state_with(
    state.bound,
    state.total_length,
    state.item_commitment,
    state.control_cbor,
    final_hash,
    result_class,
  )
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

pub fn terminal_contradiction_v1(state: ReferenceScriptScanStateV1) -> Bool {
  expect scan_state_is_authentic_v1(state)
  expect state.result_class != result_pending
  if state.bound.subject.direction == substrate.direction_wrongful_rejection {
    state.result_class != state.bound.accused_class
  } else {
    substrate.terminal_contradiction_v1(
      state.bound.subject,
      class_is_fault(state.result_class),
    )
  }
}
