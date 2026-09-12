//// Canonical ScriptSources frames authenticated by the prepared one-step
//// evidence. Copied fields inherit the canonical predecessor encoding; every
//// changed field is encoded afresh. See the non-output size plan §4.1.

use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/codec.{
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{tx_proof_commitment_v1}
use midgard/ledger_output_scan_v1
use midgard/validation_machine/machine_types.{ValidationOneStepWitnessV1}
use midgard/validation_machine/witness.{decode_frontier_peaks}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub type RawScriptSourcesFrameV1 {
  items: List<Data>,
  item_count: Int,
  prefix: ByteArray,
  stage_offset: Int,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
}

pub fn item_int_v1(frame: RawScriptSourcesFrameV1, index: Int) -> Int {
  expect Some(item) = list.at(frame.items, index)
  builtin.un_i_data(item)
}

pub fn item_bytes_v1(frame: RawScriptSourcesFrameV1, index: Int) -> ByteArray {
  expect Some(item) = list.at(frame.items, index)
  builtin.un_b_data(item)
}

pub fn item_frontier_v1(
  frame: RawScriptSourcesFrameV1,
  index: Int,
) -> List<validation_merkle_v1.FrontierPeak> {
  expect Some(item) = list.at(frame.items, index)
  decode_frontier_peaks(item)
}

pub fn open_frame_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  expected_item_count: Int,
  expected_stage: Int,
) -> RawScriptSourcesFrameV1 {
  expect expected_item_count == 30 || expected_item_count == 31
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == expected_item_count
  expect
      [
        compact, ws, lengths, context, resolved_count, accumulator, signer_count,
        signer_frontier, resolved_peaks, stage,
        ..
      ]
    = items
  let compact_cbor = builtin.un_b_data(compact)
  let witness_set_compact_cbor = builtin.un_b_data(ws)
  let field_preimage_lengths_cbor = builtin.un_b_data(lengths)
  let context_cbor = builtin.un_b_data(context)
  let prefix = encode_definite_array_header(expected_item_count)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(builtin.un_i_data(resolved_count)))
    |> bytearray.concat(encode_definite_bytes(builtin.un_b_data(accumulator)))
    |> bytearray.concat(cbor.serialise(builtin.un_i_data(signer_count)))
    |> bytearray.concat(
        encode_definite_bytes(builtin.un_b_data(signer_frontier)),
      )
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(
          decode_frontier_peaks(resolved_peaks),
        ),
      )
  expect
    builtin.slice_bytearray(
      0,
      bytearray.length(prefix),
      witness.work_witness_cbor,
    ) == prefix
  expect
    tx_proof_commitment_v1(
      pre.source_kind,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == pre.transaction_commitment
  expect
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash
  expect builtin.un_i_data(stage) == expected_stage
  RawScriptSourcesFrameV1 {
    items,
    item_count: expected_item_count,
    prefix,
    stage_offset: bytearray.length(prefix),
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
    context_cbor,
  }
}

pub fn splice_v1(
  witness_cbor: ByteArray,
  offset: Int,
  old_encoding: ByteArray,
  new_encoding: ByteArray,
) -> ByteArray {
  expect offset >= 0
  let suffix_offset = offset + bytearray.length(old_encoding)
  expect suffix_offset <= bytearray.length(witness_cbor)
  expect
    builtin.slice_bytearray(
      offset,
      bytearray.length(old_encoding),
      witness_cbor,
    ) == old_encoding
  builtin.slice_bytearray(0, offset, witness_cbor)
    |> bytearray.concat(new_encoding)
    |> bytearray.concat(
        builtin.slice_bytearray(
          suffix_offset,
          bytearray.length(witness_cbor) - suffix_offset,
          witness_cbor,
        ),
      )
}

// Work-control fields contain integers, definite bytes and definite arrays.
// Count their canonical encodings, so a splice targets a derived field boundary
// rather than any equal byte sequence elsewhere in the authenticated witness.
fn encoded_item_length_v1(item: Data) -> Int {
  builtin.choose_data(
    item,
    fail,
    fail,
    {
      let items = builtin.un_list_data(item)
      bytearray.length(encode_definite_array_header(list.length(items))) + list.foldl(
        items,
        0,
        fn(value, size) { size + encoded_item_length_v1(value) },
      )
    },
    bytearray.length(cbor.serialise(builtin.un_i_data(item))),
    {
      let length = bytearray.length(builtin.un_b_data(item))
      bytearray.length(encode_definite_array_header(length)) + length
    },
  )
}

pub fn item_offset_v1(frame: RawScriptSourcesFrameV1, index: Int) -> Int {
  expect index >= 0 && index <= frame.item_count
  bytearray.length(encode_definite_array_header(frame.item_count)) + list.foldl(
    list.take(frame.items, index),
    0,
    fn(value, size) { size + encoded_item_length_v1(value) },
  )
}

pub fn replace_items_v1(
  frame: RawScriptSourcesFrameV1,
  witness_cbor: ByteArray,
  first_index: Int,
  item_count: Int,
  next_encoding: ByteArray,
) -> ByteArray {
  expect item_count > 0
  let start = item_offset_v1(frame, first_index)
  let end = item_offset_v1(frame, first_index + item_count)
  expect end <= bytearray.length(witness_cbor)
  splice_v1(
    witness_cbor,
    start,
    builtin.slice_bytearray(start, end - start, witness_cbor),
    next_encoding,
  )
}

pub fn replace_stage_v1(
  frame: RawScriptSourcesFrameV1,
  witness_cbor: ByteArray,
  next_stage: Int,
) -> ByteArray {
  expect next_stage >= 0
  splice_v1(
    witness_cbor,
    frame.stage_offset,
    cbor.serialise(item_int_v1(frame, 9)),
    cbor.serialise(next_stage),
  )
}

pub fn append_extension_v1(
  witness_cbor: ByteArray,
  extension: ByteArray,
) -> ByteArray {
  splice_v1(witness_cbor, 0, #"981e", #"981f")
    |> bytearray.concat(encode_definite_bytes(extension))
}

pub fn replace_extension_v1(
  witness_cbor: ByteArray,
  old_extension: ByteArray,
  next_extension: ByteArray,
) -> ByteArray {
  let old = encode_definite_bytes(old_extension)
  expect builtin.slice_bytearray(0, 2, witness_cbor) == #"981f"
  splice_v1(
    witness_cbor,
    bytearray.length(witness_cbor) - bytearray.length(old),
    old,
    encode_definite_bytes(next_extension),
  )
}

pub fn drop_extension_v1(
  witness_cbor: ByteArray,
  old_extension: ByteArray,
) -> ByteArray {
  let old = encode_definite_bytes(old_extension)
  splice_v1(
    witness_cbor,
    bytearray.length(witness_cbor) - bytearray.length(old),
    old,
    #"",
  )
    |> splice_v1(0, #"981f", #"981e")
}

pub fn successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_cbor: ByteArray,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_cbor,
    ),
  }
}

pub const empty_observer_scan_cbor: ByteArray = #"83004000"

pub const empty_mint_fold_cbor: ByteArray = #"8c200040400040000000400080"

pub const initial_output_scan_cbor: ByteArray =
  ledger_output_scan_v1.initial_control_cbor_v1
