use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/bounded_item_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/native_tx/codec.{encode_definite_array_header}
use midgard/fraud_proofs/native_tx/types.{
  MidgardCredential, PubKeyCredential, ScriptCredential,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_scan_v1
use midgard/ledger_output_v1
use midgard/ledger_output_value_v1
use midgard/native_script_scan_v1
use midgard/validation_merkle_v1

pub const version: Int = 1

pub const output_field_index: Int = 2

pub const stage_structure: Int = 0

pub const stage_value_fold: Int = 1

pub const stage_datum_traversal: Int = 2

pub const stage_reference_script_commitment: Int = 3

pub const stage_script_hash: Int = 4

pub const stage_native_script: Int = 5

pub const stage_terminal: Int = 6

/// The once-verified authenticated output span whose bytes span-consuming
/// stage steps later bind by digest instead of re-running the chunk merkle
/// verification.
pub type LedgerOutputSpanWindowV1 {
  start: Int,
  length: Int,
  digest: ByteArray,
}

pub type LedgerOutputProofControlV1 {
  version: Int,
  stage: Int,
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  output_scan: ledger_output_scan_v1.LedgerOutputScanControlV1,
  value: Option<ledger_output_value_v1.LedgerOutputValueControlV1>,
  datum: Option<cek_data_traverse_v1.DataTraverseControlV1>,
  reference_script_count: Int,
  reference_script_peaks: List<validation_merkle_v1.FrontierPeak>,
  script_hash: Option<blake2b_224_trace_v1.Blake2b224TraceControlV1>,
  native_script: Option<native_script_scan_v1.NativeScriptStructureControlV1>,
  span_window: Option<LedgerOutputSpanWindowV1>,
  scan_facts_fact: Option<ByteArray>,
  reference_script_fact: Option<ByteArray>,
  datum_summary_fact: Option<ByteArray>,
  value_summary_fact: Option<ByteArray>,
}

pub type LedgerOutputProofWitnessV1 {
  LedgerOutputProofNoWitness
  LedgerOutputProofChunks {
    chunk_proof: bounded_item_v1.ChunkProofV1,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  }
  LedgerOutputProofValue {
    asset_index: Int,
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    siblings: List<ByteArray>,
    previous: Option<ledger_output_value_v1.LedgerOutputValueHeadV1>,
  }
  LedgerOutputProofDatum {
    action: cek_data_traverse_v1.DataTraverseActionV1,
    window: Option<ByteArray>,
  }
  LedgerOutputProofNativeFrame {
    frame: native_script_scan_v1.NativeScriptFrameV1,
  }
  /// The window a span attach records is not a witness choice: its start is
  /// the span the pre-control's consuming stage reads next and its length is
  /// the maximal `min(chunk_bytes, total_length - start)`, so the chunk
  /// proofs are the only witness data.
  LedgerOutputProofSpanAttach {
    chunk_proof: bounded_item_v1.ChunkProofV1,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  }
  LedgerOutputProofWindow { bytes: ByteArray }
}

pub type LedgerOutputProofStepResultV1 {
  LedgerOutputProofAdvanced { control: LedgerOutputProofControlV1 }
  LedgerOutputProofInvalidOutput
  LedgerOutputProofInvalidReferenceScript
  LedgerOutputProofNativeScriptNodeLimit
  LedgerOutputProofNativeScriptDepthLimit
}

fn script_hash_is_well_formed(control: LedgerOutputProofControlV1) -> Bool {
  when control.script_hash is {
    None -> False
    Some(hash_control) -> and {
        blake2b_224_trace_v1.control_is_well_formed(hash_control),
        hash_control.total_length == control.output_scan.reference_script_length + 1,
      }
  }
}

fn script_hash_is_terminal(control: LedgerOutputProofControlV1) -> Bool {
  when control.script_hash is {
    None -> False
    Some(hash_control) -> and {
        script_hash_is_well_formed(control),
        blake2b_224_trace_v1.digest_v1(hash_control) != None,
      }
  }
}

fn native_script_is_well_formed(control: LedgerOutputProofControlV1) -> Bool {
  when control.native_script is {
    None -> False
    Some(native_control) -> and {
        native_script_scan_v1.structure_control_is_well_formed_v1(
          native_control,
        ),
        native_control.start_offset == control.output_scan.reference_script_offset,
        native_control.end_offset == control.output_scan.reference_script_offset + control.output_scan.reference_script_length,
      }
  }
}

fn native_script_is_terminal(control: LedgerOutputProofControlV1) -> Bool {
  when control.native_script is {
    None -> False
    Some(native_control) -> and {
        native_script_is_well_formed(control),
        native_script_scan_v1.structure_terminal_is_exact_v1(native_control),
      }
  }
}

/// A descriptor fact commitment may only be recorded on a terminal-stage
/// control: the fact-attachment steps run after the traversal completes.
fn fact_is_well_formed(stage: Int, fact: Option<ByteArray>) -> Bool {
  when fact is {
    None -> True
    Some(commitment) -> and {
        stage == stage_terminal,
        bytearray.length(commitment) == 32,
      }
  }
}

pub fn control_is_well_formed(control: LedgerOutputProofControlV1) -> Bool {
  let scan_terminal = ledger_output_scan_v1.terminal_is_exact_v1(
    control.output_scan,
    control.total_length,
  )
  let value_is_well_formed = when control.value is {
    None -> False
    Some(value_control) -> and {
        ledger_output_value_v1.control_is_well_formed(value_control),
        value_control.asset_remaining <= control.output_scan.asset_count,
      }
  }
  let value_is_terminal = when control.value is {
    None -> False
    Some(value_control) -> and {
        value_is_well_formed,
        ledger_output_value_v1.finalize_v1(value_control) != None,
      }
  }
  let datum_present = control.output_scan.datum_offset != -1
  let datum_length = control.output_scan.datum_length
  let datum_is_well_formed = when control.datum is {
    None -> False
    Some(datum_control) -> and {
        cek_data_traverse_v1.control_is_well_formed(datum_control),
        datum_control.source_start == control.output_scan.datum_offset,
        datum_control.source_length == datum_length,
      }
  }
  let datum_is_terminal = when control.datum is {
    None -> False
    Some(datum_control) -> and {
        datum_is_well_formed,
        datum_control.stage == cek_data_traverse_v1.stage_terminal,
        cek_data_traverse_v1.finalize_v1(datum_control) != None,
      }
  }
  let datum_is_complete = if datum_present {
    datum_is_terminal
  } else {
    control.datum == None
  }
  let reference_language = control.output_scan.reference_script_language
  let reference_item_length = control.total_length - control.output_scan.reference_script_item_offset
  let reference_chunk_count = bounded_item_v1.chunk_count(reference_item_length)
  let reference_frontier_is_complete = and {
    reference_language != -1,
    reference_item_length > 0,
    control.reference_script_count == reference_chunk_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.reference_script_count,
      control.reference_script_peaks,
    ),
  }
  and {
    control.version == version,
    control.stage >= stage_structure,
    control.stage <= stage_terminal,
    control.output_index >= 0,
    control.total_length > 0,
    bytearray.length(control.item_commitment) == 32,
    when control.span_window is {
      None -> True
      Some(window) -> and {
          control.stage >= stage_datum_traversal,
          window.start >= 0,
          window.length > 0,
          window.length <= bounded_item_v1.chunk_bytes,
          window.start + window.length <= control.total_length,
          bytearray.length(window.digest) == 32,
        }
    },
    fact_is_well_formed(control.stage, control.scan_facts_fact),
    fact_is_well_formed(control.stage, control.reference_script_fact),
    fact_is_well_formed(control.stage, control.datum_summary_fact),
    fact_is_well_formed(control.stage, control.value_summary_fact),
    ledger_output_scan_v1.control_is_well_formed(control.output_scan),
    control.output_scan.cursor <= control.total_length,
    if control.stage == stage_structure {
      and {
        control.value == None,
        control.datum == None,
        control.reference_script_count == 0,
        control.reference_script_peaks == [],
        control.script_hash == None,
        control.native_script == None,
      }
    } else if !scan_terminal {
      False
    } else if control.stage == stage_value_fold {
      and {
        value_is_well_formed,
        control.datum == None,
        control.reference_script_count == 0,
        control.reference_script_peaks == [],
        control.script_hash == None,
        control.native_script == None,
      }
    } else if !value_is_terminal {
      False
    } else if and {
      !datum_present,
      control.datum != None,
    } {
      False
    } else if control.stage == stage_datum_traversal {
      and {
        datum_present,
        datum_length > 0,
        datum_is_well_formed,
        control.reference_script_count == 0,
        control.reference_script_peaks == [],
        control.script_hash == None,
        control.native_script == None,
      }
    } else if !datum_is_complete {
      False
    } else if reference_language == -1 {
      and {
        control.stage == stage_terminal,
        control.reference_script_count == 0,
        control.reference_script_peaks == [],
        control.script_hash == None,
        control.native_script == None,
      }
    } else if or {
      reference_item_length <= 0,
      control.reference_script_count < 0,
      control.reference_script_count > reference_chunk_count,
      !validation_merkle_v1.frontier_is_well_formed(
        control.reference_script_count,
        control.reference_script_peaks,
      ),
    } {
      False
    } else if control.stage == stage_reference_script_commitment {
      and {
        control.script_hash == None,
        control.native_script == None,
      }
    } else if or {
      !reference_frontier_is_complete,
      !script_hash_is_well_formed(control),
    } {
      False
    } else if control.stage == stage_script_hash {
      control.native_script == None
    } else if and {
      reference_language == 0,
      control.stage == stage_native_script,
    } {
      and {
        script_hash_is_terminal(control),
        native_script_is_well_formed(control),
      }
    } else {
      and {
        control.stage == stage_terminal,
        script_hash_is_terminal(control),
        if reference_language == 0 {
          native_script_is_terminal(control)
        } else {
          control.native_script == None
        },
      }
    },
  }
}

pub fn initial_control_v1(
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
) -> LedgerOutputProofControlV1 {
  let control = LedgerOutputProofControlV1 {
    version,
    stage: stage_structure,
    output_index,
    total_length,
    item_commitment,
    output_scan: ledger_output_scan_v1.initial_control_v1(),
    value: None,
    datum: None,
    reference_script_count: 0,
    reference_script_peaks: validation_merkle_v1.empty_frontier(),
    script_hash: None,
    native_script: None,
    span_window: None,
    scan_facts_fact: None,
    reference_script_fact: None,
    datum_summary_fact: None,
    value_summary_fact: None,
  }
  expect control_is_well_formed(control)
  control
}

/// The initial stage has fixed empty optional sub-controls.
pub fn encode_initial_control_v1(
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
) -> ByteArray {
  expect output_index >= 0
  expect total_length > 0
  expect bytearray.length(item_commitment) == 32
  #"91"
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(stage_structure))
    |> bytearray.concat(cbor.serialise(output_index))
    |> bytearray.concat(cbor.serialise(total_length))
    |> bytearray.concat(cbor.serialise(builtin.b_data(item_commitment)))
    |> bytearray.concat(ledger_output_scan_v1.initial_control_cbor_v1)
    |> bytearray.concat(#"d87a80d87a800080d87a80d87a80")
    |> bytearray.concat(#"d87a80d87a80d87a80d87a80d87a80")
}

fn encode_optional_script_hash(
  control: Option<blake2b_224_trace_v1.Blake2b224TraceControlV1>,
) -> ByteArray {
  when control is {
    None -> #"d87a80"
    Some(value) ->
      blake2b_224_trace_v1.encode_control_v1(value)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

fn encode_optional_datum(
  control: Option<cek_data_traverse_v1.DataTraverseControlV1>,
) -> ByteArray {
  when control is {
    None -> #"d87a80"
    Some(value) ->
      cek_data_traverse_v1.encode_control_v1(value)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

fn encode_optional_value(
  control: Option<ledger_output_value_v1.LedgerOutputValueControlV1>,
) -> ByteArray {
  when control is {
    None -> #"d87a80"
    Some(value) ->
      ledger_output_value_v1.encode_control_v1(value)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

fn encode_optional_native_script(
  control: Option<native_script_scan_v1.NativeScriptStructureControlV1>,
) -> ByteArray {
  when control is {
    None -> #"d87a80"
    Some(value) ->
      native_script_scan_v1.encode_structure_control_v1(value)
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

fn optional_script_hash_from_data(
  data: Data,
) -> Option<blake2b_224_trace_v1.Blake2b224TraceControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(blake2b_224_trace_v1.control_from_data_v1(value))
  }
}

fn optional_datum_from_data(
  data: Data,
) -> Option<cek_data_traverse_v1.DataTraverseControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(cek_data_traverse_v1.control_from_data_v1(value))
  }
}

fn optional_value_from_data(
  data: Data,
) -> Option<ledger_output_value_v1.LedgerOutputValueControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(ledger_output_value_v1.control_from_data_v1(value))
  }
}

fn optional_native_script_from_data(
  data: Data,
) -> Option<native_script_scan_v1.NativeScriptStructureControlV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(native_script_scan_v1.structure_control_from_data_v1(value))
  }
}

fn decode_peak_items(
  items: List<Data>,
) -> List<validation_merkle_v1.FrontierPeak> {
  when items is {
    [] -> []
    [item, ..rest] -> {
      expect [height_data, hash_data] = builtin.un_list_data(item)
      [
        validation_merkle_v1.FrontierPeak {
          height: builtin.un_i_data(height_data),
          hash: builtin.un_b_data(hash_data),
        },
        ..decode_peak_items(rest)
      ]
    }
  }
}

fn encode_optional_span_window(
  window: Option<LedgerOutputSpanWindowV1>,
) -> ByteArray {
  when window is {
    None -> #"d87a80"
    Some(record) ->
      #"83"
        |> bytearray.concat(cbor.serialise(record.start))
        |> bytearray.concat(cbor.serialise(record.length))
        |> bytearray.concat(cbor.serialise(builtin.b_data(record.digest)))
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

fn encode_optional_fact(fact: Option<ByteArray>) -> ByteArray {
  when fact is {
    None -> #"d87a80"
    Some(commitment) ->
      cbor.serialise(builtin.b_data(commitment))
        |> bytearray.concat(#"ff")
        |> bytearray.concat(#"d8799f", _)
  }
}

pub fn encode_control_v1(control: LedgerOutputProofControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(17)
    |> bytearray.concat(cbor.serialise(version))
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.output_index))
    |> bytearray.concat(cbor.serialise(control.total_length))
    |> bytearray.concat(cbor.serialise(builtin.b_data(control.item_commitment)))
    |> bytearray.concat(
        ledger_output_scan_v1.encode_control_v1(control.output_scan),
      )
    |> bytearray.concat(encode_optional_value(control.value))
    |> bytearray.concat(encode_optional_datum(control.datum))
    |> bytearray.concat(cbor.serialise(control.reference_script_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.reference_script_peaks),
      )
    |> bytearray.concat(encode_optional_script_hash(control.script_hash))
    |> bytearray.concat(encode_optional_native_script(control.native_script))
    |> bytearray.concat(encode_optional_span_window(control.span_window))
    |> bytearray.concat(encode_optional_fact(control.scan_facts_fact))
    |> bytearray.concat(encode_optional_fact(control.reference_script_fact))
    |> bytearray.concat(encode_optional_fact(control.datum_summary_fact))
    |> bytearray.concat(encode_optional_fact(control.value_summary_fact))
}

fn optional_span_window_from_data(
  data: Data,
) -> Option<LedgerOutputSpanWindowV1> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [record] = fields
    expect [start_data, length_data, digest_data] = builtin.un_list_data(record)
    Some(
      LedgerOutputSpanWindowV1 {
        start: builtin.un_i_data(start_data),
        length: builtin.un_i_data(length_data),
        digest: builtin.un_b_data(digest_data),
      },
    )
  }
}

fn optional_fact_from_data(data: Data) -> Option<ByteArray> {
  let Pair(index, fields) = builtin.un_constr_data(data)
  if index == 1 {
    expect fields == []
    None
  } else {
    expect index == 0
    expect [value] = fields
    Some(builtin.un_b_data(value))
  }
}

pub fn decode_control_v1(control_cbor: ByteArray) -> LedgerOutputProofControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
      [
        version_data, stage_data, output_index_data, total_length_data,
        item_commitment_data, output_scan_data, value_data, datum_data,
        reference_script_count_data, reference_script_peaks_data,
        script_hash_data, native_script_data, span_window_data,
        scan_facts_fact_data, reference_script_fact_data,
        datum_summary_fact_data, value_summary_fact_data,
      ]
    = builtin.un_list_data(data)
  let control = LedgerOutputProofControlV1 {
    version: builtin.un_i_data(version_data),
    stage: builtin.un_i_data(stage_data),
    output_index: builtin.un_i_data(output_index_data),
    total_length: builtin.un_i_data(total_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    output_scan: ledger_output_scan_v1.control_from_data_v1(output_scan_data),
    value: optional_value_from_data(value_data),
    datum: optional_datum_from_data(datum_data),
    reference_script_count: builtin.un_i_data(reference_script_count_data),
    reference_script_peaks: reference_script_peaks_data
      |> builtin.un_list_data
      |> decode_peak_items,
    script_hash: optional_script_hash_from_data(script_hash_data),
    native_script: optional_native_script_from_data(native_script_data),
    span_window: optional_span_window_from_data(span_window_data),
    scan_facts_fact: optional_fact_from_data(scan_facts_fact_data),
    reference_script_fact: optional_fact_from_data(reference_script_fact_data),
    datum_summary_fact: optional_fact_from_data(datum_summary_fact_data),
    value_summary_fact: optional_fact_from_data(value_summary_fact_data),
  }
  expect control_is_well_formed(control)
  expect encode_control_v1(control) == control_cbor
  control
}

fn output_chunk_proof_matches(
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  proof: bounded_item_v1.ChunkProofV1,
  chunk_index: Int,
) -> Bool {
  and {
    proof.field_index == output_field_index,
    proof.item_index == output_index,
    proof.total_length == total_length,
    proof.chunk_index == chunk_index,
    bounded_item_v1.verify_chunk(item_commitment, proof),
  }
}

pub fn authenticated_chunk_window_for_output(
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  cursor: Int,
  witness: LedgerOutputProofWitnessV1,
  require_following_chunk: Bool,
) -> Option<Pair<ByteArray, Int>> {
  when witness is {
    LedgerOutputProofChunks { chunk_proof, next_chunk_proof } -> {
      let chunk_index = cursor / bounded_item_v1.chunk_bytes
      let chunk_count = bounded_item_v1.chunk_count(total_length)
      let has_following_chunk = chunk_index + 1 < chunk_count
      if !output_chunk_proof_matches(
        output_index,
        total_length,
        item_commitment,
        chunk_proof,
        chunk_index,
      ) {
        None
      } else if and {
        require_following_chunk,
        has_following_chunk,
      } {
        when next_chunk_proof is {
          None -> None
          Some(next) ->
            if output_chunk_proof_matches(
              output_index,
              total_length,
              item_commitment,
              next,
              chunk_index + 1,
            ) {
              Some(
                Pair(
                  bytearray.concat(chunk_proof.chunk, next.chunk),
                  cursor - chunk_index * bounded_item_v1.chunk_bytes,
                ),
              )
            } else {
              None
            }
        }
      } else if next_chunk_proof == None {
        Some(
          Pair(
            chunk_proof.chunk,
            cursor - chunk_index * bounded_item_v1.chunk_bytes,
          ),
        )
      } else {
        None
      }
    }
    _ -> None
  }
}

fn authenticated_chunk_window(
  control: LedgerOutputProofControlV1,
  cursor: Int,
  witness: LedgerOutputProofWitnessV1,
  require_following_chunk: Bool,
) -> Option<Pair<ByteArray, Int>> {
  authenticated_chunk_window_for_output(
    control.output_index,
    control.total_length,
    control.item_commitment,
    cursor,
    witness,
    require_following_chunk,
  )
}

pub fn authenticated_output_span_for_output(
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  absolute_start: Int,
  length: Int,
  witness: LedgerOutputProofWitnessV1,
) -> Option<ByteArray> {
  if or {
    length <= 0,
    length > bounded_item_v1.chunk_bytes,
    absolute_start < 0,
    absolute_start + length > total_length,
  } {
    None
  } else {
    when witness is {
      LedgerOutputProofChunks { chunk_proof, next_chunk_proof } -> {
        let first_chunk_index = absolute_start / bounded_item_v1.chunk_bytes
        let last_chunk_index = ( absolute_start + length - 1 ) / bounded_item_v1.chunk_bytes
        let local_start = absolute_start - first_chunk_index * bounded_item_v1.chunk_bytes
        if or {
          last_chunk_index > first_chunk_index + 1,
          !output_chunk_proof_matches(
            output_index,
            total_length,
            item_commitment,
            chunk_proof,
            first_chunk_index,
          ),
        } {
          None
        } else if last_chunk_index == first_chunk_index {
          if next_chunk_proof == None {
            Some(
              builtin.slice_bytearray(local_start, length, chunk_proof.chunk),
            )
          } else {
            None
          }
        } else {
          when next_chunk_proof is {
            None -> None
            Some(next) ->
              if output_chunk_proof_matches(
                output_index,
                total_length,
                item_commitment,
                next,
                last_chunk_index,
              ) {
                Some(
                  chunk_proof.chunk
                    |> bytearray.concat(next.chunk)
                    |> builtin.slice_bytearray(local_start, length, _),
                )
              } else {
                None
              }
          }
        }
      }
      _ -> None
    }
  }
}

fn authenticated_output_span(
  control: LedgerOutputProofControlV1,
  absolute_start: Int,
  length: Int,
  witness: LedgerOutputProofWitnessV1,
) -> Option<ByteArray> {
  authenticated_output_span_for_output(
    control.output_index,
    control.total_length,
    control.item_commitment,
    absolute_start,
    length,
    witness,
  )
}

/// Whether the recorded span window contains the span
/// `[absolute_start, absolute_start + length)`. This is the interval half of
/// `bound_window_bytes_v1`; the span-attach step is admitted exactly when it
/// fails for the span the pre-control demands next, so attaching and
/// consuming are exclusive at every pre-control.
pub fn window_covers_v1(
  span_window: Option<LedgerOutputSpanWindowV1>,
  absolute_start: Int,
  length: Int,
) -> Bool {
  when span_window is {
    None -> False
    Some(window) -> and {
        length > 0,
        absolute_start >= window.start,
        absolute_start + length <= window.start + window.length,
      }
  }
}

/// Bind redeemer-supplied window bytes to the recorded span-window commitment
/// (one hash plus equality) and slice the demanded span out of them. This is
/// the consuming half of the span-attach step: the digest was recorded by an
/// earlier step whose chunk merkle verification proved these exact bytes.
pub fn bound_window_bytes_v1(
  span_window: Option<LedgerOutputSpanWindowV1>,
  absolute_start: Int,
  length: Int,
  bytes: ByteArray,
) -> Option<ByteArray> {
  when span_window is {
    None -> None
    Some(window) ->
      if and {
        window_covers_v1(span_window, absolute_start, length),
        bytearray.length(bytes) == window.length,
        crypto.blake2b_256(bytes) == window.digest,
      } {
        Some(
          builtin.slice_bytearray(absolute_start - window.start, length, bytes),
        )
      } else {
        None
      }
  }
}

/// The `(absolute_start, length)` of reference-script chunk `chunk_index`:
/// the span the reference-script commitment stage binds next.
pub fn reference_script_chunk_span_v1(
  item_offset: Int,
  item_length: Int,
  chunk_index: Int,
) -> (Int, Int) {
  (
    item_offset + chunk_index * bounded_item_v1.chunk_bytes,
    bounded_item_v1.expected_chunk_length(item_length, chunk_index),
  )
}

/// The `(absolute_start, content_length)` of the script bytes the next ready
/// block of the reference-script hash trace reads. The first block's leading
/// byte is the language tag, which is not read from the output.
pub fn script_hash_content_span_v1(
  reference_script_offset: Int,
  cursor: Int,
  total_length: Int,
) -> (Int, Int) {
  let remaining = total_length - cursor
  let expected_length = if remaining < blake2b_224_trace_v1.block_bytes {
    remaining
  } else {
    blake2b_224_trace_v1.block_bytes
  }
  if cursor == 0 {
    (reference_script_offset, expected_length - 1)
  } else {
    (reference_script_offset + cursor - 1, expected_length)
  }
}

/// The output span the pre-control's consuming stage step reads next, or
/// `None` when that step reads no output bytes. Each arm is the consuming
/// stage's own span formula.
pub fn demanded_span_v1(
  control: LedgerOutputProofControlV1,
) -> Option<(Int, Int)> {
  if control.stage == stage_datum_traversal {
    when control.datum is {
      None -> None
      Some(datum_control) ->
        when
          cek_data_traverse_v1.prevalidated_next_source_span(datum_control)
        is {
          None -> None
          Some(span) -> Some((span.absolute_start, span.length))
        }
    }
  } else if control.stage == stage_reference_script_commitment {
    let item_offset = control.output_scan.reference_script_item_offset
    let item_length = control.total_length - item_offset
    if control.reference_script_count == bounded_item_v1.chunk_count(
      item_length,
    ) {
      None
    } else {
      Some(
        reference_script_chunk_span_v1(
          item_offset,
          item_length,
          control.reference_script_count,
        ),
      )
    }
  } else if control.stage == stage_script_hash {
    when control.script_hash is {
      None -> None
      Some(hash_control) ->
        if hash_control.stage == blake2b_224_trace_v1.stage_ready {
          let span = script_hash_content_span_v1(
            control.output_scan.reference_script_offset,
            hash_control.cursor,
            hash_control.total_length,
          )
          if span.2nd > 0 {
            Some(span)
          } else {
            None
          }
        } else {
          None
        }
    }
  } else {
    None
  }
}

/// The length of the window a span attach records at `start`: the maximal
/// window, `min(chunk_bytes, total_length - start)`.
pub fn attach_window_length_v1(total_length: Int, start: Int) -> Int {
  if total_length - start < bounded_item_v1.chunk_bytes {
    total_length - start
  } else {
    bounded_item_v1.chunk_bytes
  }
}

/// The span-attach step: verify the chunk merkle membership of the window
/// the pre-control demands exactly once and record its `(start, length,
/// digest)` in the control. The window starts at the demanded span and runs
/// for `min(chunk_bytes, total_length - start)` bytes; the step is admitted
/// only when the recorded window does not already cover the demanded span.
fn span_attach_step(
  control: LedgerOutputProofControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<LedgerOutputProofStepResultV1> {
  when demanded_span_v1(control) is {
    None -> None
    Some((start, length)) ->
      if window_covers_v1(control.span_window, start, length) {
        None
      } else {
        let window_length = attach_window_length_v1(control.total_length, start)
        when
          authenticated_output_span(
            control,
            start,
            window_length,
            LedgerOutputProofChunks { chunk_proof, next_chunk_proof },
          )
        is {
          None -> None
          Some(bytes) ->
            advanced(
              LedgerOutputProofControlV1 {
                ..control,
                span_window: Some(
                  LedgerOutputSpanWindowV1 {
                    start,
                    length: window_length,
                    digest: crypto.blake2b_256(bytes),
                  },
                ),
              },
            )
        }
      }
  }
}

fn advanced(
  control: LedgerOutputProofControlV1,
) -> Option<LedgerOutputProofStepResultV1> {
  if control_is_well_formed(control) {
    Some(LedgerOutputProofAdvanced { control })
  } else {
    None
  }
}

fn map_native_result(
  result: Option<native_script_scan_v1.NativeScriptStructureStepResultV1>,
  control: LedgerOutputProofControlV1,
) -> Option<LedgerOutputProofStepResultV1> {
  when result is {
    None -> None
    Some(native_script_scan_v1.NativeScriptStructureAdvanced { control: next }) ->
      advanced(
        LedgerOutputProofControlV1 { ..control, native_script: Some(next) },
      )
    Some(native_script_scan_v1.NativeScriptStructureInvalid) ->
      Some(LedgerOutputProofInvalidReferenceScript)
    Some(native_script_scan_v1.NativeScriptStructureNodeLimit) ->
      Some(LedgerOutputProofNativeScriptNodeLimit)
    Some(native_script_scan_v1.NativeScriptStructureDepthLimit) ->
      Some(LedgerOutputProofNativeScriptDepthLimit)
  }
}

fn structure_step(
  control: LedgerOutputProofControlV1,
  witness: LedgerOutputProofWitnessV1,
) -> Option<LedgerOutputProofStepResultV1> {
  if ledger_output_scan_v1.terminal_is_exact_v1(
    control.output_scan,
    control.total_length,
  ) {
    if witness != LedgerOutputProofNoWitness {
      None
    } else {
      advanced(
        LedgerOutputProofControlV1 {
          ..control,
          stage: stage_value_fold,
          value: Some(
            ledger_output_value_v1.initial_control_v1(
              control.output_scan.asset_count,
            ),
          ),
        },
      )
    }
  } else {
    when
      ledger_output_scan_v1.finish_v1(control.output_scan, control.total_length)
    is {
      Some(finished) ->
        if witness == LedgerOutputProofNoWitness {
          advanced(
            LedgerOutputProofControlV1 { ..control, output_scan: finished },
          )
        } else {
          None
        }
      None ->
        when
          authenticated_chunk_window(
            control,
            control.output_scan.cursor,
            witness,
            control.output_scan.stage <= ledger_output_scan_v1.stage_optional_field,
          )
        is {
          None -> None
          Some(Pair(window, window_offset)) ->
            when
              ledger_output_scan_v1.step_v1(
                control.output_scan,
                control.total_length,
                window,
                window_offset,
              )
            is {
              None -> Some(LedgerOutputProofInvalidOutput)
              Some(next_scan) ->
                advanced(
                  LedgerOutputProofControlV1 {
                    ..control,
                    output_scan: next_scan,
                  },
                )
            }
        }
    }
  }
}

fn value_fold_step(
  control: LedgerOutputProofControlV1,
  witness: LedgerOutputProofWitnessV1,
  value_control: ledger_output_value_v1.LedgerOutputValueControlV1,
) -> Option<LedgerOutputProofStepResultV1> {
  if value_control.stage == ledger_output_value_v1.stage_terminal {
    if witness != LedgerOutputProofNoWitness {
      None
    } else if control.output_scan.datum_offset != -1 {
      advanced(
        LedgerOutputProofControlV1 {
          ..control,
          stage: stage_datum_traversal,
          datum: Some(
            cek_data_traverse_v1.initial_control_v1(
              control.output_scan.datum_offset,
              control.output_scan.datum_length,
            ),
          ),
        },
      )
    } else {
      advanced(
        LedgerOutputProofControlV1 {
          ..control,
          stage: if control.output_scan.reference_script_language == -1 {
            stage_terminal
          } else {
            stage_reference_script_commitment
          },
        },
      )
    }
  } else {
    let value_witness = when witness is {
      LedgerOutputProofNoWitness ->
        Some(ledger_output_value_v1.LedgerOutputValueNoWitness)
      LedgerOutputProofValue {
        asset_index,
        policy_id,
        asset_name,
        quantity,
        siblings,
        previous,
      } ->
        Some(
          ledger_output_value_v1.LedgerOutputValueAsset {
            asset_index,
            policy_id,
            asset_name,
            quantity,
            siblings,
            previous,
          },
        )
      _ -> None
    }
    when value_witness is {
      None -> None
      Some(inner_witness) ->
        when
          ledger_output_value_v1.step_v1(
            value_control,
            control.output_scan.asset_count,
            control.output_scan.asset_peaks,
            control.output_scan.lovelace,
            inner_witness,
          )
        is {
          None -> None
          Some(next_value) ->
            advanced(
              LedgerOutputProofControlV1 { ..control, value: Some(next_value) },
            )
        }
    }
  }
}

fn datum_traversal_step(
  control: LedgerOutputProofControlV1,
  witness: LedgerOutputProofWitnessV1,
  datum_control: cek_data_traverse_v1.DataTraverseControlV1,
) -> Option<LedgerOutputProofStepResultV1> {
  if datum_control.stage == cek_data_traverse_v1.stage_terminal {
    if witness == LedgerOutputProofNoWitness {
      advanced(
        LedgerOutputProofControlV1 {
          ..control,
          stage: if control.output_scan.reference_script_language == -1 {
            stage_terminal
          } else {
            stage_reference_script_commitment
          },
        },
      )
    } else {
      None
    }
  } else {
    when witness is {
      LedgerOutputProofDatum { action, window } -> {
        let authenticated: Option<Option<ByteArray>> = when
          cek_data_traverse_v1.next_source_span_v1(datum_control)
        is {
          None ->
            if window == None {
              Some(None)
            } else {
              None
            }
          Some(span) ->
            when window is {
              None -> None
              Some(window_bytes) ->
                when
                  bound_window_bytes_v1(
                    control.span_window,
                    span.absolute_start,
                    span.length,
                    window_bytes,
                  )
                is {
                  None -> None
                  Some(bytes) -> Some(Some(bytes))
                }
            }
        }
        when authenticated is {
          None -> None
          Some(source_bytes) ->
            when
              cek_data_traverse_v1.step_v1(datum_control, source_bytes, action)
            is {
              None -> None
              Some(next_datum) ->
                advanced(
                  LedgerOutputProofControlV1 {
                    ..control,
                    datum: Some(next_datum),
                  },
                )
            }
        }
      }
      _ -> None
    }
  }
}

fn reference_script_commitment_step(
  control: LedgerOutputProofControlV1,
  witness: LedgerOutputProofWitnessV1,
) -> Option<LedgerOutputProofStepResultV1> {
  let item_offset = control.output_scan.reference_script_item_offset
  let item_length = control.total_length - item_offset
  let chunk_count = bounded_item_v1.chunk_count(item_length)
  let chunk_index = control.reference_script_count
  if chunk_index == chunk_count {
    if witness == LedgerOutputProofNoWitness {
      advanced(
        LedgerOutputProofControlV1 {
          ..control,
          stage: stage_script_hash,
          script_hash: Some(
            blake2b_224_trace_v1.initial_control_v1(
              control.output_scan.reference_script_length + 1,
            ),
          ),
        },
      )
    } else {
      None
    }
  } else {
    let (absolute_start, chunk_length) = reference_script_chunk_span_v1(
      item_offset,
      item_length,
      chunk_index,
    )
    when
      when witness is {
        LedgerOutputProofWindow { bytes } ->
          bound_window_bytes_v1(
            control.span_window,
            absolute_start,
            chunk_length,
            bytes,
          )
        _ -> None
      }
    is {
      None -> None
      Some(chunk) ->
        advanced(
          LedgerOutputProofControlV1 {
            ..control,
            reference_script_count: chunk_index + 1,
            reference_script_peaks: validation_merkle_v1.append_leaf(
              chunk_index,
              control.reference_script_peaks,
              bounded_item_v1.hash_chunk(
                output_field_index,
                control.output_index,
                chunk_index,
                chunk,
              ),
            ),
          },
        )
    }
  }
}

fn script_hash_step(
  control: LedgerOutputProofControlV1,
  witness: LedgerOutputProofWitnessV1,
  hash_control: blake2b_224_trace_v1.Blake2b224TraceControlV1,
) -> Option<LedgerOutputProofStepResultV1> {
  if hash_control.stage == blake2b_224_trace_v1.stage_terminal {
    if witness != LedgerOutputProofNoWitness {
      None
    } else if control.output_scan.reference_script_language != 0 {
      advanced(LedgerOutputProofControlV1 { ..control, stage: stage_terminal })
    } else if control.output_scan.reference_script_length == 0 {
      Some(LedgerOutputProofInvalidReferenceScript)
    } else {
      advanced(
        LedgerOutputProofControlV1 {
          ..control,
          stage: stage_native_script,
          native_script: Some(
            native_script_scan_v1.initial_structure_control_v1(
              control.output_scan.reference_script_offset,
              control.output_scan.reference_script_length,
            ),
          ),
        },
      )
    }
  } else if hash_control.stage == blake2b_224_trace_v1.stage_ready {
    let includes_language = hash_control.cursor == 0
    let (absolute_start, content_length) = script_hash_content_span_v1(
      control.output_scan.reference_script_offset,
      hash_control.cursor,
      hash_control.total_length,
    )
    let content = if content_length == 0 {
      if witness == LedgerOutputProofNoWitness {
        Some(#"")
      } else {
        None
      }
    } else {
      when witness is {
        LedgerOutputProofWindow { bytes } ->
          bound_window_bytes_v1(
            control.span_window,
            absolute_start,
            content_length,
            bytes,
          )
        _ -> None
      }
    }
    when content is {
      None -> None
      Some(bytes) -> {
        let block = if includes_language {
          #""
            |> bytearray.push(control.output_scan.reference_script_language)
            |> bytearray.concat(bytes)
        } else {
          bytes
        }
        when blake2b_224_trace_v1.step_v1(hash_control, Some(block)) is {
          None -> None
          Some(next_hash) ->
            advanced(
              LedgerOutputProofControlV1 {
                ..control,
                script_hash: Some(next_hash),
              },
            )
        }
      }
    }
  } else if witness != LedgerOutputProofNoWitness {
    None
  } else {
    when blake2b_224_trace_v1.step_v1(hash_control, None) is {
      None -> None
      Some(next_hash) ->
        advanced(
          LedgerOutputProofControlV1 { ..control, script_hash: Some(next_hash) },
        )
    }
  }
}

fn native_script_step(
  control: LedgerOutputProofControlV1,
  witness: LedgerOutputProofWitnessV1,
  native_control: native_script_scan_v1.NativeScriptStructureControlV1,
) -> Option<LedgerOutputProofStepResultV1> {
  if native_control.stage == native_script_scan_v1.structure_stage_terminal {
    if witness == LedgerOutputProofNoWitness {
      advanced(LedgerOutputProofControlV1 { ..control, stage: stage_terminal })
    } else {
      None
    }
  } else if native_control.stage == native_script_scan_v1.structure_stage_token {
    when
      authenticated_chunk_window(control, native_control.cursor, witness, True)
    is {
      None -> None
      Some(Pair(window, window_offset)) ->
        map_native_result(
          native_script_scan_v1.structure_token_step_v1(
            native_control,
            window,
            window_offset,
          ),
          control,
        )
    }
  } else if native_control.stage == native_script_scan_v1.structure_stage_frame {
    when witness is {
      LedgerOutputProofNativeFrame { frame } ->
        map_native_result(
          native_script_scan_v1.structure_frame_step_v1(native_control, frame),
          control,
        )
      _ -> None
    }
  } else if witness == LedgerOutputProofNoWitness {
    map_native_result(
      native_script_scan_v1.finalize_structure_v1(native_control),
      control,
    )
  } else {
    None
  }
}

pub fn step_v1(
  control: LedgerOutputProofControlV1,
  witness: LedgerOutputProofWitnessV1,
) -> Option<LedgerOutputProofStepResultV1> {
  if !control_is_well_formed(control) {
    None
  } else if when witness is {
    LedgerOutputProofSpanAttach { .. } -> True
    _ -> False
  } {
    expect LedgerOutputProofSpanAttach { chunk_proof, next_chunk_proof } = witness
    span_attach_step(control, chunk_proof, next_chunk_proof)
  } else if control.stage == stage_structure {
    structure_step(control, witness)
  } else if control.stage == stage_value_fold {
    when control.value is {
      None -> None
      Some(value_control) -> value_fold_step(control, witness, value_control)
    }
  } else if control.stage == stage_script_hash {
    when control.script_hash is {
      None -> None
      Some(hash_control) -> script_hash_step(control, witness, hash_control)
    }
  } else if control.stage == stage_datum_traversal {
    when control.datum is {
      None -> None
      Some(datum_control) ->
        datum_traversal_step(control, witness, datum_control)
    }
  } else if control.stage == stage_reference_script_commitment {
    reference_script_commitment_step(control, witness)
  } else if control.stage == stage_native_script {
    when control.native_script is {
      None -> None
      Some(native_control) ->
        native_script_step(control, witness, native_control)
    }
  } else {
    None
  }
}

pub fn value_summary_v1(
  control: LedgerOutputProofControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if !terminal_is_exact_v1(control) {
    None
  } else {
    when control.value is {
      None -> None
      Some(value_control) -> ledger_output_value_v1.finalize_v1(value_control)
    }
  }
}

fn credential_data_v1(credential: MidgardCredential) -> Data {
  when credential is {
    PubKeyCredential(hash) -> builtin.constr_data(0, [builtin.b_data(hash)])
    ScriptCredential(hash) -> builtin.constr_data(1, [builtin.b_data(hash)])
  }
}

fn stake_credential_data_v1(credential: Option<MidgardCredential>) -> Data {
  when credential is {
    None -> builtin.constr_data(1, [])
    Some(value) ->
      builtin.constr_data(
        0,
        [builtin.constr_data(0, [credential_data_v1(value)])],
      )
  }
}

fn output_address_summary_v1(
  control: LedgerOutputProofControlV1,
  midgard_encoding: Bool,
) -> Option<cek_data_v1.DataSummaryV1> {
  when
    ledger_output_v1.decode_canonical_address_bytes(control.output_scan.address)
  is {
    None -> None
    Some(address) ->
      Some(
        cek_data_v1.semantic_data_summary_v1(
          builtin.constr_data(
            if midgard_encoding && address.protected {
              1
            } else {
              0
            },
            [
              credential_data_v1(address.payment_credential),
              stake_credential_data_v1(address.stake_credential),
            ],
          ),
        ),
      )
  }
}

fn output_datum_summary_v1(
  control: LedgerOutputProofControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if control.output_scan.datum_offset == -1 {
    Some(
      cek_data_v1.small_constr_data_summary_v1(
        0,
        cek_data_v1.empty_data_list_summary_v1(),
      ),
    )
  } else {
    when control.datum is {
      None -> None
      Some(datum_control) ->
        when cek_data_traverse_v1.finalize_v1(datum_control) is {
          None -> None
          Some(datum) ->
            Some(
              cek_data_v1.small_constr_data_summary_v1(
                2,
                cek_data_v1.prepend_data_list_summary_v1(
                  datum,
                  cek_data_v1.empty_data_list_summary_v1(),
                ),
              ),
            )
        }
    }
  }
}

fn output_reference_script_summary_v1(
  control: LedgerOutputProofControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if control.output_scan.reference_script_language == -1 {
    Some(
      cek_data_v1.small_constr_data_summary_v1(
        1,
        cek_data_v1.empty_data_list_summary_v1(),
      ),
    )
  } else {
    when reference_script_digest_v1(control) is {
      None -> None
      Some(digest) ->
        Some(
          cek_data_v1.semantic_data_summary_v1(
            builtin.constr_data(0, [builtin.b_data(digest)]),
          ),
        )
    }
  }
}

fn tx_out_summary_v1(
  control: LedgerOutputProofControlV1,
  midgard_encoding: Bool,
) -> Option<cek_data_v1.DataSummaryV1> {
  if !terminal_is_exact_v1(control) {
    None
  } else {
    when output_address_summary_v1(control, midgard_encoding) is {
      None -> None
      Some(address) ->
        when value_summary_v1(control) is {
          None -> None
          Some(value) ->
            when output_datum_summary_v1(control) is {
              None -> None
              Some(datum) ->
                when output_reference_script_summary_v1(control) is {
                  None -> None
                  Some(reference_script) ->
                    Some(
                      cek_data_v1.small_constr_data_summary_v1(
                        0,
                        cek_data_v1.prepend_data_list_summary_v1(
                          address,
                          cek_data_v1.prepend_data_list_summary_v1(
                            value,
                            cek_data_v1.prepend_data_list_summary_v1(
                              datum,
                              cek_data_v1.prepend_data_list_summary_v1(
                                reference_script,
                                cek_data_v1.empty_data_list_summary_v1(),
                              ),
                            ),
                          ),
                        ),
                      ),
                    )
                }
            }
        }
    }
  }
}

pub fn cardano_tx_out_summary_v1(
  control: LedgerOutputProofControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  tx_out_summary_v1(control, False)
}

pub fn midgard_tx_out_summary_v1(
  control: LedgerOutputProofControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  tx_out_summary_v1(control, True)
}

pub fn cardano_spend_datum_summary_v1(
  control: LedgerOutputProofControlV1,
) -> Option<cek_data_v1.DataSummaryV1> {
  if !terminal_is_exact_v1(control) {
    None
  } else if control.output_scan.datum_offset == -1 {
    Some(
      cek_data_v1.small_constr_data_summary_v1(
        1,
        cek_data_v1.empty_data_list_summary_v1(),
      ),
    )
  } else {
    when control.datum is {
      None -> None
      Some(datum_control) ->
        when cek_data_traverse_v1.finalize_v1(datum_control) is {
          None -> None
          Some(datum) ->
            Some(
              cek_data_v1.small_constr_data_summary_v1(
                0,
                cek_data_v1.prepend_data_list_summary_v1(
                  datum,
                  cek_data_v1.empty_data_list_summary_v1(),
                ),
              ),
            )
        }
    }
  }
}

pub fn reference_script_item_commitment_v1(
  control: LedgerOutputProofControlV1,
) -> Option<ByteArray> {
  if or {
    !terminal_is_exact_v1(control),
    control.output_scan.reference_script_language == -1,
  } {
    None
  } else {
    Some(
      bounded_item_v1.commitment(
        output_field_index,
        control.output_index,
        control.total_length - control.output_scan.reference_script_item_offset,
        control.reference_script_peaks,
      ),
    )
  }
}

pub fn terminal_is_exact_v1(control: LedgerOutputProofControlV1) -> Bool {
  and {
    control_is_well_formed(control),
    control.stage == stage_terminal,
  }
}

pub fn reference_script_digest_v1(
  control: LedgerOutputProofControlV1,
) -> Option<ByteArray> {
  if !terminal_is_exact_v1(control) {
    None
  } else {
    when control.script_hash is {
      None -> None
      Some(hash_control) -> blake2b_224_trace_v1.digest_v1(hash_control)
    }
  }
}

/// The four reference-script descriptor fields of an exact terminal control:
/// `(language, hash, total_length, item_commitment)`, the empty tuple of an
/// output without a reference script.
fn terminal_reference_script_fields_v1(
  control: LedgerOutputProofControlV1,
) -> Option<(Int, ByteArray, Int, ByteArray)> {
  let language = control.output_scan.reference_script_language
  if language == -1 {
    Some((-1, #"", 0, #""))
  } else {
    when reference_script_digest_v1(control) is {
      None -> None
      Some(digest) ->
        when reference_script_item_commitment_v1(control) is {
          None -> None
          Some(item_commitment) ->
            Some(
              (
                language,
                digest,
                control.total_length - control.output_scan.reference_script_item_offset,
                item_commitment,
              ),
            )
        }
    }
  }
}

/// The descriptor of an exact terminal control, every field computed from the
/// control itself. `None` off the terminal stage.
pub fn terminal_descriptor_v1(
  control: LedgerOutputProofControlV1,
) -> Option<ledger_output_commitment_v1.LedgerOutputCommitmentV1> {
  when cardano_tx_out_summary_v1(control) is {
    None -> None
    Some(cardano_tx_out) ->
      when midgard_tx_out_summary_v1(control) is {
        None -> None
        Some(midgard_tx_out) ->
          when cardano_spend_datum_summary_v1(control) is {
            None -> None
            Some(cardano_spend_datum) ->
              when terminal_reference_script_fields_v1(control) is {
                None -> None
                Some((language, hash, reference_length, reference_commitment)) ->
                  Some(
                    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
                      version: ledger_output_commitment_v1.version,
                      output_index: control.output_index,
                      total_length: control.total_length,
                      item_commitment: control.item_commitment,
                      address: control.output_scan.address,
                      lovelace: control.output_scan.lovelace,
                      asset_count: control.output_scan.asset_count,
                      asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
                        control.output_scan.asset_count,
                        control.output_scan.asset_peaks,
                      ),
                      cardano_value_size: control.output_scan.cardano_value_size,
                      reference_script_language: language,
                      reference_script_hash: hash,
                      reference_script_total_length: reference_length,
                      reference_script_item_commitment: reference_commitment,
                      cardano_tx_out,
                      midgard_tx_out,
                      cardano_spend_datum,
                    },
                  )
              }
          }
      }
  }
}

/// `blake2b_256` over the serialised Plutus data list `payload`: the shape of
/// every descriptor fact commitment.
pub fn fact_digest_v1(payload: List<Data>) -> ByteArray {
  crypto.blake2b_256(builtin.serialise_data(builtin.list_data(payload)))
}

/// The role-1 fact payload: the four reference-script descriptor fields,
/// exactly what the reference-script descriptor yield pins.
pub fn reference_script_fact_payload_v1(
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> List<Data> {
  [
    builtin.i_data(descriptor.reference_script_language),
    builtin.b_data(descriptor.reference_script_hash),
    builtin.i_data(descriptor.reference_script_total_length),
    builtin.b_data(descriptor.reference_script_item_commitment),
  ]
}

/// The commitment a fact-attachment step records for descriptor role `role`
/// (0 scan facts, 1 reference script, 2 datum summary, 3 value summary). Each
/// fact commits only what its own descriptor yield pins: role 3 the value
/// summary, role 2 the datum summary, role 1 the four reference-script
/// fields, and role 0 the whole descriptor. The role-0 yield runs last and
/// checks the three recorded facts against the descriptor and the summaries
/// it consumes, so every attachment step has one successor and the thin
/// terminal's descriptor is the preimage of the role-0 fact.
pub fn fact_commitment_v1(
  role: Int,
  descriptor_cbor: ByteArray,
  claimed_value_summary: Data,
  claimed_datum_summary: Data,
) -> ByteArray {
  if role == 0 {
    fact_digest_v1([builtin.b_data(descriptor_cbor)])
  } else if role == 1 {
    fact_digest_v1(
      reference_script_fact_payload_v1(
        ledger_output_commitment_v1.decode(descriptor_cbor),
      ),
    )
  } else if role == 2 {
    fact_digest_v1([claimed_datum_summary])
  } else {
    expect role == 3
    fact_digest_v1([claimed_value_summary])
  }
}

pub fn fact_v1(
  control: LedgerOutputProofControlV1,
  role: Int,
) -> Option<ByteArray> {
  if role == 0 {
    control.scan_facts_fact
  } else if role == 1 {
    control.reference_script_fact
  } else if role == 2 {
    control.datum_summary_fact
  } else {
    expect role == 3
    control.value_summary_fact
  }
}

pub fn with_fact_v1(
  control: LedgerOutputProofControlV1,
  role: Int,
  commitment: ByteArray,
) -> LedgerOutputProofControlV1 {
  if role == 0 {
    LedgerOutputProofControlV1 { ..control, scan_facts_fact: Some(commitment) }
  } else if role == 1 {
    LedgerOutputProofControlV1 {
      ..control,
      reference_script_fact: Some(commitment),
    }
  } else if role == 2 {
    LedgerOutputProofControlV1 {
      ..control,
      datum_summary_fact: Some(commitment),
    }
  } else {
    expect role == 3
    LedgerOutputProofControlV1 {
      ..control,
      value_summary_fact: Some(commitment),
    }
  }
}

pub fn facts_complete_v1(control: LedgerOutputProofControlV1) -> Bool {
  and {
    control.scan_facts_fact != None,
    control.reference_script_fact != None,
    control.datum_summary_fact != None,
    control.value_summary_fact != None,
  }
}

/// The claimed-summary channel of the finalize dispatchers, computed from the
/// terminal control itself: the `DataSummaryV1` of the output's value and the
/// `Option<DataSummaryV1>` of its datum result, both as their wire data.
pub fn terminal_claimed_summaries_v1(
  control: LedgerOutputProofControlV1,
) -> Option<(Data, Data)> {
  if !terminal_is_exact_v1(control) {
    None
  } else {
    when value_summary_v1(control) is {
      None -> None
      Some(value_summary) -> {
        let value_data: Data = value_summary
        let datum_data: Data = if control.output_scan.datum_offset == -1 {
          builtin.constr_data(1, [])
        } else {
          when control.datum is {
            None -> builtin.constr_data(1, [])
            Some(datum_control) ->
              when cek_data_traverse_v1.finalize_v1(datum_control) is {
                None -> builtin.constr_data(1, [])
                Some(datum_summary) -> {
                  let inner: Data = datum_summary
                  builtin.constr_data(0, [inner])
                }
              }
          }
        }
        Some((value_data, datum_data))
      }
    }
  }
}

/// The canonical fact-attachment groups, in machine order: the two leaf
/// summaries first, then the reference script, then the scan facts, whose
/// yield consumes the three facts recorded before it. The grouping bounds
/// each attachment transaction's descriptor-yield count so its measured
/// memory stays within the positive-path basis.
pub fn fact_attach_groups_v1() -> List<List<Int>> {
  [[2, 3], [1], [0]]
}

/// One canonical fact-attachment machine step on a terminal control: record
/// the next incomplete group's fact commitments, computed from the
/// control's own terminal descriptor and summaries. `None` when every fact
/// is already recorded (the next step is the thin terminal finalize).
pub fn fact_attach_v1(
  control: LedgerOutputProofControlV1,
) -> Option<LedgerOutputProofControlV1> {
  when terminal_descriptor_v1(control) is {
    None -> None
    Some(descriptor) ->
      when terminal_claimed_summaries_v1(control) is {
        None -> None
        Some((value_data, datum_data)) -> {
          let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
          let next_group = list.find(
            fact_attach_groups_v1(),
            fn(group) {
              list.any(group, fn(role) { fact_v1(control, role) == None })
            },
          )
          when next_group is {
            None -> None
            Some(group) -> {
              expect
                list.all(group, fn(role) { fact_v1(control, role) == None })
              Some(
                list.foldl(
                  group,
                  control,
                  fn(role, acc) {
                    with_fact_v1(
                      acc,
                      role,
                      fact_commitment_v1(
                        role,
                        descriptor_cbor,
                        value_data,
                        datum_data,
                      ),
                    )
                  },
                ),
              )
            }
          }
        }
      }
  }
}

/// The thin terminal's fact gate: the recorded role-0 fact is the commitment
/// to exactly these descriptor bytes. A role-0 fact is recorded only by the
/// last attachment group, whose yield checked the other three facts against
/// the same descriptor, so this one equality pins the whole descriptor.
pub fn facts_are_exact_v1(
  control: LedgerOutputProofControlV1,
  descriptor_cbor: ByteArray,
) -> Bool {
  and {
    terminal_is_exact_v1(control),
    control.scan_facts_fact == Some(
      fact_digest_v1([builtin.b_data(descriptor_cbor)]),
    ),
  }
}
