use aiken/builtin
use aiken/cbor
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
}

pub type LedgerOutputProofWitnessV1 {
  LedgerOutputProofNoWitness
  LedgerOutputProofChunks {
    chunk_proof: bounded_item_v1.ChunkProofV1,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  }
  LedgerOutputProofValue {
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    siblings: List<ByteArray>,
  }
  LedgerOutputProofDatum {
    action: cek_data_traverse_v1.DataTraverseActionV1,
    chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  }
  LedgerOutputProofNativeFrame {
    frame: native_script_scan_v1.NativeScriptFrameV1,
  }
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
  }
  expect control_is_well_formed(control)
  control
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

pub fn encode_control_v1(control: LedgerOutputProofControlV1) -> ByteArray {
  expect control_is_well_formed(control)
  encode_definite_array_header(12)
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
}

pub fn decode_control_v1(control_cbor: ByteArray) -> LedgerOutputProofControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
      [
        version_data, stage_data, output_index_data, total_length_data,
        item_commitment_data, output_scan_data, value_data, datum_data,
        reference_script_count_data, reference_script_peaks_data,
        script_hash_data, native_script_data,
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
  }
  expect control_is_well_formed(control)
  expect encode_control_v1(control) == control_cbor
  control
}

fn output_chunk_proof_matches(
  control: LedgerOutputProofControlV1,
  proof: bounded_item_v1.ChunkProofV1,
  chunk_index: Int,
) -> Bool {
  and {
    proof.field_index == output_field_index,
    proof.item_index == control.output_index,
    proof.total_length == control.total_length,
    proof.chunk_index == chunk_index,
    bounded_item_v1.verify_chunk(control.item_commitment, proof),
  }
}

fn authenticated_chunk_window(
  control: LedgerOutputProofControlV1,
  cursor: Int,
  witness: LedgerOutputProofWitnessV1,
  require_following_chunk: Bool,
) -> Option<Pair<ByteArray, Int>> {
  when witness is {
    LedgerOutputProofChunks { chunk_proof, next_chunk_proof } -> {
      let chunk_index = cursor / bounded_item_v1.chunk_bytes
      let chunk_count = bounded_item_v1.chunk_count(control.total_length)
      let has_following_chunk = chunk_index + 1 < chunk_count
      if !output_chunk_proof_matches(control, chunk_proof, chunk_index) {
        None
      } else if and {
        require_following_chunk,
        has_following_chunk,
      } {
        when next_chunk_proof is {
          None -> None
          Some(next) ->
            if output_chunk_proof_matches(control, next, chunk_index + 1) {
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

fn authenticated_output_span(
  control: LedgerOutputProofControlV1,
  absolute_start: Int,
  length: Int,
  witness: LedgerOutputProofWitnessV1,
) -> Option<ByteArray> {
  if or {
    length <= 0,
    length > bounded_item_v1.chunk_bytes,
    absolute_start < 0,
    absolute_start + length > control.total_length,
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
          !output_chunk_proof_matches(control, chunk_proof, first_chunk_index),
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
              if output_chunk_proof_matches(control, next, last_chunk_index) {
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
      LedgerOutputProofValue { policy_id, asset_name, quantity, siblings } ->
        Some(
          ledger_output_value_v1.LedgerOutputValueAsset {
            policy_id,
            asset_name,
            quantity,
            siblings,
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
      LedgerOutputProofDatum { action, chunk_proof, next_chunk_proof } -> {
        let authenticated: Option<Option<ByteArray>> = when
          cek_data_traverse_v1.next_source_span_v1(datum_control)
        is {
          None ->
            if and {
              chunk_proof == None,
              next_chunk_proof == None,
            } {
              Some(None)
            } else {
              None
            }
          Some(span) ->
            when chunk_proof is {
              None -> None
              Some(first) ->
                when
                  authenticated_output_span(
                    control,
                    span.absolute_start,
                    span.length,
                    LedgerOutputProofChunks {
                      chunk_proof: first,
                      next_chunk_proof,
                    },
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
    let chunk_length = bounded_item_v1.expected_chunk_length(
      item_length,
      chunk_index,
    )
    let absolute_start = item_offset + chunk_index * bounded_item_v1.chunk_bytes
    when
      authenticated_output_span(control, absolute_start, chunk_length, witness)
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
    let remaining = hash_control.total_length - hash_control.cursor
    let expected_length = if remaining < blake2b_224_trace_v1.block_bytes {
      remaining
    } else {
      blake2b_224_trace_v1.block_bytes
    }
    let includes_language = hash_control.cursor == 0
    let language_length = if includes_language {
      1
    } else {
      0
    }
    let content_length = expected_length - language_length
    let output_offset_adjustment = if includes_language {
      0
    } else {
      1
    }
    let absolute_start = control.output_scan.reference_script_offset + hash_control.cursor - output_offset_adjustment
    let content = if content_length == 0 {
      if witness == LedgerOutputProofNoWitness {
        Some(#"")
      } else {
        None
      }
    } else {
      authenticated_output_span(
        control,
        absolute_start,
        content_length,
        witness,
      )
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

fn descriptor_reference_script_is_exact_v1(
  control: LedgerOutputProofControlV1,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> Bool {
  if control.output_scan.reference_script_language == -1 {
    and {
      descriptor.reference_script_language == -1,
      descriptor.reference_script_hash == #"",
      descriptor.reference_script_total_length == 0,
      descriptor.reference_script_item_commitment == #"",
    }
  } else {
    when reference_script_digest_v1(control) is {
      None -> False
      Some(digest) ->
        when reference_script_item_commitment_v1(control) is {
          None -> False
          Some(item_commitment) -> and {
              descriptor.reference_script_language == control.output_scan.reference_script_language,
              descriptor.reference_script_hash == digest,
              descriptor.reference_script_total_length == control.total_length - control.output_scan.reference_script_item_offset,
              descriptor.reference_script_item_commitment == item_commitment,
            }
        }
    }
  }
}

/// Fail-closed admission boundary for a compact ledger MPF value. Every
/// descriptor fact must equal the independently proven terminal output fact.
pub fn descriptor_is_exact_v1(
  control: LedgerOutputProofControlV1,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> Bool {
  if !terminal_is_exact_v1(control) {
    False
  } else {
    when cardano_tx_out_summary_v1(control) is {
      None -> False
      Some(cardano_tx_out) ->
        when midgard_tx_out_summary_v1(control) is {
          None -> False
          Some(midgard_tx_out) ->
            when cardano_spend_datum_summary_v1(control) is {
              None -> False
              Some(cardano_spend_datum) -> and {
                  descriptor.version == ledger_output_commitment_v1.version,
                  descriptor.output_index == control.output_index,
                  descriptor.total_length == control.total_length,
                  descriptor.item_commitment == control.item_commitment,
                  descriptor.address == control.output_scan.address,
                  descriptor.lovelace == control.output_scan.lovelace,
                  descriptor.asset_count == control.output_scan.asset_count,
                  descriptor.asset_frontier_commitment == validation_merkle_v1.frontier_commitment(
                    control.output_scan.asset_count,
                    control.output_scan.asset_peaks,
                  ),
                  descriptor.cardano_value_size == control.output_scan.cardano_value_size,
                  descriptor_reference_script_is_exact_v1(control, descriptor),
                  descriptor.cardano_tx_out == cardano_tx_out,
                  descriptor.midgard_tx_out == midgard_tx_out,
                  descriptor.cardano_spend_datum == cardano_spend_datum,
                }
            }
        }
    }
  }
}
