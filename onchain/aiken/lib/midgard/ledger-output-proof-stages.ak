//// Narrow LOP stage transitions. The raw frame is the authenticated carrier's
//// exact control; unchanged sub-controls are preserved without typed decoding.

use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1 as blake
use midgard/bounded_item_v1 as bounded
use midgard/cek_data_v1 as data_summary
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_v1 as proof
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_value_v1 as value
use midgard/native_script_scan_v1 as native
use midgard/validation_machine/witness.{decode_frontier_peaks}
use midgard/validation_merkle_v1 as merkle

pub type StepResult {
  Advanced { control: raw.Frame }
  InvalidOutput
  InvalidReferenceScript
  NativeScriptNodeLimit
  NativeScriptDepthLimit
}

fn encoded_data(bytes: ByteArray) -> Data {
  expect Some(data) = cbor.deserialise(bytes)
  data
}

pub fn structure(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  expect raw.integer(control, 1) == proof.stage_structure
  let scan_control = scan.control_from_data_v1(raw.item(control, 5))
  let total_length = raw.integer(control, 3)
  expect scan_control.cursor <= total_length
  expect raw.item(control, 6) == builtin.constr_data(1, [])
  expect raw.item(control, 7) == builtin.constr_data(1, [])
  expect raw.integer(control, 8) == 0
  expect raw.item(control, 9) == builtin.list_data([])
  expect raw.item(control, 10) == builtin.constr_data(1, [])
  expect raw.item(control, 11) == builtin.constr_data(1, [])
  if scan.terminal_is_exact_v1(scan_control, total_length) {
    if witness != proof.LedgerOutputProofNoWitness {
      None
    } else {
      let initial_value = initial_value_data(scan_control.asset_count)
      Some(
        Advanced {
          control: control
            |> raw.replace(1, builtin.i_data(proof.stage_value_fold))
            |> raw.replace(6, builtin.constr_data(0, [initial_value])),
        },
      )
    }
  } else {
    when scan.finish_v1(scan_control, total_length) is {
      Some(finished) ->
        if witness == proof.LedgerOutputProofNoWitness {
          Some(
            Advanced {
              control: raw.replace(
                control,
                5,
                encoded_data(scan.encode_control_v1(finished)),
              ),
            },
          )
        } else {
          None
        }
      None ->
        when
          proof.authenticated_chunk_window_for_output(
            raw.integer(control, 2),
            total_length,
            raw.bytes(control, 4),
            scan_control.cursor,
            witness,
            scan_control.stage <= scan.stage_optional_field,
          )
        is {
          None -> None
          Some(Pair(window, window_offset)) ->
            when
              scan.step_v1(scan_control, total_length, window, window_offset)
            is {
              None -> Some(InvalidOutput)
              Some(next) ->
                Some(
                  Advanced {
                    control: raw.replace(
                      control,
                      5,
                      encoded_data(scan.encode_control_v1(next)),
                    ),
                  },
                )
            }
        }
    }
  }
}

pub fn initial_datum_data(offset: Int, length: Int) -> Data {
  expect offset >= 0 && offset <= 4294967295
  expect length > 0 && length <= 4294967295
  let none = builtin.constr_data(1, [])
  builtin.list_data(
    [
      builtin.i_data(1),
      builtin.i_data(0),
      builtin.i_data(offset),
      builtin.i_data(length),
      builtin.i_data(0),
      builtin.b_data(#""),
      none,
      none,
      none,
      none,
    ],
  )
}

pub fn value_fold(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  expect raw.integer(control, 1) == proof.stage_value_fold
  let scan_control = scan.control_from_data_v1(raw.item(control, 5))
  expect scan.terminal_is_exact_v1(scan_control, raw.integer(control, 3))
  expect Pair(0, [value_data]) = builtin.un_constr_data(raw.item(control, 6))
  let value_control = value.control_from_data_v1(value_data)
  expect value_control.asset_remaining <= scan_control.asset_count
  expect raw.item(control, 7) == builtin.constr_data(1, [])
  expect raw.integer(control, 8) == 0
  expect raw.item(control, 9) == builtin.list_data([])
  expect raw.item(control, 10) == builtin.constr_data(1, [])
  expect raw.item(control, 11) == builtin.constr_data(1, [])
  if value_control.stage == value.stage_terminal {
    if witness != proof.LedgerOutputProofNoWitness {
      None
    } else if scan_control.datum_offset != -1 {
      Some(
        Advanced {
          control: control
            |> raw.replace(1, builtin.i_data(proof.stage_datum_traversal))
            |> raw.replace(
                7,
                builtin.constr_data(
                  0,
                  [
                    initial_datum_data(
                      scan_control.datum_offset,
                      scan_control.datum_length,
                    ),
                  ],
                ),
              ),
        },
      )
    } else {
      Some(
        Advanced {
          control: raw.replace(
            control,
            1,
            builtin.i_data(
              if scan_control.reference_script_language == -1 {
                proof.stage_terminal
              } else {
                proof.stage_reference_script_commitment
              },
            ),
          ),
        },
      )
    }
  } else {
    let inner = when witness is {
      proof.LedgerOutputProofNoWitness -> Some(value.LedgerOutputValueNoWitness)
      proof.LedgerOutputProofValue { policy_id, asset_name, quantity, siblings } ->
        Some(
          value.LedgerOutputValueAsset {
            policy_id,
            asset_name,
            quantity,
            siblings,
          },
        )
      _ -> None
    }
    when inner is {
      None -> None
      Some(inner_witness) ->
        when
          value.step_v1(
            value_control,
            scan_control.asset_count,
            scan_control.asset_peaks,
            scan_control.lovelace,
            inner_witness,
          )
        is {
          None -> None
          Some(next) ->
            Some(
              Advanced {
                control: raw.replace(
                  control,
                  6,
                  builtin.constr_data(
                    0,
                    [encoded_data(value.encode_control_v1(next))],
                  ),
                ),
              },
            )
        }
    }
  }
}

fn output_span(
  control: raw.Frame,
  start: Int,
  length: Int,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<ByteArray> {
  proof.authenticated_output_span_for_output(
    raw.integer(control, 2),
    raw.integer(control, 3),
    raw.bytes(control, 4),
    start,
    length,
    witness,
  )
}

fn optional_data(encoded: ByteArray) -> Data {
  builtin.constr_data(0, [encoded_data(encoded)])
}

pub fn reference_script(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  expect raw.integer(control, 1) == proof.stage_reference_script_commitment
  let scan_control = scan.control_from_data_v1(raw.item(control, 5))
  let length = raw.integer(control, 3)
  expect scan.terminal_is_exact_v1(scan_control, length)
  expect scan_control.reference_script_language != -1
  let item_offset = scan_control.reference_script_item_offset
  let item_length = length - item_offset
  expect item_length > 0
  let chunk_count = bounded.chunk_count(item_length)
  let chunk_index = raw.integer(control, 8)
  let peaks = decode_frontier_peaks(raw.item(control, 9))
  expect chunk_index >= 0 && chunk_index <= chunk_count
  expect merkle.frontier_is_well_formed(chunk_index, peaks)
  expect raw.item(control, 10) == builtin.constr_data(1, [])
  expect raw.item(control, 11) == builtin.constr_data(1, [])
  if chunk_index == chunk_count {
    if witness == proof.LedgerOutputProofNoWitness {
      let initial = blake.initial_control_v1(
        scan_control.reference_script_length + 1,
      )
        |> blake.encode_control_v1
        |> optional_data
      Some(
        Advanced {
          control: control
            |> raw.replace(1, builtin.i_data(proof.stage_script_hash))
            |> raw.replace(10, initial),
        },
      )
    } else {
      None
    }
  } else {
    let chunk_length = bounded.expected_chunk_length(item_length, chunk_index)
    let start = item_offset + chunk_index * bounded.chunk_bytes
    when output_span(control, start, chunk_length, witness) is {
      None -> None
      Some(chunk) -> {
        let next_peaks = merkle.append_leaf(
          chunk_index,
          peaks,
          bounded.hash_chunk(2, raw.integer(control, 2), chunk_index, chunk),
        )
        Some(
          Advanced {
            control: control
              |> raw.replace(8, builtin.i_data(chunk_index + 1))
              |> raw.replace(
                  9,
                  encoded_data(merkle.encode_frontier(next_peaks)),
                ),
          },
        )
      }
    }
  }
}

fn next_hash(
  control: raw.Frame,
  result: Option<blake.Blake2b224TraceControlV1>,
) -> Option<StepResult> {
  when result is {
    None -> None
    Some(next) ->
      Some(
        Advanced {
          control: raw.replace(
            control,
            10,
            optional_data(blake.encode_control_v1(next)),
          ),
        },
      )
  }
}

pub fn script_hash(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  expect raw.integer(control, 1) == proof.stage_script_hash
  let scan_control = scan.control_from_data_v1(raw.item(control, 5))
  expect Pair(0, [hash_data]) = builtin.un_constr_data(raw.item(control, 10))
  let hash_control = blake.control_from_data_v1(hash_data)
  expect hash_control.total_length == scan_control.reference_script_length + 1
  expect raw.item(control, 11) == builtin.constr_data(1, [])
  if hash_control.stage == blake.stage_terminal {
    if witness != proof.LedgerOutputProofNoWitness {
      None
    } else if scan_control.reference_script_language != 0 {
      Some(
        Advanced {
          control: raw.replace(control, 1, builtin.i_data(proof.stage_terminal)),
        },
      )
    } else if scan_control.reference_script_length == 0 {
      Some(InvalidReferenceScript)
    } else {
      let initial = native.initial_structure_control_v1(
        scan_control.reference_script_offset,
        scan_control.reference_script_length,
      )
        |> native.encode_structure_control_v1
        |> optional_data
      Some(
        Advanced {
          control: control
            |> raw.replace(1, builtin.i_data(proof.stage_native_script))
            |> raw.replace(11, initial),
        },
      )
    }
  } else if hash_control.stage == blake.stage_ready {
    let remaining = hash_control.total_length - hash_control.cursor
    let expected_length = if remaining < blake.block_bytes {
      remaining
    } else {
      blake.block_bytes
    }
    let includes_language = hash_control.cursor == 0
    let content_length = expected_length - if includes_language {
      1
    } else {
      0
    }
    let start = scan_control.reference_script_offset + hash_control.cursor - if includes_language {
      0
    } else {
      1
    }
    let content = if content_length == 0 {
      if witness == proof.LedgerOutputProofNoWitness {
        Some(#"")
      } else {
        None
      }
    } else {
      output_span(control, start, content_length, witness)
    }
    when content is {
      None -> None
      Some(bytes) -> {
        let block = if includes_language {
          #""
            |> bytearray.push(scan_control.reference_script_language)
            |> bytearray.concat(bytes)
        } else {
          bytes
        }
        next_hash(control, blake.step_v1(hash_control, Some(block)))
      }
    }
  } else if witness != proof.LedgerOutputProofNoWitness {
    None
  } else {
    next_hash(control, blake.step_v1(hash_control, None))
  }
}

fn map_native(
  control: raw.Frame,
  result: Option<native.NativeScriptStructureStepResultV1>,
) -> Option<StepResult> {
  when result is {
    None -> None
    Some(native.NativeScriptStructureAdvanced { control: next }) ->
      Some(
        Advanced {
          control: raw.replace(
            control,
            11,
            optional_data(native.encode_structure_control_v1(next)),
          ),
        },
      )
    Some(native.NativeScriptStructureInvalid) -> Some(InvalidReferenceScript)
    Some(native.NativeScriptStructureNodeLimit) -> Some(NativeScriptNodeLimit)
    Some(native.NativeScriptStructureDepthLimit) -> Some(NativeScriptDepthLimit)
  }
}

pub fn native_script(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  expect raw.integer(control, 1) == proof.stage_native_script
  expect Pair(0, [native_data]) = builtin.un_constr_data(raw.item(control, 11))
  let native_control = native.structure_control_from_data_v1(native_data)
  if native_control.stage == native.structure_stage_terminal {
    if witness == proof.LedgerOutputProofNoWitness {
      Some(
        Advanced {
          control: raw.replace(control, 1, builtin.i_data(proof.stage_terminal)),
        },
      )
    } else {
      None
    }
  } else if native_control.stage == native.structure_stage_token {
    when
      proof.authenticated_chunk_window_for_output(
        raw.integer(control, 2),
        raw.integer(control, 3),
        raw.bytes(control, 4),
        native_control.cursor,
        witness,
        True,
      )
    is {
      None -> None
      Some(Pair(window, offset)) ->
        map_native(
          control,
          native.structure_token_step_v1(native_control, window, offset),
        )
    }
  } else if native_control.stage == native.structure_stage_frame {
    when witness is {
      proof.LedgerOutputProofNativeFrame { frame } ->
        map_native(
          control,
          native.structure_frame_step_v1(native_control, frame),
        )
      _ -> None
    }
  } else if witness == proof.LedgerOutputProofNoWitness {
    map_native(control, native.finalize_structure_v1(native_control))
  } else {
    None
  }
}

pub fn initial_value_data(asset_count: Int) -> Data {
  expect asset_count >= 0 && asset_count <= 4294967295
  let empty = builtin.list_data(
    [
      builtin.b_data(data_summary.empty_data_pair_root_v1()),
      builtin.i_data(0),
      builtin.i_data(0),
      builtin.i_data(0),
    ],
  )
  builtin.list_data(
    [
      builtin.i_data(1),
      builtin.i_data(0),
      builtin.i_data(asset_count),
      builtin.b_data(#""),
      empty,
      empty,
      builtin.constr_data(1, []),
    ],
  )
}

fn structure_scan(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
  group: Int,
  transition: fn(scan.LedgerOutputScanControlV1, Int, ByteArray, Int) ->
    Option<scan.LedgerOutputScanControlV1>,
) -> Option<StepResult> {
  expect raw.integer(control, 1) == proof.stage_structure
  let current = scan.control_from_data_v1(raw.item(control, 5))
  expect
    if group == 0 {
      current.stage == scan.stage_required_fields || current.stage == scan.stage_value_header
    } else if
    group == 1 {
      current.stage == scan.stage_policy_header || current.stage == scan.stage_asset
    } else {
      current.stage >= scan.stage_optional_field && current.stage <= scan.stage_reference_script_payload
    }
  let total_length = raw.integer(control, 3)
  expect current.cursor <= total_length
  expect scan.finish_v1(current, total_length) == None
  expect
    raw.item(control, 6) == builtin.constr_data(1, []) && raw.item(control, 7) == builtin.constr_data(
      1,
      [],
    )
  expect
    raw.integer(control, 8) == 0 && raw.item(control, 9) == builtin.list_data(
      [],
    )
  expect
    raw.item(control, 10) == builtin.constr_data(1, []) && raw.item(control, 11) == builtin.constr_data(
      1,
      [],
    )
  when
    proof.authenticated_chunk_window_for_output(
      raw.integer(control, 2),
      total_length,
      raw.bytes(control, 4),
      current.cursor,
      witness,
      current.stage <= scan.stage_optional_field,
    )
  is {
    None -> None
    Some(Pair(window, offset)) ->
      when transition(current, total_length, window, offset) is {
        None -> Some(InvalidOutput)
        Some(next) ->
          Some(
            Advanced {
              control: raw.replace(
                control,
                5,
                encoded_data(scan.encode_control_v1(next)),
              ),
            },
          )
      }
  }
}

pub fn structure_headers(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  structure_scan(control, witness, 0, scan.step_headers)
}

pub fn structure_assets(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  structure_scan(control, witness, 1, scan.step_assets)
}

pub fn structure_optional(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  structure_scan(control, witness, 2, scan.step_optional)
}

pub fn structure_finish(
  control: raw.Frame,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Option<StepResult> {
  expect raw.integer(control, 1) == proof.stage_structure
  expect witness == proof.LedgerOutputProofNoWitness
  let current = scan.control_from_data_v1(raw.item(control, 5))
  let total_length = raw.integer(control, 3)
  expect
    raw.item(control, 6) == builtin.constr_data(1, []) && raw.item(control, 7) == builtin.constr_data(
      1,
      [],
    )
  expect
    raw.integer(control, 8) == 0 && raw.item(control, 9) == builtin.list_data(
      [],
    )
  expect
    raw.item(control, 10) == builtin.constr_data(1, []) && raw.item(control, 11) == builtin.constr_data(
      1,
      [],
    )
  if scan.terminal_is_exact_v1(current, total_length) {
    Some(
      Advanced {
        control: control
          |> raw.replace(1, builtin.i_data(proof.stage_value_fold))
          |> raw.replace(
              6,
              builtin.constr_data(0, [initial_value_data(current.asset_count)]),
            ),
      },
    )
  } else {
    when scan.finish_v1(current, total_length) is {
      None -> None
      Some(next) ->
        Some(
          Advanced {
            control: raw.replace(
              control,
              5,
              encoded_data(scan.encode_control_v1(next)),
            ),
          },
        )
    }
  }
}
