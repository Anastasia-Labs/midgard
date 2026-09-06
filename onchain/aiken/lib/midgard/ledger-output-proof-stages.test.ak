use aiken/builtin
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1 as blake
use midgard/bounded_item_v1
use midgard/cek_data_traverse_v1 as traverse
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as narrow
use midgard/ledger_output_proof_v1 as proof
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_value_v1 as value
use midgard/validation_merkle_v1 as merkle

fn span_claim(start: Int, length: Int) -> Data {
  builtin.constr_data(0, [builtin.i_data(start), builtin.i_data(length)])
}

fn no_claim() -> Data {
  builtin.constr_data(1, [])
}

/// The honest span claim the step-dispatcher action carries: exactly the span
/// the typed monolith reads at this control, `constr 1 []` when it reads none.
fn honest_span_claim(control: proof.LedgerOutputProofControlV1) -> Data {
  if control.stage == proof.stage_reference_script_commitment {
    let scan_control = control.output_scan
    let item_offset = scan_control.reference_script_item_offset
    let item_length = control.total_length - item_offset
    let chunk_count = bounded_item_v1.chunk_count(item_length)
    let chunk_index = control.reference_script_count
    if chunk_index == chunk_count {
      no_claim()
    } else {
      span_claim(
        item_offset + chunk_index * bounded_item_v1.chunk_bytes,
        bounded_item_v1.expected_chunk_length(item_length, chunk_index),
      )
    }
  } else if control.stage == proof.stage_script_hash {
    expect Some(hash_control) = control.script_hash
    if hash_control.stage == blake.stage_ready {
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
      if content_length == 0 {
        no_claim()
      } else {
        span_claim(
          control.output_scan.reference_script_offset + hash_control.cursor - if includes_language {
            0
          } else {
            1
          },
          content_length,
        )
      }
    } else {
      no_claim()
    }
  } else {
    no_claim()
  }
}

fn same_step(
  control: proof.LedgerOutputProofControlV1,
  witness: proof.LedgerOutputProofWitnessV1,
) -> Bool {
  let frame = raw.open(proof.encode_control_v1(control))
  let result = if control.stage == proof.stage_structure {
    if witness == proof.LedgerOutputProofNoWitness {
      narrow.structure_finish(frame, witness)
    } else if control.output_scan.stage <= scan.stage_value_header {
      narrow.structure_headers(frame, witness)
    } else if control.output_scan.stage <= scan.stage_asset {
      narrow.structure_assets(frame, witness)
    } else {
      narrow.structure_optional(frame, witness)
    }
  } else if control.stage == proof.stage_value_fold {
    narrow.value_fold(frame, witness)
  } else if control.stage == proof.stage_reference_script_commitment {
    narrow.reference_script(frame, witness, honest_span_claim(control))
  } else if control.stage == proof.stage_script_hash {
    narrow.script_hash(frame, witness, honest_span_claim(control))
  } else {
    narrow.native_script(frame, witness)
  }
  when proof.step_v1(control, witness) is {
    None -> result == None
    Some(proof.LedgerOutputProofInvalidOutput) ->
      result == Some(narrow.InvalidOutput)
    Some(proof.LedgerOutputProofAdvanced { control: next }) -> {
      expect Some(narrow.Advanced { control: raw_next }) = result
      raw.encode(raw_next) == proof.encode_control_v1(next)
    }
    Some(proof.LedgerOutputProofInvalidReferenceScript) ->
      result == Some(narrow.InvalidReferenceScript)
    Some(proof.LedgerOutputProofNativeScriptNodeLimit) ->
      result == Some(narrow.NativeScriptNodeLimit)
    Some(proof.LedgerOutputProofNativeScriptDepthLimit) ->
      result == Some(narrow.NativeScriptDepthLimit)
  }
}

fn check_chain(
  control: proof.LedgerOutputProofControlV1,
  chunks: proof.LedgerOutputProofWitnessV1,
  fuel: Int,
) -> Bool {
  expect fuel > 0
  if control.stage == proof.stage_datum_traversal || control.stage == proof.stage_terminal || control.stage == proof.stage_value_fold && control.output_scan.asset_count > 0 {
    True
  } else {
    let witness = when
      proof.step_v1(control, proof.LedgerOutputProofNoWitness)
    is {
      Some(_) -> proof.LedgerOutputProofNoWitness
      None -> chunks
    }
    and {
      same_step(control, witness),
      when proof.step_v1(control, witness) is {
        Some(proof.LedgerOutputProofAdvanced { control: next }) ->
          check_chain(next, chunks, fuel - 1)
        Some(proof.LedgerOutputProofInvalidOutput) -> True
        Some(proof.LedgerOutputProofInvalidReferenceScript) -> True
        _ -> False
      },
    }
  }
}

fn output_chain(output_index: Int, output: ByteArray) -> Bool {
  let frontier = merkle.append_leaf(
    0,
    merkle.empty_frontier(),
    bounded_item_v1.hash_chunk(2, output_index, 0, output),
  )
  let length = bytearray.length(output)
  let commitment = bounded_item_v1.commitment(2, output_index, length, frontier)
  let initial = proof.initial_control_v1(output_index, length, commitment)
  let chunk = bounded_item_v1.ChunkProofV1 {
    version: 1,
    field_index: 2,
    item_index: output_index,
    total_length: length,
    chunk_index: 0,
    chunk: output,
    frontier,
    siblings: [],
  }
  check_chain(
    initial,
    proof.LedgerOutputProofChunks { chunk_proof: chunk, next_chunk_proof: None },
    1024,
  )
}

test structure_raw_matches_all_empty_output_steps(
  index via fuzz.int_between(0, 65535),
) {
  output_chain(
    index,
    #"a200581d7811111111111111111111111111111111111111111111111111111111018200a0",
  )
}

test structure_raw_matches_reference_script_steps() {
  output_chain(
    0,
    #"a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b",
  )
}

test structure_raw_matches_invalid_output() {
  output_chain(
    0,
    #"b80200581d7811111111111111111111111111111111111111111111111111111111018200a0",
  )
}

test initial_datum_template_matches_typed(
  args via fuzz.tuple(
    fuzz.int_between(0, 4294967295),
    fuzz.int_between(1, 4294967295),
  ),
) {
  let (offset, length) = args
  raw.encode_item(narrow.initial_datum_data(offset, length)) == traverse.encode_control_v1(
    traverse.initial_control_v1(offset, length),
  )
}

test value_raw_matches_datum_transition() {
  output_chain(
    0,
    #"a300581d7811111111111111111111111111111111111111111111111111111111018200a00243d87980",
  )
}

test raw_matches_native_timelock_chain() {
  output_chain(
    0,
    #"a300581d7811111111111111111111111111111111111111111111111111111111018200a003820043820401",
  )
}

test initial_value_template_matches_typed(
  asset_count via fuzz.int_between(0, 4294967295),
) {
  raw.encode_item(narrow.initial_value_data(asset_count)) == value.encode_control_v1(
    value.initial_control_v1(asset_count),
  )
}

test raw_matches_policy_and_asset_scan() {
  output_chain(
    0,
    #"a200581d7811111111111111111111111111111111111111111111111111111111018200a1581c55555555555555555555555555555555555555555555555555555555a141aa01",
  )
}
