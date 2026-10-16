use aiken/builtin
use aiken/collection/list
use aiken/crypto
use aiken/fuzz
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_frame_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_datum as narrow_datum
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as narrow
use midgard/ledger_output_proof_v1
use midgard/script_context_v1
use midgard/validation_merkle_v1

const no_reference_output =
  #"a200581d7811111111111111111111111111111111111111111111111111111111018200a0"

const small_reference_output =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b"

const small_datum_output =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a00243d87980"

const malformed_nonminimal_output =
  #"b80200581d7811111111111111111111111111111111111111111111111111111111018200a0"

const cross_language_long_terminal_control =
  #"91010600192bf15820df9ca61ac2b387114175c5666592059ef0258587263fc76c7be14b469c8d347b970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820de4ab123673d8a9958247e2b709508944a6f64a4db4c7208d0a871d3358804c01854191427000319147c191481191770d8799f8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f8358201779e0023b9273af30dcfadb5e5ec557763a8c2c3af34f57fb2a2ad59f50ada018301852ffffd8799f890107185419142719142740d87a80d87a80d8799f8358202aa2efa1446b0d53ad8a806d29396c82aca037037c095811d7948f5304d3896219142719138cffff02818201582028f935b37d798dd5f68f23fa40e9d9dd02037d6b1e1fa7ad7edfdcb84b63a26cd8799f8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000ffd87a80d87a80d87a80d87a80d87a80d87a80"

fn one_chunk_material(
  output_index: Int,
  output: ByteArray,
) -> Pair<ByteArray, bounded_item_v1.ChunkProofV1> {
  let frontier = validation_merkle_v1.append_leaf(
    0,
    validation_merkle_v1.empty_frontier(),
    bounded_item_v1.hash_chunk(2, output_index, 0, output),
  )
  let commitment = bounded_item_v1.commitment(
    2,
    output_index,
    bytearray.length(output),
    frontier,
  )
  Pair(
    commitment,
    bounded_item_v1.ChunkProofV1 {
      version: 1,
      field_index: 2,
      item_index: output_index,
      total_length: bytearray.length(output),
      chunk_index: 0,
      chunk: output,
      frontier,
      siblings: [],
    },
  )
}

fn chunks(
  proof: bounded_item_v1.ChunkProofV1,
) -> ledger_output_proof_v1.LedgerOutputProofWitnessV1 {
  ledger_output_proof_v1.LedgerOutputProofChunks {
    chunk_proof: proof,
    next_chunk_proof: None,
  }
}

fn datum(
  action: cek_data_traverse_v1.DataTraverseActionV1,
  window: Option<ByteArray>,
) -> ledger_output_proof_v1.LedgerOutputProofWitnessV1 {
  ledger_output_proof_v1.LedgerOutputProofDatum { action, window }
}

/// A span-attach witness over a one-chunk output. The window itself is
/// derived from the pre-control's demanded span.
fn span_attach(
  proof: bounded_item_v1.ChunkProofV1,
) -> ledger_output_proof_v1.LedgerOutputProofWitnessV1 {
  ledger_output_proof_v1.LedgerOutputProofSpanAttach {
    chunk_proof: proof,
    next_chunk_proof: None,
  }
}

/// The bytes of the window `control` records, sliced out of `output`.
fn recorded_window(
  control: ledger_output_proof_v1.LedgerOutputProofControlV1,
  output: ByteArray,
) -> ByteArray {
  expect Some(window) = control.span_window
  builtin.slice_bytearray(window.start, window.length, output)
}

fn advanced(
  result: Option<ledger_output_proof_v1.LedgerOutputProofStepResultV1>,
) -> ledger_output_proof_v1.LedgerOutputProofControlV1 {
  expect Some(ledger_output_proof_v1.LedgerOutputProofAdvanced { control }) = result
  control
}

fn finish_empty_value(
  scan_terminal: ledger_output_proof_v1.LedgerOutputProofControlV1,
) -> ledger_output_proof_v1.LedgerOutputProofControlV1 {
  let value_ready = advanced(
    ledger_output_proof_v1.step_v1(
      scan_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let value_finalize = advanced(
    ledger_output_proof_v1.step_v1(
      value_ready,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let value_terminal = advanced(
    ledger_output_proof_v1.step_v1(
      value_finalize,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  advanced(
    ledger_output_proof_v1.step_v1(
      value_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
}

test authenticates_a_complete_output_without_a_reference_script() {
  let Pair(commitment, proof) = one_chunk_material(0, no_reference_output)
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(no_reference_output),
    commitment,
  )
  let value_header = advanced(
    ledger_output_proof_v1.step_v1(initial, chunks(proof)),
  )
  let optional_field = advanced(
    ledger_output_proof_v1.step_v1(value_header, chunks(proof)),
  )
  let scan_terminal = advanced(
    ledger_output_proof_v1.step_v1(
      optional_field,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let terminal = finish_empty_value(scan_terminal)
  expect Some(cardano_tx_out) = script_context_v1.tx_out_data_v1(
    no_reference_output,
    False,
  )
  expect Some(midgard_tx_out) = script_context_v1.tx_out_data_v1(
    no_reference_output,
    True,
  )
  expect Some(cardano_tx_out_summary) = ledger_output_proof_v1.cardano_tx_out_summary_v1(
    terminal,
  )
  expect Some(midgard_tx_out_summary) = ledger_output_proof_v1.midgard_tx_out_summary_v1(
    terminal,
  )
  expect Some(cardano_spend_datum_summary) = ledger_output_proof_v1.cardano_spend_datum_summary_v1(
    terminal,
  )
  let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: terminal.output_index,
    total_length: terminal.total_length,
    item_commitment: terminal.item_commitment,
    address: terminal.output_scan.address,
    lovelace: terminal.output_scan.lovelace,
    asset_count: terminal.output_scan.asset_count,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
      terminal.output_scan.asset_count,
      terminal.output_scan.asset_peaks,
    ),
    cardano_value_size: terminal.output_scan.cardano_value_size,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: cardano_tx_out_summary,
    midgard_tx_out: midgard_tx_out_summary,
    cardano_spend_datum: cardano_spend_datum_summary,
  }
  and {
    ledger_output_proof_v1.terminal_is_exact_v1(terminal),
    terminal.output_scan.address == #"7811111111111111111111111111111111111111111111111111111111",
    terminal.output_scan.lovelace == 0,
    terminal.script_hash == None,
    ledger_output_proof_v1.value_summary_v1(terminal) == Some(
      cek_data_v1.semantic_data_summary_v1(builtin.map_data([])),
    ),
    ledger_output_proof_v1.cardano_tx_out_summary_v1(terminal) == Some(
      cek_data_v1.semantic_data_summary_v1(cardano_tx_out),
    ),
    ledger_output_proof_v1.midgard_tx_out_summary_v1(terminal) == Some(
      cek_data_v1.semantic_data_summary_v1(midgard_tx_out),
    ),
    cardano_tx_out != midgard_tx_out,
    ledger_output_proof_v1.terminal_descriptor_v1(terminal) == Some(descriptor),
    // The three attachment groups record the facts of the derived
    // descriptor; the thin terminal gate accepts exactly its bytes.
    {
      expect Some(leaves) = ledger_output_proof_v1.fact_attach_v1(terminal)
      expect Some(reference) = ledger_output_proof_v1.fact_attach_v1(leaves)
      expect Some(facts) = ledger_output_proof_v1.fact_attach_v1(reference)
      and {
        ledger_output_proof_v1.fact_attach_v1(facts) == None,
        ledger_output_proof_v1.facts_are_exact_v1(
          facts,
          ledger_output_commitment_v1.encode(descriptor),
        ),
        !ledger_output_proof_v1.facts_are_exact_v1(
          facts,
          ledger_output_commitment_v1.encode(
            ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
              ..descriptor,
              lovelace: descriptor.lovelace + 1,
            },
          ),
        ),
        !ledger_output_proof_v1.facts_are_exact_v1(
          reference,
          ledger_output_commitment_v1.encode(descriptor),
        ),
      }
    },
  }
}

test derives_the_first_script_hash_block_from_the_output_chunk() {
  let Pair(commitment, proof) = one_chunk_material(0, small_reference_output)
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(small_reference_output),
    commitment,
  )
  let value_header = advanced(
    ledger_output_proof_v1.step_v1(initial, chunks(proof)),
  )
  let optional_field = advanced(
    ledger_output_proof_v1.step_v1(value_header, chunks(proof)),
  )
  let script_payload = advanced(
    ledger_output_proof_v1.step_v1(optional_field, chunks(proof)),
  )
  let optional_complete = advanced(
    ledger_output_proof_v1.step_v1(script_payload, chunks(proof)),
  )
  let scan_terminal = advanced(
    ledger_output_proof_v1.step_v1(
      optional_complete,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let reference_ready = finish_empty_value(scan_terminal)
  let span_attached = advanced(
    ledger_output_proof_v1.step_v1(reference_ready, span_attach(proof)),
  )
  let reference_committed = advanced(
    ledger_output_proof_v1.step_v1(
      span_attached,
      ledger_output_proof_v1.LedgerOutputProofWindow {
        bytes: recorded_window(span_attached, small_reference_output),
      },
    ),
  )
  let hash_ready = advanced(
    ledger_output_proof_v1.step_v1(
      reference_committed,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let hash_round = advanced(
    ledger_output_proof_v1.step_v1(
      hash_ready,
      ledger_output_proof_v1.LedgerOutputProofWindow {
        bytes: recorded_window(hash_ready, small_reference_output),
      },
    ),
  )
  expect Some(hash_control) = hash_round.script_hash
  and {
    reference_committed.reference_script_count == 1,
    ledger_output_proof_v1.reference_script_item_commitment_v1(
      reference_committed,
    ) == None,
    hash_round.stage == ledger_output_proof_v1.stage_script_hash,
    hash_control.stage == 1,
    hash_control.active_block_length == 4,
    builtin.slice_bytearray(0, 4, hash_control.active_block) == #"036b6b6b",
  }
}

test authenticates_the_inline_datum_semantics_from_output_chunks() {
  let Pair(commitment, proof) = one_chunk_material(0, small_datum_output)
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(small_datum_output),
    commitment,
  )
  let value_header = advanced(
    ledger_output_proof_v1.step_v1(initial, chunks(proof)),
  )
  let optional_field = advanced(
    ledger_output_proof_v1.step_v1(value_header, chunks(proof)),
  )
  let datum_payload = advanced(
    ledger_output_proof_v1.step_v1(optional_field, chunks(proof)),
  )
  let optional_complete = advanced(
    ledger_output_proof_v1.step_v1(datum_payload, chunks(proof)),
  )
  let scan_terminal = advanced(
    ledger_output_proof_v1.step_v1(
      optional_complete,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let datum_ready = finish_empty_value(scan_terminal)
  let datum_windowed = advanced(
    ledger_output_proof_v1.step_v1(datum_ready, span_attach(proof)),
  )
  let datum_opened = advanced(
    ledger_output_proof_v1.step_v1(
      datum_windowed,
      datum(
        cek_data_traverse_v1.HeadSequence,
        Some(recorded_window(datum_windowed, small_datum_output)),
      ),
    ),
  )
  let frame = cek_data_frame_v1.initial_small_constr_frame_v1(0, #"")
  let datum_authenticated = advanced(
    ledger_output_proof_v1.step_v1(
      datum_opened,
      datum(cek_data_traverse_v1.FinalizeFrame { frame, parent: None }, None),
    ),
  )
  let raw_witness: Data = datum(
    cek_data_traverse_v1.FinalizeFrame { frame, parent: None },
    None,
  )
  expect Some(narrow.Advanced { control: raw_datum_authenticated }) = narrow_datum.finalize_frame(
    raw.open(ledger_output_proof_v1.encode_control_v1(datum_opened)),
    raw_witness,
  )
  expect
    raw.encode(raw_datum_authenticated) == ledger_output_proof_v1.encode_control_v1(
      datum_authenticated,
    )
  let terminal = advanced(
    ledger_output_proof_v1.step_v1(
      datum_authenticated,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let expected_spend_datum = builtin.constr_data(
    0,
    [builtin.constr_data(0, [])],
  )
    |> cek_data_v1.semantic_data_summary_v1
  and {
    datum_ready.stage == ledger_output_proof_v1.stage_datum_traversal,
    ledger_output_proof_v1.cardano_spend_datum_summary_v1(terminal) == Some(
      expected_spend_datum,
    ),
    ledger_output_proof_v1.terminal_is_exact_v1(terminal),
  }
}

test reports_authenticated_invalid_output_bytes() {
  let Pair(commitment, proof) = one_chunk_material(
    0,
    malformed_nonminimal_output,
  )
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(malformed_nonminimal_output),
    commitment,
  )
  ledger_output_proof_v1.step_v1(initial, chunks(proof)) == Some(
    ledger_output_proof_v1.LedgerOutputProofInvalidOutput,
  )
}

test fails_closed_for_a_substituted_chunk() {
  let Pair(commitment, proof) = one_chunk_material(0, no_reference_output)
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(no_reference_output),
    commitment,
  )
  let substituted = bounded_item_v1.ChunkProofV1 {
    ..proof,
    chunk: malformed_nonminimal_output,
  }
  ledger_output_proof_v1.step_v1(initial, chunks(substituted)) == None
}

test fails_closed_for_a_substituted_reference_script_item_chunk() {
  let Pair(commitment, proof) = one_chunk_material(0, small_reference_output)
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(small_reference_output),
    commitment,
  )
  let value_header = advanced(
    ledger_output_proof_v1.step_v1(initial, chunks(proof)),
  )
  let optional_field = advanced(
    ledger_output_proof_v1.step_v1(value_header, chunks(proof)),
  )
  let script_payload = advanced(
    ledger_output_proof_v1.step_v1(optional_field, chunks(proof)),
  )
  let optional_complete = advanced(
    ledger_output_proof_v1.step_v1(script_payload, chunks(proof)),
  )
  let scan_terminal = advanced(
    ledger_output_proof_v1.step_v1(
      optional_complete,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let reference_ready = finish_empty_value(scan_terminal)
  let substituted = bounded_item_v1.ChunkProofV1 {
    ..proof,
    chunk: builtin.replicate_byte(bytearray.length(proof.chunk), 0),
  }
  let span_attached = advanced(
    ledger_output_proof_v1.step_v1(reference_ready, span_attach(proof)),
  )
  and {
    // A substituted chunk fails the span-attach merkle check.
    ledger_output_proof_v1.step_v1(
      reference_ready,
      ledger_output_proof_v1.LedgerOutputProofSpanAttach {
        chunk_proof: substituted,
        next_chunk_proof: None,
      },
    ) == None,
    // Window bytes that do not hash to the recorded digest are refused at the
    // consuming step.
    ledger_output_proof_v1.step_v1(
      span_attached,
      ledger_output_proof_v1.LedgerOutputProofWindow {
        bytes: builtin.replicate_byte(
          bytearray.length(
            recorded_window(span_attached, small_reference_output),
          ),
          0,
        ),
      },
    ) == None,
  }
}

test fails_closed_for_a_substituted_inline_datum_item_chunk() {
  let Pair(commitment, proof) = one_chunk_material(0, small_datum_output)
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(small_datum_output),
    commitment,
  )
  let value_header = advanced(
    ledger_output_proof_v1.step_v1(initial, chunks(proof)),
  )
  let optional_field = advanced(
    ledger_output_proof_v1.step_v1(value_header, chunks(proof)),
  )
  let datum_payload = advanced(
    ledger_output_proof_v1.step_v1(optional_field, chunks(proof)),
  )
  let optional_complete = advanced(
    ledger_output_proof_v1.step_v1(datum_payload, chunks(proof)),
  )
  let scan_terminal = advanced(
    ledger_output_proof_v1.step_v1(
      optional_complete,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let datum_ready = finish_empty_value(scan_terminal)
  let substituted = bounded_item_v1.ChunkProofV1 {
    ..proof,
    chunk: builtin.replicate_byte(bytearray.length(proof.chunk), 0),
  }
  let datum_windowed = advanced(
    ledger_output_proof_v1.step_v1(datum_ready, span_attach(proof)),
  )
  and {
    // A substituted chunk fails the span-attach merkle check.
    ledger_output_proof_v1.step_v1(
      datum_ready,
      ledger_output_proof_v1.LedgerOutputProofSpanAttach {
        chunk_proof: substituted,
        next_chunk_proof: None,
      },
    ) == None,
    // Tampered window bytes are refused by the datum head's window binding.
    ledger_output_proof_v1.step_v1(
      datum_windowed,
      datum(
        cek_data_traverse_v1.HeadSequence,
        Some(
          builtin.replicate_byte(
            bytearray.length(
              recorded_window(datum_windowed, small_datum_output),
            ),
            0,
          ),
        ),
      ),
    ) == None,
    // A datum step without the span-attach step has no window to bind to.
    ledger_output_proof_v1.step_v1(
      datum_ready,
      datum(cek_data_traverse_v1.HeadSequence, Some(small_datum_output)),
    ) == None,
  }
}

test decodes_the_long_typescript_terminal_control_canonically() {
  let control = ledger_output_proof_v1.decode_control_v1(
    cross_language_long_terminal_control,
  )
  expect Some(spend_datum_summary) = ledger_output_proof_v1.cardano_spend_datum_summary_v1(
    control,
  )
  expect Some(reference_item_commitment) = ledger_output_proof_v1.reference_script_item_commitment_v1(
    control,
  )
  expect Some(cardano_tx_out) = ledger_output_proof_v1.cardano_tx_out_summary_v1(
    control,
  )
  expect Some(midgard_tx_out) = ledger_output_proof_v1.midgard_tx_out_summary_v1(
    control,
  )
  and {
    ledger_output_proof_v1.terminal_is_exact_v1(control),
    control.reference_script_count == 2,
    bytearray.length(spend_datum_summary.root) == 32,
    bytearray.length(reference_item_commitment) == 32,
    bytearray.length(cardano_tx_out.root) == 32,
    bytearray.length(midgard_tx_out.root) == 32,
    cardano_tx_out.root != midgard_tx_out.root,
    ledger_output_proof_v1.reference_script_digest_v1(control) == Some(
      #"634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098",
    ),
    ledger_output_proof_v1.encode_control_v1(control) == cross_language_long_terminal_control,
  }
}

test initial_control_specialized_encoder_matches_generic(
  args via fuzz.tuple3(
    fuzz.int_at_least(0),
    fuzz.int_at_least(1),
    fuzz.bytearray_fixed(32),
  ),
) {
  let (index, length, commitment) = args
  ledger_output_proof_v1.encode_initial_control_v1(index, length, commitment) == ledger_output_proof_v1.encode_control_v1(
    ledger_output_proof_v1.initial_control_v1(index, length, commitment),
  )
}

/// The window binding returns exactly the authenticated span over the honest
/// window bytes, for every demanded span the window covers; out-of-window
/// spans, oversized or truncated bytes, and tampered bytes are refused.
test bound_window_bytes_binds_the_recorded_digest(
  args via fuzz.tuple(fuzz.int_between(-3, 60), fuzz.int_between(-1, 60)),
) {
  let (start, length) = args
  let Pair(commitment, proof) = one_chunk_material(0, small_datum_output)
  let total = bytearray.length(small_datum_output)
  let window = Some(
    ledger_output_proof_v1.LedgerOutputSpanWindowV1 {
      start: 0,
      length: total,
      digest: crypto.blake2b_256(small_datum_output),
    },
  )
  let bound = ledger_output_proof_v1.bound_window_bytes_v1(
    window,
    start,
    length,
    small_datum_output,
  )
  let authenticated = ledger_output_proof_v1.authenticated_output_span_for_output(
    0,
    total,
    commitment,
    start,
    length,
    chunks(proof),
  )
  and {
    bound == authenticated,
    ledger_output_proof_v1.bound_window_bytes_v1(
      window,
      start,
      length,
      builtin.replicate_byte(total, 0),
    ) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(
      None,
      start,
      length,
      small_datum_output,
    ) == None,
  }
}

// Spans over multi-chunk outputs. Field 2 items of 8,000 bytes (chunks of
// 4,095 and 3,905 bytes) and 9,000 bytes (4,095, 4,095 and 810), byte i being
// i mod 256.

const span_ramp =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"

fn repeat_span_ramp(count: Int) -> ByteArray {
  if count <= 0 {
    #""
  } else {
    bytearray.concat(span_ramp, repeat_span_ramp(count - 1))
  }
}

fn ramp_item(length: Int) -> ByteArray {
  bytearray.take(repeat_span_ramp(length / 256 + 1), length)
}

/// The commitment and the chunk proofs, in chunk order, of a two- or
/// three-chunk field 2 item at index 0.
fn ramp_item_material(
  item: ByteArray,
) -> Pair<ByteArray, List<bounded_item_v1.ChunkProofV1>> {
  let total_length = bytearray.length(item)
  let count = bounded_item_v1.chunk_count(total_length)
  let chunk_list = list.indexed_map(
    list.repeat(Void, count),
    fn(index, _) {
      let start = index * bounded_item_v1.chunk_bytes
      bytearray.slice(
        item,
        start,
        start + bounded_item_v1.expected_chunk_length(total_length, index) - 1,
      )
    },
  )
  let leaves = list.indexed_map(
    chunk_list,
    fn(index, chunk) { bounded_item_v1.hash_chunk(2, 0, index, chunk) },
  )
  let frontier = validation_merkle_v1.build_frontier(leaves).peaks
  let siblings = when leaves is {
    [first, second] -> [[second], [first]]
    [first, second, _] -> [[second], [first], []]
    _ -> fail
  }
  Pair(
    bounded_item_v1.commitment(2, 0, total_length, frontier),
    list.indexed_map(
      list.zip(chunk_list, siblings),
      fn(index, pair) {
        let (chunk, chunk_siblings) = pair
        bounded_item_v1.ChunkProofV1 {
          version: 1,
          field_index: 2,
          item_index: 0,
          total_length,
          chunk_index: index,
          chunk,
          frontier,
          siblings: chunk_siblings,
        }
      },
    ),
  )
}

fn item_span(item: ByteArray, start: Int, length: Int) -> ByteArray {
  bytearray.slice(item, start, start + length - 1)
}

fn span_of(
  item: ByteArray,
  commitment: ByteArray,
  start: Int,
  length: Int,
  first: bounded_item_v1.ChunkProofV1,
  next: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<ByteArray> {
  ledger_output_proof_v1.authenticated_output_span_for_output(
    0,
    bytearray.length(item),
    commitment,
    start,
    length,
    ledger_output_proof_v1.LedgerOutputProofChunks {
      chunk_proof: first,
      next_chunk_proof: next,
    },
  )
}

test two_chunk_proofs_verify() {
  let item = ramp_item(8_000)
  let Pair(commitment, proofs) = ramp_item_material(item)
  list.all(
    proofs,
    fn(proof) { bounded_item_v1.verify_chunk(commitment, proof) },
  )
}

test three_chunk_proofs_verify() {
  let item = ramp_item(9_000)
  let Pair(commitment, proofs) = ramp_item_material(item)
  and {
    list.length(proofs) == 3,
    list.all(
      proofs,
      fn(proof) { bounded_item_v1.verify_chunk(commitment, proof) },
    ),
  }
}

test output_span_reads_a_two_chunk_span_exactly() {
  let item = ramp_item(8_000)
  let Pair(commitment, proofs) = ramp_item_material(item)
  expect [first, second] = proofs
  and {
    span_of(item, commitment, 5, 4_095, first, Some(second)) == Some(
      item_span(item, 5, 4_095),
    ),
    span_of(item, commitment, 4_000, 100, first, Some(second)) == Some(
      item_span(item, 4_000, 100),
    ),
    span_of(item, commitment, 4_094, 2, first, Some(second)) == Some(
      item_span(item, 4_094, 2),
    ),
    span_of(item, commitment, 3_905, 4_095, first, Some(second)) == Some(
      item_span(item, 3_905, 4_095),
    ),
  }
}

test output_span_reads_a_one_chunk_span_exactly() {
  let item = ramp_item(8_000)
  let Pair(commitment, proofs) = ramp_item_material(item)
  expect [first, second] = proofs
  and {
    span_of(item, commitment, 0, 4_095, first, None) == Some(
      item_span(item, 0, 4_095),
    ),
    span_of(item, commitment, 4_094, 1, first, None) == Some(
      item_span(item, 4_094, 1),
    ),
    span_of(item, commitment, 4_095, 3_905, second, None) == Some(
      item_span(item, 4_095, 3_905),
    ),
    span_of(item, commitment, 7_999, 1, second, None) == Some(
      item_span(item, 7_999, 1),
    ),
  }
}

test output_span_refuses_a_missing_extra_or_wrong_proof() {
  let item = ramp_item(8_000)
  let Pair(commitment, proofs) = ramp_item_material(item)
  expect [first, second] = proofs
  and {
    span_of(item, commitment, 5, 4_095, first, None) == None,
    span_of(item, commitment, 5, 4_095, first, Some(first)) == None,
    span_of(item, commitment, 5, 4_095, second, Some(second)) == None,
    span_of(item, commitment, 5, 4_095, second, Some(first)) == None,
    span_of(item, commitment, 0, 4_095, first, Some(second)) == None,
    span_of(item, commitment, 4_095, 10, first, None) == None,
    span_of(item, commitment, 0, 10, second, None) == None,
    span_of(
      item,
      commitment,
      5,
      4_095,
      first,
      Some(bounded_item_v1.ChunkProofV1 { ..second, chunk: first.chunk }),
    ) == None,
  }
}

test output_span_refuses_a_span_out_of_bounds() {
  let item = ramp_item(8_000)
  let Pair(commitment, proofs) = ramp_item_material(item)
  expect [first, second] = proofs
  and {
    span_of(item, commitment, 0, 4_096, first, Some(second)) == None,
    span_of(item, commitment, 5, 4_096, first, Some(second)) == None,
    span_of(item, commitment, 7_999, 2, second, None) == None,
    span_of(item, commitment, 7_000, 1_001, second, None) == None,
    span_of(item, commitment, 5, 0, first, None) == None,
    span_of(item, commitment, 5, 0, first, Some(second)) == None,
    span_of(item, commitment, 4_095, 0, second, None) == None,
    span_of(item, commitment, -1, 10, first, None) == None,
    span_of(item, commitment, -1, 10, first, Some(second)) == None,
  }
}

test output_span_reads_three_chunk_items_one_chunk_pair_at_a_time() {
  let item = ramp_item(9_000)
  let Pair(commitment, proofs) = ramp_item_material(item)
  expect [first, second, third] = proofs
  and {
    span_of(item, commitment, 4_095, 4_095, second, None) == Some(
      item_span(item, 4_095, 4_095),
    ),
    span_of(item, commitment, 4_096, 4_095, second, Some(third)) == Some(
      item_span(item, 4_096, 4_095),
    ),
    span_of(item, commitment, 8_000, 1_000, second, Some(third)) == Some(
      item_span(item, 8_000, 1_000),
    ),
    span_of(item, commitment, 8_190, 810, third, None) == Some(
      item_span(item, 8_190, 810),
    ),
    span_of(item, commitment, 4_094, 4_097, first, Some(second)) == None,
    span_of(item, commitment, 4_094, 4_097, first, Some(third)) == None,
    span_of(item, commitment, 1, 8_190, first, Some(second)) == None,
    span_of(item, commitment, 4_096, 4_095, second, None) == None,
    span_of(item, commitment, 4_096, 4_095, first, Some(third)) == None,
  }
}

fn full_window(
  item: ByteArray,
) -> ledger_output_proof_v1.LedgerOutputSpanWindowV1 {
  ledger_output_proof_v1.LedgerOutputSpanWindowV1 {
    start: 5,
    length: 4_095,
    digest: crypto.blake2b_256(item_span(item, 5, 4_095)),
  }
}

test bound_window_reads_spans_inside_a_full_window_exactly() {
  let item = ramp_item(8_000)
  let window = Some(full_window(item))
  let bytes = item_span(item, 5, 4_095)
  and {
    ledger_output_proof_v1.bound_window_bytes_v1(window, 5, 4_095, bytes) == Some(
      bytes,
    ),
    ledger_output_proof_v1.bound_window_bytes_v1(window, 4_000, 100, bytes) == Some(
      item_span(item, 4_000, 100),
    ),
    ledger_output_proof_v1.bound_window_bytes_v1(window, 4_099, 1, bytes) == Some(
      item_span(item, 4_099, 1),
    ),
    ledger_output_proof_v1.bound_window_bytes_v1(window, 5, 1, bytes) == Some(
      item_span(item, 5, 1),
    ),
    ledger_output_proof_v1.window_covers_v1(window, 5, 4_095),
    ledger_output_proof_v1.window_covers_v1(window, 4_099, 1),
  }
}

test bound_window_refuses_spans_outside_a_full_window() {
  let item = ramp_item(8_000)
  let window = Some(full_window(item))
  let bytes = item_span(item, 5, 4_095)
  and {
    ledger_output_proof_v1.bound_window_bytes_v1(window, 4_099, 2, bytes) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(window, 4, 1, bytes) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(window, 4, 2, bytes) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(window, 5, 4_096, bytes) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(window, 5, 0, bytes) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(None, 5, 1, bytes) == None,
    !ledger_output_proof_v1.window_covers_v1(window, 5, 0),
    !ledger_output_proof_v1.window_covers_v1(window, 4_100, 0),
    !ledger_output_proof_v1.window_covers_v1(window, 4_099, 2),
    !ledger_output_proof_v1.window_covers_v1(window, 4, 1),
    !ledger_output_proof_v1.window_covers_v1(None, 5, 1),
  }
}

test bound_window_refuses_bytes_other_than_the_recorded_window() {
  let item = ramp_item(8_000)
  let window = Some(full_window(item))
  let bytes = item_span(item, 5, 4_095)
  and {
    ledger_output_proof_v1.bound_window_bytes_v1(
      window,
      5,
      10,
      bytearray.take(bytes, 4_094),
    ) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(
      window,
      5,
      10,
      bytearray.concat(bytes, #"00"),
    ) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(
      window,
      5,
      10,
      bytearray.concat(#"ff", bytearray.drop(bytes, 1)),
    ) == None,
    ledger_output_proof_v1.bound_window_bytes_v1(
      window,
      5,
      10,
      item_span(item, 6, 4_095),
    ) == None,
  }
}

/// A datum-traversal control recorded by the TypeScript builder over a
/// 5,204-byte output (a 5,000-byte indefinite byte string datum), right after
/// its first span attach recorded the window [45, 4,140).
const recorded_window_control =
  #"910102001914545820ea7b4cad74c02730ee2f8a29afd5ca9e738cc0988de2ee9cfe469138049098d79701071914540301581d78111111111111111111111111111111111111111111111111111111111a001e8480050000004040400080182d1914270020202000d8799f8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f8358205db969a5b6a01d8ed8cf56107fbf827cadb0870b2aa233e24cc566840cefbc43091819ffffd8799f890100182d1914270040d87a80d87a80d87a80ff0080d87a80d87a80d8799f83182d190fff582063ef556085161995fc2a133a6e9f67fd1aa20bf5c1930b403ca1cf6c00f01babffd87a80d87a80d87a80d87a80"

fn with_window(
  start: Int,
  length: Int,
  digest_length: Int,
) -> ledger_output_proof_v1.LedgerOutputProofControlV1 {
  let control = ledger_output_proof_v1.decode_control_v1(
    recorded_window_control,
  )
  ledger_output_proof_v1.LedgerOutputProofControlV1 {
    ..control,
    span_window: Some(
      ledger_output_proof_v1.LedgerOutputSpanWindowV1 {
        start,
        length,
        digest: bytearray.take(repeat_span_ramp(1), digest_length),
      },
    ),
  }
}

test recorded_window_control_is_well_formed() {
  let control = ledger_output_proof_v1.decode_control_v1(
    recorded_window_control,
  )
  and {
    control.total_length == 5_204,
    ledger_output_proof_v1.control_is_well_formed(control),
    ledger_output_proof_v1.control_is_well_formed(with_window(45, 4_095, 32)),
    ledger_output_proof_v1.control_is_well_formed(with_window(0, 1, 32)),
    ledger_output_proof_v1.control_is_well_formed(
      with_window(5_204 - 4_095, 4_095, 32),
    ),
  }
}

test control_window_must_be_one_chunk_inside_the_output() {
  and {
    !ledger_output_proof_v1.control_is_well_formed(with_window(45, 4_096, 32)),
    !ledger_output_proof_v1.control_is_well_formed(with_window(45, 0, 32)),
    !ledger_output_proof_v1.control_is_well_formed(
      with_window(5_204 - 4_094, 4_095, 32),
    ),
    !ledger_output_proof_v1.control_is_well_formed(with_window(-1, 4_095, 32)),
    !ledger_output_proof_v1.control_is_well_formed(with_window(45, 4_095, 31)),
  }
}
