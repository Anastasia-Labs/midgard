use aiken/builtin
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_frame_v1
use midgard/cek_data_traverse_v1
use midgard/cek_data_v1
use midgard/ledger_output_commitment_v1
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
  #"8c010600192bf15820a023c9459077b4fc906660cacfa81a46eea15b9ad1f21fb20fbd745d2678f9ec970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c191481191770d8799f8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f835820f1f2c712b57f2a363ba09d8673a2cf47b8688568395388f1baf86ce7f477c23618301852ffffd8799f8a0107185419142719142740d87a80d87a80d87a80d8799f8358202aa2efa1446b0d53ad8a806d29396c82aca037037c095811d7948f5304d3896219142719138cffff02818201582028f935b37d798dd5f68f23fa40e9d9dd02037d6b1e1fa7ad7edfdcb84b63a26cd8799f8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000ffd87a80"

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
  proof: Option<bounded_item_v1.ChunkProofV1>,
) -> ledger_output_proof_v1.LedgerOutputProofWitnessV1 {
  ledger_output_proof_v1.LedgerOutputProofDatum {
    action,
    chunk_proof: proof,
    next_chunk_proof: None,
  }
}

fn advanced(
  result: Option<ledger_output_proof_v1.LedgerOutputProofStepResultV1>,
) -> ledger_output_proof_v1.LedgerOutputProofControlV1 {
  expect Some(ledger_output_proof_v1.LedgerOutputProofAdvanced { control }) =
    result
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
  expect Some(cardano_tx_out) =
    script_context_v1.tx_out_data_v1(no_reference_output, False)
  expect Some(midgard_tx_out) =
    script_context_v1.tx_out_data_v1(no_reference_output, True)
  expect Some(cardano_tx_out_summary) =
    ledger_output_proof_v1.cardano_tx_out_summary_v1(terminal)
  expect Some(midgard_tx_out_summary) =
    ledger_output_proof_v1.midgard_tx_out_summary_v1(terminal)
  expect Some(cardano_spend_datum_summary) =
    ledger_output_proof_v1.cardano_spend_datum_summary_v1(terminal)
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
    terminal.output_scan.address
      == #"7811111111111111111111111111111111111111111111111111111111",
    terminal.output_scan.lovelace == 0,
    terminal.script_hash == None,
    ledger_output_proof_v1.value_summary_v1(terminal)
      == Some(
          cek_data_v1.semantic_data_summary_v1(builtin.map_data([])),
        ),
    ledger_output_proof_v1.cardano_tx_out_summary_v1(terminal)
      == Some(cek_data_v1.semantic_data_summary_v1(cardano_tx_out)),
    ledger_output_proof_v1.midgard_tx_out_summary_v1(terminal)
      == Some(cek_data_v1.semantic_data_summary_v1(midgard_tx_out)),
    cardano_tx_out != midgard_tx_out,
    ledger_output_proof_v1.descriptor_is_exact_v1(terminal, descriptor),
    !ledger_output_proof_v1.descriptor_is_exact_v1(
      terminal,
      ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
        ..descriptor,
        lovelace: descriptor.lovelace + 1,
      },
    ),
    !ledger_output_proof_v1.descriptor_is_exact_v1(
      terminal,
      ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
        ..descriptor,
        cardano_tx_out: descriptor.midgard_tx_out,
      },
    ),
  }
}

test derives_the_first_script_hash_block_from_the_output_chunk() {
  let Pair(commitment, proof) = one_chunk_material(
    0,
    small_reference_output,
  )
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
  let reference_committed = advanced(
    ledger_output_proof_v1.step_v1(reference_ready, chunks(proof)),
  )
  let hash_ready = advanced(
    ledger_output_proof_v1.step_v1(
      reference_committed,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let hash_round = advanced(
    ledger_output_proof_v1.step_v1(hash_ready, chunks(proof)),
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
  let datum_opened = advanced(
    ledger_output_proof_v1.step_v1(
      datum_ready,
      datum(
        cek_data_traverse_v1.HeadSequence { expected_children: 0 },
        Some(proof),
      ),
    ),
  )
  let frame = cek_data_frame_v1.initial_small_constr_frame_v1(
    0,
    #"",
    0,
  )
  let datum_authenticated = advanced(
    ledger_output_proof_v1.step_v1(
      datum_opened,
      datum(
        cek_data_traverse_v1.FinalizeFrame {
          frame,
          parent: None,
        },
        None,
      ),
    ),
  )
  let terminal = advanced(
    ledger_output_proof_v1.step_v1(
      datum_authenticated,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let expected_spend_datum =
    builtin.constr_data(0, [builtin.constr_data(0, [])])
      |> cek_data_v1.semantic_data_summary_v1
  and {
    datum_ready.stage == ledger_output_proof_v1.stage_datum_traversal,
    ledger_output_proof_v1.cardano_spend_datum_summary_v1(terminal)
      == Some(expected_spend_datum),
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
  ledger_output_proof_v1.step_v1(initial, chunks(proof))
    == Some(ledger_output_proof_v1.LedgerOutputProofInvalidOutput)
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
  let Pair(commitment, proof) = one_chunk_material(
    0,
    small_reference_output,
  )
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
  ledger_output_proof_v1.step_v1(
    reference_ready,
    chunks(substituted),
  ) == None
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
  ledger_output_proof_v1.step_v1(
    datum_ready,
    datum(
      cek_data_traverse_v1.HeadSequence { expected_children: 0 },
      Some(substituted),
    ),
  ) == None
}

test decodes_the_long_typescript_terminal_control_canonically() {
  let control = ledger_output_proof_v1.decode_control_v1(
    cross_language_long_terminal_control,
  )
  expect Some(spend_datum_summary) =
    ledger_output_proof_v1.cardano_spend_datum_summary_v1(control)
  expect Some(reference_item_commitment) =
    ledger_output_proof_v1.reference_script_item_commitment_v1(control)
  expect Some(cardano_tx_out) =
    ledger_output_proof_v1.cardano_tx_out_summary_v1(control)
  expect Some(midgard_tx_out) =
    ledger_output_proof_v1.midgard_tx_out_summary_v1(control)
  and {
    ledger_output_proof_v1.terminal_is_exact_v1(control),
    control.reference_script_count == 2,
    bytearray.length(spend_datum_summary.root) == 32,
    bytearray.length(reference_item_commitment) == 32,
    bytearray.length(cardano_tx_out.root) == 32,
    bytearray.length(midgard_tx_out.root) == 32,
    cardano_tx_out.root != midgard_tx_out.root,
    ledger_output_proof_v1.reference_script_digest_v1(control)
      == Some(
          #"634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098",
        ),
    ledger_output_proof_v1.encode_control_v1(control)
      == cross_language_long_terminal_control,
  }
}
