use aiken/builtin
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/ledger_output_proof_v1
use midgard/validation_merkle_v1

const no_reference_output =
  #"a200581d7811111111111111111111111111111111111111111111111111111111018200a0"

const small_reference_output =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b"

const malformed_nonminimal_output =
  #"b80200581d7811111111111111111111111111111111111111111111111111111111018200a0"

const cross_language_long_terminal_control =
  #"8a010400192bf15820a023c9459077b4fc906660cacfa81a46eea15b9ad1f21fb20fbd745d2678f9ec970107192bf10402581d78111111111111111111111111111111111111111111111111111111111a007a1200182e000000581c555555555555555555555555555555555555555555555555555555554040028182015820fdd05992e96e478560b718d45058402827072f35e5220f396e2569800a2b76fe1854191427000319147c19148119177002818201582028f935b37d798dd5f68f23fa40e9d9dd02037d6b1e1fa7ad7edfdcb84b63a26cd8799f8901031917711917715840634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098dbdd88d3a54a6646cce0bede0423668a5079fb08595004db249d66dbc8e10681056a775c40004000ffd87a80"

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

fn advanced(
  result: Option<ledger_output_proof_v1.LedgerOutputProofStepResultV1>,
) -> ledger_output_proof_v1.LedgerOutputProofControlV1 {
  expect Some(ledger_output_proof_v1.LedgerOutputProofAdvanced { control }) =
    result
  control
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
  let terminal = advanced(
    ledger_output_proof_v1.step_v1(
      scan_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  and {
    ledger_output_proof_v1.terminal_is_exact_v1(terminal),
    terminal.output_scan.address
      == #"7811111111111111111111111111111111111111111111111111111111",
    terminal.output_scan.lovelace == 0,
    terminal.script_hash == None,
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
  let reference_ready = advanced(
    ledger_output_proof_v1.step_v1(
      scan_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
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
  let reference_ready = advanced(
    ledger_output_proof_v1.step_v1(
      scan_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let substituted = bounded_item_v1.ChunkProofV1 {
    ..proof,
    chunk: builtin.replicate_byte(bytearray.length(proof.chunk), 0),
  }
  ledger_output_proof_v1.step_v1(
    reference_ready,
    chunks(substituted),
  ) == None
}

test decodes_the_long_typescript_terminal_control_canonically() {
  let control = ledger_output_proof_v1.decode_control_v1(
    cross_language_long_terminal_control,
  )
  expect Some(reference_item_commitment) =
    ledger_output_proof_v1.reference_script_item_commitment_v1(control)
  and {
    ledger_output_proof_v1.terminal_is_exact_v1(control),
    control.reference_script_count == 2,
    bytearray.length(reference_item_commitment) == 32,
    ledger_output_proof_v1.reference_script_digest_v1(control)
      == Some(
          #"634e9ca63abb532a52c53389db12d1514358f8ff155e3d82c0622098",
        ),
    ledger_output_proof_v1.encode_control_v1(control)
      == cross_language_long_terminal_control,
  }
}
