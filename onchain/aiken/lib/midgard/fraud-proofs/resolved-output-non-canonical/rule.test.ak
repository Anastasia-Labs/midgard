use aiken/builtin
use aiken/primitive/bytearray
use cardano/transaction.{OutputReference}
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/resolved_output_non_canonical/rule
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_scan_v1
use midgard/rejection_reason_v1.{InputNotFound, InputSpentOutputNonCanonical}
use midgard/validation_merkle_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const prior_root =
  #"ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

const h32 = #"2222222222222222222222222222222222222222222222222222222222222222"

/// A canonical two-entry ledger output: `{0: address(29), 1: [5000000, {}]}`.
const canonical_output =
  #"a200581d601111111111111111111111111111111111111111111111111111111101821a004c4b40a0"

fn accepted() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_acceptance,
    source_kind: substrate.source_kind_accepted,
    transaction_id: tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn rejected(source_kind: Int, input_index: Int) -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(
      InputSpentOutputNonCanonical { source_kind, input_index },
    ),
  }
}

fn verdict(
  subject: substrate.VerdictSubjectV1,
  invalid: Bool,
) -> rule.CanonicalVerdictV1 {
  rule.CanonicalVerdictV1 { subject, output_is_non_canonical: invalid }
}

/// The compact ledger descriptor a prior block committed for `output`. Only
/// the item commitment, output index and total length reach the scan; the
/// remaining facts are well-formed placeholders.
fn descriptor(
  output_index: Int,
  output: ByteArray,
) -> ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
  ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index,
    total_length: bytearray.length(output),
    item_commitment: ledger_output_commitment_v1.output_item_commitment(
      output_index,
      output,
    ),
    address: #"6011111111111111111111111111111111111111111111111111111111",
    lovelace: 5000000,
    asset_count: 0,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
    cardano_value_size: 5,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: cek_data_v1.DataSummaryV1 {
      root: h32,
      cbor_length: 101,
      memory: 202,
    },
    midgard_tx_out: cek_data_v1.DataSummaryV1 {
      root: h32,
      cbor_length: 103,
      memory: 204,
    },
    cardano_spend_datum: cek_data_v1.DataSummaryV1 {
      root: h32,
      cbor_length: 3,
      memory: 4,
    },
  }
}

/// The single chunk proof of an output shorter than one chunk.
fn chunk_proof(
  output_index: Int,
  output: ByteArray,
) -> bounded_item_v1.ChunkProofV1 {
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index: ledger_output_commitment_v1.output_field_index,
    item_index: output_index,
    total_length: bytearray.length(output),
    chunk_index: 0,
    chunk: output,
    frontier: validation_merkle_v1.append_leaf(
      0,
      [],
      bounded_item_v1.hash_chunk(
        ledger_output_commitment_v1.output_field_index,
        output_index,
        0,
        output,
      ),
    ),
    siblings: [],
  }
}

fn advance(
  output: ByteArray,
  control: ledger_output_scan_v1.LedgerOutputScanControlV1,
) -> Option<ledger_output_scan_v1.LedgerOutputScanControlV1> {
  rule.advance_reconstruction_v1(
    descriptor(0, output),
    control,
    chunk_proof(0, output),
    None,
  )
}

/// Walks the required-fields and value tokens of a two-entry output; the
/// returned control sits at the optional-field stage.
fn walk_value(
  output: ByteArray,
) -> ledger_output_scan_v1.LedgerOutputScanControlV1 {
  expect Some(after_address) = advance(
    output,
    ledger_output_scan_v1.initial_control_v1(),
  )
  expect after_address.stage == ledger_output_scan_v1.stage_value_header
  expect Some(after_value) = advance(output, after_address)
  expect after_value.stage == ledger_output_scan_v1.stage_optional_field
  after_value
}

test resolved_output_non_canonical_convicts_wrongful_acceptance() {
  rule.terminal_contradiction_v1(verdict(accepted(), True))
}

test resolved_output_non_canonical_convicts_wrongful_rejection() {
  rule.terminal_contradiction_v1(verdict(rejected(1, 4), False))
}

test resolved_output_non_canonical_refuses_honest_acceptance() fail {
  rule.terminal_contradiction_v1(verdict(accepted(), False))
}

test resolved_output_non_canonical_refuses_honest_rejection() fail {
  rule.terminal_contradiction_v1(verdict(rejected(0, 2), True))
}

test resolved_output_non_canonical_binds_exact_reason_coordinate() {
  let bound = rule.bind_input_v1(rejected(1, 4), 1, 4, prior_root)
  and {
    bound.source_kind == 1,
    bound.input_index == 4,
    bound.prior_root == prior_root,
  }
}

test resolved_output_non_canonical_binds_spend_source_reason() {
  rule.bind_input_v1(rejected(0, 0), 0, 0, prior_root).source_kind == 0
}

test resolved_output_non_canonical_refuses_source_substitution() fail {
  rule.bind_input_v1(rejected(1, 4), 0, 4, prior_root).source_kind == 0
}

test resolved_output_non_canonical_refuses_index_substitution() fail {
  rule.bind_input_v1(rejected(1, 4), 1, 3, prior_root).input_index == 3
}

test resolved_output_non_canonical_refuses_out_of_range_source() fail {
  rule.bind_input_v1(accepted(), 2, 0, prior_root).source_kind == 2
}

test resolved_output_non_canonical_refuses_negative_index() fail {
  rule.bind_input_v1(accepted(), 0, -1, prior_root).input_index == -1
}

test resolved_output_non_canonical_refuses_other_reason() fail {
  let subject = substrate.VerdictSubjectV1 {
    ..rejected(1, 4),
    rejection_reason: Some(InputNotFound { source_kind: 1, input_index: 4 }),
  }
  rule.bind_input_v1(subject, 1, 4, prior_root).input_index == 4
}

test resolved_output_non_canonical_refuses_prior_root_substitution() fail {
  rule.bind_input_v1(accepted(), 0, 0, #"00").input_index == 0
}

test resolved_output_non_canonical_authenticates_exact_out_ref() {
  let bound = rule.bind_input_v1(accepted(), 0, 0, prior_root)
  let out_ref = OutputReference { transaction_id: tx_id, output_index: 7 }
  rule.authenticate_out_ref_v1(bound, out_ref).out_ref == out_ref
}

// Resumable canonical reconstruction: the forced direction closes only when
// the scan walks the descriptor's exact committed item to its end.

test resolved_output_non_canonical_reconstructs_canonical_output_to_exact_end() {
  let at_end = walk_value(canonical_output)
  and {
    at_end.cursor == bytearray.length(canonical_output),
    rule.finalize_canonical_v1(descriptor(0, canonical_output), at_end),
  }
}

test resolved_output_non_canonical_refuses_premature_finalize() {
  expect Some(after_address) = advance(
    canonical_output,
    ledger_output_scan_v1.initial_control_v1(),
  )
  and {
    !rule.finalize_canonical_v1(
      descriptor(0, canonical_output),
      ledger_output_scan_v1.initial_control_v1(),
    ),
    !rule.finalize_canonical_v1(descriptor(0, canonical_output), after_address),
  }
}

/// The accepted-path guard: a finishable control cannot be advanced into a
/// non-canonical verdict by handing the engine an exhausted window.
test resolved_output_non_canonical_refuses_advance_at_finishable_control() fail {
  advance(canonical_output, walk_value(canonical_output)) == None
}

test resolved_output_non_canonical_trailing_byte_is_non_canonical() {
  let trailing = bytearray.concat(canonical_output, #"00")
  let at_end = walk_value(trailing)
  expect Some(terminal) = advance(trailing, at_end)
  and {
    at_end.cursor == bytearray.length(canonical_output),
    !rule.finalize_canonical_v1(descriptor(0, trailing), at_end),
    terminal.stage == ledger_output_scan_v1.stage_terminal,
    !rule.finalize_canonical_v1(descriptor(0, trailing), terminal),
    advance(trailing, terminal) == None,
  }
}

test resolved_output_non_canonical_malformed_address_is_non_canonical() {
  // The address bytes are one byte short of an enterprise address.
  let malformed = #"a200581c111111111111111111111111111111111111111111111111111101821a004c4b40a0"
  advance(malformed, ledger_output_scan_v1.initial_control_v1()) == None
}

test resolved_output_non_canonical_refuses_substituted_chunk_bytes() fail {
  let substituted = bytearray.concat(#"a3", bytearray.drop(canonical_output, 1))
  rule.advance_reconstruction_v1(
    descriptor(0, canonical_output),
    ledger_output_scan_v1.initial_control_v1(),
    chunk_proof(0, substituted),
    None,
  ) == None
}

test resolved_output_non_canonical_refuses_substituted_descriptor() fail {
  rule.advance_reconstruction_v1(
    descriptor(1, canonical_output),
    ledger_output_scan_v1.initial_control_v1(),
    chunk_proof(0, canonical_output),
    None,
  ) == None
}

test resolved_output_non_canonical_refuses_chunk_outside_checkpoint() fail {
  let control = ledger_output_scan_v1.LedgerOutputScanControlV1 {
    ..ledger_output_scan_v1.initial_control_v1(),
    cursor: bounded_item_v1.chunk_bytes,
  }
  advance(canonical_output, control) == None
}

test resolved_output_non_canonical_refuses_unexpected_next_chunk() fail {
  rule.advance_reconstruction_v1(
    descriptor(0, canonical_output),
    ledger_output_scan_v1.initial_control_v1(),
    chunk_proof(0, canonical_output),
    Some(chunk_proof(0, canonical_output)),
  ) == None
}

/// A two-chunk item (the canonical output padded past one chunk) with valid
/// chunk proofs for both chunks. The padding makes it non-canonical, but the
/// first transition only needs the address token at the head of the window.
fn two_chunk_item() -> (
  ByteArray,
  bounded_item_v1.ChunkProofV1,
  bounded_item_v1.ChunkProofV1,
) {
  let long_output = bytearray.concat(
    canonical_output,
    builtin.replicate_byte(5000, 0),
  )
  let first_chunk = bytearray.take(long_output, bounded_item_v1.chunk_bytes)
  let second_chunk = bytearray.drop(long_output, bounded_item_v1.chunk_bytes)
  let field_index = ledger_output_commitment_v1.output_field_index
  let first_hash = bounded_item_v1.hash_chunk(field_index, 0, 0, first_chunk)
  let second_hash = bounded_item_v1.hash_chunk(field_index, 0, 1, second_chunk)
  let built = validation_merkle_v1.build_frontier([first_hash, second_hash])
  let proof = fn(chunk_index: Int, chunk: ByteArray, sibling: ByteArray) {
    bounded_item_v1.ChunkProofV1 {
      version: bounded_item_v1.version,
      field_index,
      item_index: 0,
      total_length: bytearray.length(long_output),
      chunk_index,
      chunk,
      frontier: built.peaks,
      siblings: [sibling],
    }
  }
  (
    long_output,
    proof(0, first_chunk, second_hash),
    proof(1, second_chunk, first_hash),
  )
}

test resolved_output_non_canonical_advances_across_chunk_boundary() {
  let (long_output, first, second) = two_chunk_item()
  expect Some(after_address) = rule.advance_reconstruction_v1(
    descriptor(0, long_output),
    ledger_output_scan_v1.initial_control_v1(),
    first,
    Some(second),
  )
  after_address.stage == ledger_output_scan_v1.stage_value_header
}

/// The same valid first-chunk proof without its successor: the guard that
/// trips is the successor requirement, not chunk authentication.
test resolved_output_non_canonical_multi_chunk_requires_next_chunk() fail {
  let (long_output, first, _second) = two_chunk_item()
  rule.advance_reconstruction_v1(
    descriptor(0, long_output),
    ledger_output_scan_v1.initial_control_v1(),
    first,
    None,
  ) == None
}

/// A successor chunk for another item does not authenticate.
test resolved_output_non_canonical_refuses_substituted_next_chunk() fail {
  let (long_output, first, second) = two_chunk_item()
  rule.advance_reconstruction_v1(
    descriptor(0, long_output),
    ledger_output_scan_v1.initial_control_v1(),
    first,
    Some(
      bounded_item_v1.ChunkProofV1 {
        ..second,
        chunk: bytearray.concat(#"01", bytearray.drop(second.chunk, 1)),
      },
    ),
  ) == None
}

test resolved_output_non_canonical_successor_is_decided_by_the_engine() {
  let at_end = walk_value(canonical_output)
  let trailing = bytearray.concat(canonical_output, #"00")
  and {
    // A nonterminal transition continues the self-loop.
    advance(canonical_output, ledger_output_scan_v1.initial_control_v1()) != None,
    // The exact end closes as canonical, never as a self-loop.
    rule.finalize_canonical_v1(descriptor(0, canonical_output), at_end),
    // A structural fault closes as non-canonical, never as a self-loop.
    advance(trailing, walk_value(trailing)) != None,
    advance(
      trailing,
      when advance(trailing, walk_value(trailing)) is {
        Some(control) -> control
        None -> fail
      },
    ) == None,
  }
}
