//// The four descriptor attestation predicates of the shared finalize family
//// conjoin to exactly `ledger_output_proof_v1.descriptor_is_exact_v1`: over
//// honest terminal controls the conjunction and the monolith agree, and each
//// forged fact — a mutated scalar, a substituted leaf summary, a forged
//// reference digest, a datum-mode lie — is refused by the yield that owns
//// it. A non-terminal control is refused by every role's shared
//// `terminal_scan` clause.

use aiken/builtin
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_frame_v1 as frame
use midgard/cek_data_traverse_v1 as traverse
use midgard/cek_data_v1 as summary
use midgard/ledger_output_commitment_v1 as commitment
use midgard/ledger_output_proof_descriptor as descriptor_yield
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_v1 as proof
use midgard/validation_merkle_v1 as merkle

const no_reference_output =
  #"a200581d7811111111111111111111111111111111111111111111111111111111018200a0"

const small_reference_output =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a0038203436b6b6b"

const small_datum_output =
  #"a300581d7811111111111111111111111111111111111111111111111111111111018200a00243d87980"

fn one_chunk_material(
  output_index: Int,
  output: ByteArray,
) -> Pair<ByteArray, bounded_item_v1.ChunkProofV1> {
  let frontier = merkle.append_leaf(
    0,
    merkle.empty_frontier(),
    bounded_item_v1.hash_chunk(2, output_index, 0, output),
  )
  let item_commitment = bounded_item_v1.commitment(
    2,
    output_index,
    bytearray.length(output),
    frontier,
  )
  Pair(
    item_commitment,
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

fn advanced(
  result: Option<proof.LedgerOutputProofStepResultV1>,
) -> proof.LedgerOutputProofControlV1 {
  expect Some(proof.LedgerOutputProofAdvanced { control }) = result
  control
}

/// Drive the typed proof to its terminal stage. The only datum these outputs
/// carry is the trivial `d87980`, opened and finalized with its two scripted
/// actions; every other stage takes `NoWitness` where accepted, otherwise the
/// single whole-output chunk proof.
fn until_terminal(
  control: proof.LedgerOutputProofControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  fuel: Int,
) -> proof.LedgerOutputProofControlV1 {
  expect fuel > 0
  if control.stage == proof.stage_terminal {
    control
  } else {
    let witness = if control.stage == proof.stage_datum_traversal {
      expect Some(datum_control) = control.datum
      if datum_control.stage == traverse.stage_head {
        proof.LedgerOutputProofDatum {
          action: traverse.HeadSequence { expected_children: 0 },
          chunk_proof: Some(chunk_proof),
          next_chunk_proof: None,
        }
      } else if datum_control.stage == traverse.stage_fold {
        proof.LedgerOutputProofDatum {
          action: traverse.FinalizeFrame {
            frame: frame.initial_small_constr_frame_v1(0, #"", 0),
            parent: None,
          },
          chunk_proof: None,
          next_chunk_proof: None,
        }
      } else {
        expect datum_control.stage == traverse.stage_terminal
        proof.LedgerOutputProofNoWitness
      }
    } else {
      when proof.step_v1(control, proof.LedgerOutputProofNoWitness) is {
        Some(_) -> proof.LedgerOutputProofNoWitness
        None ->
          proof.LedgerOutputProofChunks { chunk_proof, next_chunk_proof: None }
      }
    }
    until_terminal(
      advanced(proof.step_v1(control, witness)),
      chunk_proof,
      fuel - 1,
    )
  }
}

fn terminal_of(output: ByteArray) -> proof.LedgerOutputProofControlV1 {
  let Pair(item_commitment, chunk_proof) = one_chunk_material(0, output)
  until_terminal(
    proof.initial_control_v1(0, bytearray.length(output), item_commitment),
    chunk_proof,
    256,
  )
}

/// The honest descriptor of a terminal control, every field derived from the
/// control itself.
fn honest_descriptor(
  terminal: proof.LedgerOutputProofControlV1,
) -> commitment.LedgerOutputCommitmentV1 {
  expect Some(cardano_tx_out) = proof.cardano_tx_out_summary_v1(terminal)
  expect Some(midgard_tx_out) = proof.midgard_tx_out_summary_v1(terminal)
  expect Some(cardano_spend_datum) = proof.cardano_spend_datum_summary_v1(
    terminal,
  )
  let language = terminal.output_scan.reference_script_language
  commitment.LedgerOutputCommitmentV1 {
    version: commitment.version,
    output_index: terminal.output_index,
    total_length: terminal.total_length,
    item_commitment: terminal.item_commitment,
    address: terminal.output_scan.address,
    lovelace: terminal.output_scan.lovelace,
    asset_count: terminal.output_scan.asset_count,
    asset_frontier_commitment: merkle.frontier_commitment(
      terminal.output_scan.asset_count,
      terminal.output_scan.asset_peaks,
    ),
    cardano_value_size: terminal.output_scan.cardano_value_size,
    reference_script_language: language,
    reference_script_hash: if language == -1 {
      #""
    } else {
      expect Some(digest) = proof.reference_script_digest_v1(terminal)
      digest
    },
    reference_script_total_length: if language == -1 {
      0
    } else {
      terminal.total_length - terminal.output_scan.reference_script_item_offset
    },
    reference_script_item_commitment: if language == -1 {
      #""
    } else {
      expect Some(pinned) = proof.reference_script_item_commitment_v1(terminal)
      pinned
    },
    cardano_tx_out,
    midgard_tx_out,
    cardano_spend_datum,
  }
}

fn honest_value(
  terminal: proof.LedgerOutputProofControlV1,
) -> summary.DataSummaryV1 {
  expect Some(value_summary) = proof.value_summary_v1(terminal)
  value_summary
}

fn honest_datum(
  terminal: proof.LedgerOutputProofControlV1,
) -> Option<summary.DataSummaryV1> {
  if terminal.output_scan.datum_offset == -1 {
    None
  } else {
    expect Some(datum_control) = terminal.datum
    expect Some(datum_summary) = traverse.finalize_v1(datum_control)
    Some(datum_summary)
  }
}

fn conjunction(
  control_frame: raw.Frame,
  descriptor_cbor: ByteArray,
  claimed_value: summary.DataSummaryV1,
  claimed_datum: Option<summary.DataSummaryV1>,
) -> Bool {
  and {
    descriptor_yield.scan_facts_are_pinned(
      control_frame,
      descriptor_cbor,
      claimed_value,
      claimed_datum,
    ),
    descriptor_yield.reference_script_is_pinned(control_frame, descriptor_cbor),
    descriptor_yield.datum_summary_is_pinned(control_frame, claimed_datum),
    descriptor_yield.value_summary_is_pinned(control_frame, claimed_value),
  }
}

fn conjunction_matches_monolith(output: ByteArray) -> Bool {
  let terminal = terminal_of(output)
  let descriptor = honest_descriptor(terminal)
  let control_frame = raw.open(proof.encode_control_v1(terminal))
  let value_claim = honest_value(terminal)
  let datum_claim = honest_datum(terminal)
  let forged_scalar = commitment.LedgerOutputCommitmentV1 {
    ..descriptor,
    lovelace: descriptor.lovelace + 1,
  }
  let swapped_summaries = commitment.LedgerOutputCommitmentV1 {
    ..descriptor,
    cardano_tx_out: descriptor.midgard_tx_out,
  }
  and {
    proof.descriptor_is_exact_v1(terminal, descriptor),
    conjunction(
      control_frame,
      commitment.encode(descriptor),
      value_claim,
      datum_claim,
    ),
    // The monolith and the conjunction refuse the same forgeries.
    !proof.descriptor_is_exact_v1(terminal, forged_scalar),
    !descriptor_yield.scan_facts_are_pinned(
      control_frame,
      commitment.encode(forged_scalar),
      value_claim,
      datum_claim,
    ),
    !proof.descriptor_is_exact_v1(terminal, swapped_summaries),
    !descriptor_yield.scan_facts_are_pinned(
      control_frame,
      commitment.encode(swapped_summaries),
      value_claim,
      datum_claim,
    ),
  }
}

test descriptor_conjunction_matches_the_monolith_without_reference_script() {
  conjunction_matches_monolith(no_reference_output)
}

test descriptor_conjunction_matches_the_monolith_with_inline_datum() {
  conjunction_matches_monolith(small_datum_output)
}

test descriptor_conjunction_matches_the_monolith_with_reference_script() {
  conjunction_matches_monolith(small_reference_output)
}

/// A forged reference digest is refused by the reference-script yield.
test forged_reference_digest_is_refused() {
  let terminal = terminal_of(small_reference_output)
  let descriptor = honest_descriptor(terminal)
  let control_frame = raw.open(proof.encode_control_v1(terminal))
  let forged = commitment.LedgerOutputCommitmentV1 {
    ..descriptor,
    reference_script_hash: #"00000000000000000000000000000000000000000000000000000000",
  }
  and {
    descriptor_yield.reference_script_is_pinned(
      control_frame,
      commitment.encode(descriptor),
    ),
    !descriptor_yield.reference_script_is_pinned(
      control_frame,
      commitment.encode(forged),
    ),
    !proof.descriptor_is_exact_v1(terminal, forged),
  }
}

/// A substituted leaf summary is refused both by the leaf's own yield and by
/// the composite rebuild of the scan-facts yield; a datum-mode lie (claiming
/// a datum-bearing output datum-free) is refused by both datum-consuming
/// yields.
test substituted_leaf_summaries_are_refused() {
  let terminal = terminal_of(small_datum_output)
  let descriptor = honest_descriptor(terminal)
  let descriptor_cbor = commitment.encode(descriptor)
  let control_frame = raw.open(proof.encode_control_v1(terminal))
  let value_claim = honest_value(terminal)
  expect Some(datum_summary) = honest_datum(terminal)
  and {
    !descriptor_yield.value_summary_is_pinned(control_frame, datum_summary),
    !descriptor_yield.datum_summary_is_pinned(control_frame, Some(value_claim)),
    !descriptor_yield.datum_summary_is_pinned(control_frame, None),
    !descriptor_yield.scan_facts_are_pinned(
      control_frame,
      descriptor_cbor,
      datum_summary,
      Some(datum_summary),
    ),
    !descriptor_yield.scan_facts_are_pinned(
      control_frame,
      descriptor_cbor,
      value_claim,
      None,
    ),
  }
}

/// Every descriptor yield shares the `terminal_scan` clause, so an active
/// (non-terminal) control is refused before any fact is read.
test descriptor_yields_refuse_a_non_terminal_control() fail {
  let Pair(item_commitment, _chunk_proof) = one_chunk_material(
    0,
    no_reference_output,
  )
  let initial = proof.initial_control_v1(
    0,
    bytearray.length(no_reference_output),
    item_commitment,
  )
  descriptor_yield.terminal_scan(raw.open(proof.encode_control_v1(initial))) == descriptor_yield.terminal_scan(
    raw.open(proof.encode_control_v1(initial)),
  )
}
