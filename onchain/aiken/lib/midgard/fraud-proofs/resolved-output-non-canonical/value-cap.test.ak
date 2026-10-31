//// `resolvedOutputNonCanonical` at the Cardano value cap, both directions:
//// the resolved output's bytes carry a 5,000-byte value (canonical) or a
//// 5,001-byte one (not), spelled with long asset names across the 4,095-byte
//// chunk boundary and read through two authenticated chunks.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{
  ContinueDatum, FinishDatum,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_scan_v1
use midgard/ledger_output_value_size_fixture_v1 as fixture
use midgard/rejection_reason_v1.{InputSpentOutputNonCanonical}
use midgard/validation_merkle_v1

const tx_id =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"

const h32 = #"2222222222222222222222222222222222222222222222222222222222222222"

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

fn rejected() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: substrate.direction_wrongful_rejection,
    source_kind: substrate.source_kind_forced,
    transaction_id: tx_id,
    source_key: #"01",
    rejection_reason: Some(
      InputSpentOutputNonCanonical { source_kind: 0, input_index: 0 },
    ),
  }
}

/// The descriptor a prior block committed for an output with item
/// commitment `commitment` and `total_length` bytes. Only those two and the
/// output index reach the scan; the remaining facts are well-formed
/// placeholders.
fn descriptor(
  commitment: ByteArray,
  total_length: Int,
) -> ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
  let summary = cek_data_v1.DataSummaryV1 {
    root: h32,
    cbor_length: 3,
    memory: 4,
  }
  ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: 0,
    total_length,
    item_commitment: commitment,
    address: fixture.address,
    lovelace: 2000000,
    asset_count: 0,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
    cardano_value_size: 5,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out: summary,
    midgard_tx_out: summary,
    cardano_spend_datum: summary,
  }
}

/// Reconstructs the output to the verdict the resolved-output thread
/// records: each step admits the chunk holding the cursor and the chunk
/// after it, and a closable control is finalized.
fn reconstruct(
  subject: substrate.VerdictSubjectV1,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
  proofs: List<bounded_item_v1.ChunkProofV1>,
  control: ledger_output_scan_v1.LedgerOutputScanControlV1,
  fuel: Int,
) -> rule.CanonicalVerdictV1 {
  expect fuel > 0
  when ledger_output_scan_v1.finish_v1(control, descriptor.total_length) is {
    Some(_) ->
      when
        rule.open_datum_v1(
          subject,
          descriptor,
          rule.finalize_canonical_v1(descriptor, control),
        )
      is {
        FinishDatum { verdict } -> verdict
        ContinueDatum { .. } -> fail
      }
    None -> {
      let chunk_index = control.cursor / bounded_item_v1.chunk_bytes
      expect Some(chunk_proof) = list.at(proofs, chunk_index)
      when
        rule.advance_reconstruction_v1(
          descriptor,
          control,
          chunk_proof,
          list.at(proofs, chunk_index + 1),
        )
      is {
        None ->
          rule.CanonicalVerdictV1 { subject, output_is_non_canonical: True }
        Some(next) -> reconstruct(subject, descriptor, proofs, next, fuel - 1)
      }
    }
  }
}

fn verdict(
  subject: substrate.VerdictSubjectV1,
  tail_quantity: Int,
) -> rule.CanonicalVerdictV1 {
  let output = fixture.long_name_output(tail_quantity)
  let Pair(commitment, proofs) = fixture.two_chunk_material(0, output)
  reconstruct(
    subject,
    descriptor(commitment, bytearray.length(output)),
    proofs,
    ledger_output_scan_v1.initial_control_v1(),
    256,
  )
}

test resolved_output_value_of_5000_bytes_is_canonical() {
  !verdict(accepted(), fixture.quantity_at_cap).output_is_non_canonical
}

test honest_acceptance_of_a_resolved_5000_byte_value_is_not_convicted() {
  !rule.terminal_contradiction_v1(verdict(accepted(), fixture.quantity_at_cap))
}

test wrongful_acceptance_of_a_resolved_5001_byte_value_is_convicted() {
  let terminal = verdict(accepted(), fixture.quantity_over_cap)
  and {
    terminal.output_is_non_canonical,
    rule.terminal_contradiction_v1(terminal),
  }
}

test wrongful_rejection_of_a_resolved_5000_byte_value_is_convicted() {
  let terminal = verdict(rejected(), fixture.quantity_at_cap)
  and {
    !terminal.output_is_non_canonical,
    rule.terminal_contradiction_v1(terminal),
  }
}

test honest_rejection_of_a_resolved_5001_byte_value_is_not_convicted() {
  let terminal = verdict(rejected(), fixture.quantity_over_cap)
  and {
    terminal.output_is_non_canonical,
    !rule.terminal_contradiction_v1(terminal),
  }
}
