//// The ledger output proof at the Cardano value cap: the structure stage of
//// an output whose value is 5,000 bytes reaches the value fold, and one
//// whose value is 5,001 bytes ends in `LedgerOutputProofInvalidOutput`. Each
//// step also runs through its physical stage function, to the same result.

use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/ledger_output_proof_raw as raw
use midgard/ledger_output_proof_stages as narrow
use midgard/ledger_output_proof_v1 as proof
use midgard/ledger_output_scan_v1 as scan
use midgard/ledger_output_value_size_fixture_v1 as fixture

/// The physical structure-stage function's result for the typed step's
/// witness agrees with the typed step's.
fn same_structure_step(
  control: proof.LedgerOutputProofControlV1,
  witness: proof.LedgerOutputProofWitnessV1,
  typed: Option<proof.LedgerOutputProofStepResultV1>,
) -> Bool {
  let frame = raw.open(proof.encode_control_v1(control))
  let physical = if witness == proof.LedgerOutputProofNoWitness {
    narrow.structure_finish(frame, witness)
  } else if control.output_scan.stage <= scan.stage_value_header {
    narrow.structure_headers(frame, witness)
  } else if control.output_scan.stage <= scan.stage_asset {
    narrow.structure_assets(frame, witness)
  } else {
    narrow.structure_optional(frame, witness)
  }
  when typed is {
    Some(proof.LedgerOutputProofAdvanced { control: next }) -> {
      expect Some(narrow.Advanced { control: raw_next }) = physical
      raw.encode(raw_next) == proof.encode_control_v1(next)
    }
    Some(proof.LedgerOutputProofInvalidOutput) ->
      physical == Some(narrow.InvalidOutput)
    _ -> False
  }
}

/// Steps the structure stage over the output's chunk proofs until it leaves
/// that stage: the first result that does not advance within it.
fn structure(
  control: proof.LedgerOutputProofControlV1,
  proofs: List<bounded_item_v1.ChunkProofV1>,
  fuel: Int,
) -> Option<proof.LedgerOutputProofStepResultV1> {
  expect fuel > 0
  let output_scan = control.output_scan
  let closable = or {
    scan.finish_v1(output_scan, control.total_length) != None,
    scan.terminal_is_exact_v1(output_scan, control.total_length),
  }
  let witness = if closable {
    proof.LedgerOutputProofNoWitness
  } else {
    let chunk_index = output_scan.cursor / bounded_item_v1.chunk_bytes
    expect Some(chunk_proof) = list.at(proofs, chunk_index)
    proof.LedgerOutputProofChunks {
      chunk_proof,
      next_chunk_proof: if output_scan.stage <= scan.stage_optional_field {
        list.at(proofs, chunk_index + 1)
      } else {
        None
      },
    }
  }
  let result = proof.step_v1(control, witness)
  expect same_structure_step(control, witness, result)
  when result is {
    Some(proof.LedgerOutputProofAdvanced { control: next }) ->
      if next.stage == proof.stage_structure {
        structure(next, proofs, fuel - 1)
      } else {
        result
      }
    _ -> result
  }
}

fn structure_of(
  tail_quantity: Int,
) -> Option<proof.LedgerOutputProofStepResultV1> {
  let output = fixture.long_name_output(tail_quantity)
  let Pair(commitment, proofs) = fixture.two_chunk_material(0, output)
  structure(
    proof.initial_control_v1(0, bytearray.length(output), commitment),
    proofs,
    256,
  )
}

test output_value_of_5000_bytes_reaches_the_value_fold() {
  expect Some(proof.LedgerOutputProofAdvanced { control }) = structure_of(
    fixture.quantity_at_cap,
  )
  and {
    control.stage == proof.stage_value_fold,
    control.output_scan.cardano_value_size == 5000,
  }
}

test output_value_of_5001_bytes_is_an_invalid_output() {
  structure_of(fixture.quantity_over_cap) == Some(
    proof.LedgerOutputProofInvalidOutput,
  )
}
