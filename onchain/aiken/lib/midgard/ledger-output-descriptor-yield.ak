//// Shared dispatch for the four ledger-output descriptor yields. Each yield
//// authenticates exactly one finalize dispatcher input (ScriptSources output
//// proof or ResolveInputs membership), reads the one redeemer layout both
//// dispatchers carry, and opens the terminal control the dispatcher pinned to
//// its own carrier bytes.

use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/cek_data_v1 as summary
use midgard/ledger_output_proof_raw as raw
use midgard/validation_semantic_yield_v1

pub type DescriptorClaim {
  control: raw.Frame,
  descriptor_cbor: ByteArray,
  claimed_value_summary: summary.DataSummaryV1,
  claimed_datum_summary: Option<summary.DataSummaryV1>,
}

pub fn dispatch(
  dispatcher_script_hashes: List<ScriptHash>,
  tx: Transaction,
) -> DescriptorClaim {
  let (_state, _transition, extra) = validation_semantic_yield_v1.unique_semantic_dispatch_v1(
    dispatcher_script_hashes,
    tx,
  )
  expect
      [
        descriptor_data, _signer_proof_data, control_data, value_summary_data,
        datum_summary_data, _yield_ref_input_indices,
      ]
    = extra
  expect claimed_value_summary: summary.DataSummaryV1 = value_summary_data
  expect claimed_datum_summary: Option<summary.DataSummaryV1> = datum_summary_data
  DescriptorClaim {
    control: raw.open(builtin.un_b_data(control_data)),
    descriptor_cbor: builtin.un_b_data(descriptor_data),
    claimed_value_summary,
    claimed_datum_summary,
  }
}
