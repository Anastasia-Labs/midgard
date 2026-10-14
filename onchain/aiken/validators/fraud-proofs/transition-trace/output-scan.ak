use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/output_pipeline as pipeline
use midgard/ledger_output_scan_v1 as scan

validator scan_output(dispatcher: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.phase == 2 && state.kind != 2
    let bytes = delegation.output_bytes(state, args, tx.reference_inputs)
    let control = if state.scan_cbor == #"" {
      scan.initial_control_v1()
    } else {
      scan.decode_control_v1(state.scan_cbor)
    }
    let (next, start, end) = pipeline.scan_batch(
      control,
      bytes,
      state.value_start,
      state.value_end,
      4,
    )
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State {
        ..state,
        phase: if scan.terminal_is_exact_v1(next, bytearray.length(bytes)) {
          7
        } else {
          2
        },
        scan_cbor: scan.encode_control_v1(next),
        value_start: start,
        value_end: end,
      },
    )
  }

  else(_) {
    fail
  }
}
