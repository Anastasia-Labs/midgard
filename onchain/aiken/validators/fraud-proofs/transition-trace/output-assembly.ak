use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/output_pipeline as pipeline
use midgard/ledger_output_scan_v1 as scan

validator assembly(dispatcher: ScriptHash) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.phase == 3
    let index = if state.kind == 1 {
      state.deposit_index
    } else {
      state.output_index
    }
    let bytes = delegation.output_bytes(state, args, tx.reference_inputs)
    expect [(s1, s2, s3)] = state.summaries.summaries
    let descriptor_cbor = pipeline.descriptor_cbor(
      index,
      bytes,
      scan.decode_control_v1(state.scan_cbor),
      state.value_start,
      s1,
      s2,
      s3,
    )
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State {
        ..state,
        phase: if state.kind == 1 {
          9
        } else {
          4
        },
        descriptor_cbor,
      },
    )
  }

  else(_) {
    fail
  }
}
