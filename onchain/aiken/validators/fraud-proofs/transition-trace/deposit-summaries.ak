use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput, MidgardValue}
use midgard/fraud_proofs/transition_trace/deposit_source
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/proof
use midgard/script_context_v1

validator summaries(dispatcher: ScriptHash) {
  withdraw(
    committed: delegation.OutputSummaries,
    _credential: Credential,
    tx: Transaction,
  ) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 1 && state.phase == 8
    let (reference, _, _) = deposit_source.open(
      state,
      args,
      tx.reference_inputs,
    )
    let info = reference.event.info
    let output = MidgardTxOutput {
      address: proof.deposit_address_to_midgard(
        info.l2_address,
        info.l2_network_id,
      ),
      value: MidgardValue { lovelace: 0, assets: [] },
      datum_cbor: None,
      script_ref: None,
    }
    let datum_field = when info.l2_datum is {
      None -> builtin.constr_data(0, [])
      Some(data) -> builtin.constr_data(2, [data])
    }
    expect Some(value) = state.value_summary
    let summary = script_context_v1.ledger_output_summaries_of_authenticated_parts_v1(
      output,
      value,
      datum_field,
    )
    expect committed.summaries == [summary]
    delegation.advance(
      datum,
      args,
      tx.outputs,
      delegation.State { ..state, phase: 3, summaries: committed },
    )
  }

  else(_) {
    fail
  }
}
