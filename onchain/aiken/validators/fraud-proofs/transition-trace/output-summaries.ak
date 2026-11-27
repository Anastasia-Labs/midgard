use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/transaction.{Transaction}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/output_summaries.{WalkWitness}

validator summaries(dispatcher: ScriptHash) {
  withdraw(witness: WalkWitness, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.phase == 8
    delegation.advance(
      datum,
      args,
      tx.outputs,
      output_summaries.step(
        state,
        delegation.output_bytes(state, args, tx.reference_inputs),
        witness,
      ),
    )
  }

  else(_) {
    fail
  }
}
