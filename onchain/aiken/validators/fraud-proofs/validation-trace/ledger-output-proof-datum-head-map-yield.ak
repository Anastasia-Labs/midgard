use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/ledger_output_proof_datum as datum
use midgard/ledger_output_proof_yield as shared

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let claim = shared.dispatch(
      dispatcher_script_hashes,
      15,
      tx.inputs,
      tx.redeemers,
    )
    shared.attest(
      claim,
      datum.head_map(claim.control, claim.witness, claim.claimed_span),
    )
  }

  else(_) {
    fail
  }
}
