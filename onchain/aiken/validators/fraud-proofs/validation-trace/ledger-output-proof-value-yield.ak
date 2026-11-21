use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_yield as shared

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let claim = shared.dispatch(
      dispatcher_script_hashes,
      1,
      tx.inputs,
      tx.redeemers,
    )
    shared.attest(
      claim,
      stages.value_fold(claim.control, shared.value_witness(claim.witness)),
    )
  }

  else(_) {
    fail
  }
}
