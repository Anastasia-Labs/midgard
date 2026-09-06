use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_yield as shared

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let claim = shared.dispatch(
      dispatcher_script_hashes,
      8,
      tx.inputs,
      tx.redeemers,
    )
    shared.attest(
      claim,
      stages.reference_script(
        claim.control,
        shared.chunk_witness(claim.witness),
        claim.claimed_span,
      ),
    )
  }

  else(_) {
    fail
  }
}
