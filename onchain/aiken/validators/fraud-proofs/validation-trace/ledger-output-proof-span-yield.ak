//// The span-attach stage yield of the shared LOP step family
//// (`V1VtLopSpanYield`, step role 23). It performs the chunk merkle
//// verification of the claimed output span exactly once and attests the
//// successor control whose frame item 12 records the window's
//// `(start, length, digest)`; every subsequent span-consuming stage step
//// binds its redeemer-supplied window bytes to that digest with one hash
//// plus equality instead of re-running the merkle verification.

use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/ledger_output_proof_stages as stages
use midgard/ledger_output_proof_yield as shared

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let claim = shared.dispatch(
      dispatcher_script_hashes,
      23,
      tx.inputs,
      tx.redeemers,
    )
    shared.attest(
      claim,
      stages.span_attach(
        claim.control,
        shared.span_attach_witness(claim.witness),
      ),
    )
  }

  else(_) {
    fail
  }
}
