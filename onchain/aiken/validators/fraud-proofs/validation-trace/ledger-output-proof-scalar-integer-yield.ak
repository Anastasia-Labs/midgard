//// The integer scalar-control decode attestation yield of the shared LOP
//// step family (`V1VtLopScalarIntegerYield`). It attests that the pinned pre
//// control's active integer scalar sub-control — with every well-formedness,
//// range and prefix-alignment clause of the monolithic decode path — equals
//// the scalar claim of the step dispatcher action, which the integer-family
//// datum stage yield of the same transaction consumes without re-decoding.

use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/ledger_output_proof_datum as datum
use midgard/ledger_output_proof_yield as shared

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let claim = shared.dispatch_attestation(
      dispatcher_script_hashes,
      tx.inputs,
      tx.redeemers,
    )
    datum.integer_scalar_claim_is_exact(claim.control, claim.claimed_scalar)
  }

  else(_) {
    fail
  }
}
