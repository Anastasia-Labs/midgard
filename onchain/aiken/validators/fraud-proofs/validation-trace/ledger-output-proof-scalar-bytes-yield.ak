//// The bytes scalar-control decode attestation yield of the shared LOP step
//// family (`V1VtLopScalarBytesYield`); see the integer variant. The two
//// codec families are separate roles so a stage yield never links the codec
//// of the family it does not serve.

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
    datum.bytes_scalar_claim_is_exact(claim.control, claim.claimed_scalar)
  }

  else(_) {
    fail
  }
}
