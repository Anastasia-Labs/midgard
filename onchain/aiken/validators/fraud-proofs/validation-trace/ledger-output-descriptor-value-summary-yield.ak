use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/ledger_output_descriptor_yield as shared
use midgard/ledger_output_proof_descriptor as descriptor

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let claim = shared.dispatch(dispatcher_script_hashes, tx)
    descriptor.value_summary_is_pinned(
      claim.control,
      claim.claimed_value_summary,
    )
  }

  else(_) {
    fail
  }
}
