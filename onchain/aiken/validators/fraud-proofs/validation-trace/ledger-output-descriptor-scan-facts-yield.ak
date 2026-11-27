use aiken/crypto.{ScriptHash}
use cardano/transaction.{Transaction}
use midgard/ledger_output_descriptor_yield as shared
use midgard/ledger_output_proof_descriptor as descriptor

validator main(dispatcher_script_hashes: List<ScriptHash>) {
  withdraw(_redeemer: Data, _credential, tx: Transaction) {
    let claim = shared.dispatch(dispatcher_script_hashes, tx)
    descriptor.scan_facts_are_pinned(
      claim.control,
      claim.descriptor_cbor,
      claim.claimed_value_summary,
      claim.claimed_datum_summary,
    )
  }

  else(_) {
    fail
  }
}
