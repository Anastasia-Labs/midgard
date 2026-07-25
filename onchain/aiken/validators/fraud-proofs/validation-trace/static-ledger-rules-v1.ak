use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/validation_resolver_v1.{Datum, PrepareSpendRedeemer, prepare}
use midgard/validation_trace_v1.{StaticLedgerRules}

validator main(
  semantic_resolver_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: PrepareSpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    prepare(
      StaticLedgerRules,
      semantic_resolver_script_hash,
      computation_thread_policy_id,
      datum,
      redeemer,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}
