use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/validation_resolver_v1.{
  Datum, PrepareSelectedSpendRedeemer, prepare_selected,
}
use midgard/validation_trace_v1.{CompactBinding}

validator main(
  semantic_resolver_script_hashes: List<ScriptHash>,
  computation_thread_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: PrepareSelectedSpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    prepare_selected(
      CompactBinding,
      semantic_resolver_script_hashes,
      1,
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
