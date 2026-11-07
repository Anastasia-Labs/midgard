use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/validation_resolver_v1.{
  Datum, PrepareSelectedSpendRedeemer,
  phase_a_script_preconditions_semantic_resolver_count, prepare_selected,
}
use midgard/validation_trace_v1.{PhaseAScriptPreconditions}

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
      PhaseAScriptPreconditions,
      semantic_resolver_script_hashes,
      phase_a_script_preconditions_semantic_resolver_count,
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
