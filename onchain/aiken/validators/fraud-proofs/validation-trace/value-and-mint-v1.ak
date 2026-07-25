use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/validation_machine_v1.{verify_value_and_mint_one_step_v1}
use midgard/validation_resolver_v1.{Datum, SpendRedeemer, validate}
use midgard/validation_trace_v1.{ValueAndMint}

validator main(
  computation_thread_policy_id: PolicyId,
  fraud_proof_policy_id: PolicyId,
  fraud_proof_address: Address,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    validate(
      ValueAndMint,
      verify_value_and_mint_one_step_v1,
      computation_thread_policy_id,
      fraud_proof_policy_id,
      fraud_proof_address,
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
