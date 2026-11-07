use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/spend_input_signer_missing/rule.{terminal_v1}
use midgard/fraud_proofs/spend_input_signer_missing/step_05.{
  Args, Datum, SpendRedeemer,
}

validator main(
  fraud_proof_policy: PolicyId,
  fraud_proof_address: Address,
  thread_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, state <- finalize(
          thread_policy,
          fraud_proof_policy,
          fraud_proof_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_ref,
          inputs,
          outputs,
          redeemers,
        )
        expect Some(verdict) = state
        terminal_v1(verdict)
      }
    }
  }

  else(_) {
    fail
  }
}
