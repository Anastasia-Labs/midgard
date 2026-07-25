use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/validation_award_v1.{Args, Datum, SpendRedeemer}
use midgard/validation_resolution_v1

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
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
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
        finalize(
          computation_thread_policy_id,
          fraud_proof_policy_id,
          fraud_proof_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
          fn(
            _input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
          ) {
            expect Some(state) = input_state
            validation_resolution_v1.winning_resolution_is_well_formed(state)
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
