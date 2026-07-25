use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/transition_trace/final_v1.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/transition_trace/proof.{validate_l1_event_fault_proof}
use midgard/hub_oracle as hub

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  hub_oracle: PolicyId,
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
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        hub_ref_input_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, asset_name, _, proof <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )
        expect Some(transition_proof) = proof
        validate_l1_event_fault_proof(
          transition_proof,
          asset_name,
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index),
          reference_inputs,
        )
      }
    }
  }

  else(_) {
    fail
  }
}
