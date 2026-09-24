use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/transition_trace/final_l1_event.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/transition_trace/final_l1_event as delegation
use midgard/hub_oracle as hub
use midgard/state_queue
use midgard/state_queue_yield

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  reference_script_auth_policy_id: PolicyId,
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
        event_reference: _,
        yield_ref_input_index,
        completed_fraud_witness,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, asset_name, _, carried_proof <- finalize(
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
        let hub_datum = hub.get_datum(
          reference_inputs,
          hub_oracle,
          hub_ref_input_index,
        )
        expect witness: state_queue.CompletedFraudWitness = completed_fraud_witness
        expect
          state_queue.require_completed_fraud_witness(
            tx,
            hub_datum.state_queue,
            asset_name,
            witness,
          )
        expect Some(raw_proof) = carried_proof
        expect _yield = state_queue_yield.require_authenticated_zero_yield(
          tx.reference_inputs,
          tx.withdrawals,
          tx.redeemers,
          reference_script_auth_policy_id,
          delegation.role_for(raw_proof),
          yield_ref_input_index,
        )
        True
      }
    }
  }

  else(_) {
    fail
  }
}
