use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/transition_trace/proof.{
  TransitionFaultProof, validate_transition_fault_proof,
}
use midgard/hub_oracle as hub

pub type Datum =
  ct.StepDatum<Data>

pub type Args {
  input_index: Int,
  output_index: Int,
  hub_ref_input_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  proof: TransitionFaultProof,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

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
        proof,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } =
          tx
        let hub_datum =
          hub.get_datum(reference_inputs, hub_oracle, hub_ref_input_index)
        expect Some(step_datum) = datum

        let
          _own_script_hash,
          computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
        <-
          finalize(
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

        expect None = m_input_state_data
        validate_transition_fault_proof(
          proof,
          computation_thread_token_asset_name,
          hub_datum,
          reference_inputs,
        )
      }
    }
  }

  else(_) {
    fail
  }
}
