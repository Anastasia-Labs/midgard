use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_signature/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_signature/step_03

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
        bad_required_signer_hash_index,
        bad_required_signer_hash,
        bad_required_signer_membership_proof,
        inclusion_proof_script_withdraw_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
        <-
          continue(
            computation_thread_token_policy_id,
            step_datum,
            input_index,
            output_index,
            own_out_ref,
            inputs,
            outputs,
          )

        expect Some(State {
          verified_required_signers_root,
          verified_addr_tx_wits,
        }) = m_input_state_data

        // 2. Provided required signer must be a member of the verified root
        //    passed from the previous step.
        expect
          utils.plutarch_phas(
            merkle_root: verified_required_signers_root,
            key: bad_required_signer_hash_index,
            value: bad_required_signer_hash,
            membership_proof: bad_required_signer_membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: inclusion_proof_script_withdraw_redeemer_index,
          )

        // 3. The required signer (whose signature is being proven missing),
        //    along with the verified address witness set must be passed to the
        //    next step.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data =
          step_03.State {
            missing_required_signer_hash: bad_required_signer_hash,
            verified_addr_tx_wits,
          }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
