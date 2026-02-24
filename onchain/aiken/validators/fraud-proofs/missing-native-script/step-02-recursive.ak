use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/constants
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/missing_native_script/step_02_recursive.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/ledger_state.{Plutus, Timelock}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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
        tx_inclusion_args,
        script_wit_to_delete_key,
        script_wit_to_delete_value,
        script_wits_delete_proof,
        plutarch_pdelete_withdraw_redeemer_index,
      }) -> {
        let Transaction { redeemers, .. } = tx

        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
          own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          m_input_state_data,
          output_script_hash,
          output_state_data,
          _header,
          _bad_tx_id,
          _bad_tx,
          _bad_tx_body,
        <-
          pass_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )

        expect Some(State { bad_tx_script_wits, bad_output_script_hash }) =
          m_input_state_data
        // 2. We are proving that the bad output native script doesn't exist in the tx.
        // If you are able to delete it from the merkletree, 
        // it means it existed and the fraud proof is wrong!
        expect
          bad_output_script_hash != when script_wit_to_delete_value is {
            Timelock { script } -> script
            Plutus { script } -> script
          }
        // 2. Recursion of the merkle proof deletion to demonstrate that 
        //(script_hash(address(u)), s) doesn't belong to script_tx_wits(tx)
        let new_bad_tx_script_wits =
          utils.plutarch_pdelete(
            bad_tx_script_wits,
            script_wit_to_delete_key,
            script_wit_to_delete_value,
            script_wits_delete_proof,
            redeemers,
            plutarch_pdelete_withdraw_redeemer_index,
          )

        if new_bad_tx_script_wits == constants.empty_merkle_tree_root {
          // 3a. Base case, merkle tree is empty
          expect output_script_hash == step_03_validator_script_hash
          let expected_output_state: Data = None
          expect output_state_data == expected_output_state
        } else {
          // 3b. Recursion with new merkle root
          expect output_script_hash == own_script_hash
          let expected_output_state: Data =
            State {
              bad_tx_script_wits: new_bad_tx_script_wits,
              bad_output_script_hash,
            }
          expect output_state_data == expected_output_state
        }

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
