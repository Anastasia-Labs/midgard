use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/constants.{empty_merkle_tree_root}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/non_existent_input_no_index/step_04.{
  Datum, Finalize, Recurse, SpendRedeemer, State,
}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: ScriptHash,
  fraud_proof_token_address: Address,
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
      ct.Continue(Recurse {
        input_index,
        output_index,
        output_to_delete,
        membership_proof,
        merkle_delete_script_withdraw_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
          own_script_hash,
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

        expect Some(
          State {
            verified_outputs_root,
            non_existent_input_output_index,
            output_index_to_delete,
          } as input_state,
        ) = m_input_state_data

        // 2. With `Recurse` redeemer, the outputs must not be empty
        expect verified_outputs_root != empty_merkle_tree_root

        // 3. With `Recurse` redeemer, target input's output index cannot be
        //    equal or smaller than the output currently being removed from the
        //    tree.
        expect non_existent_input_output_index > output_index_to_delete

        let updated_outputs_root =
          utils.plutarch_pdelete(
            merkle_root_before_delete: verified_outputs_root,
            key: builtin.i_data(output_index_to_delete),
            value: output_to_delete,
            membership_proof: membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: merkle_delete_script_withdraw_redeemer_index,
          )

        // 4. The UTxO must be produced at own script hash with the updated
        //    state.
        expect output_script_hash == own_script_hash
        let expected_output_state: Data =
          State {
            ..input_state,
            verified_outputs_root: updated_outputs_root,
            output_index_to_delete: output_index_to_delete + 1,
          }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
      ct.Continue(Finalize {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
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

        expect Some(State {
          verified_outputs_root,
          non_existent_input_output_index,
          output_index_to_delete,
        }) = m_input_state_data

        // 2. With `Finalize` redeemer, the outputs must be empty.
        expect verified_outputs_root == empty_merkle_tree_root

        // 3. With outputs exhausted, `non_existent_input_output_index` must be
        //    greater than or equal `output_index_to_delete` (since this value
        //    is now equal to the "length" of the outputs list).
        expect non_existent_input_output_index >= output_index_to_delete

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
