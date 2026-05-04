use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use env.{empty_merkle_tree_root}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/missing_native_script/step_03_recursive.{
  Datum, Finalize, Recurse, SpendRedeemer, State,
}
use midgard/ledger_state

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
        script_hash_to_delete,
        script_to_delete,
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
          State { expected_missing_script_hash, verified_script_wits_root } as input_state,
        ) = m_input_state_data

        // 2. With `Recurse` redeemer, the script witness set must not be empty.
        expect verified_script_wits_root != empty_merkle_tree_root

        // 3. Provided script must correctly correspond to its hash.
        //    TODO: Another fraud proof seems to be needed for invalid mapping.
        expect
          ledger_state.hash_midgard_script(script_to_delete) == script_hash_to_delete

        // 4. The script about to be deleted must not have a hash that is
        //    expected to be missing from the transaction's script witness set.
        expect script_hash_to_delete != expected_missing_script_hash

        let updated_script_wits_root =
          utils.plutarch_pdelete(
            merkle_root_before_delete: verified_script_wits_root,
            key: script_hash_to_delete,
            value: script_to_delete,
            membership_proof: membership_proof,
            redeemers: redeemers,
            withdraw_redeemer_index: merkle_delete_script_withdraw_redeemer_index,
          )

        // 5. The UTxO must be produced at own script hash with the updated
        //    state.
        expect output_script_hash == own_script_hash
        let expected_output_state: Data =
          State {
            ..input_state,
            verified_script_wits_root: updated_script_wits_root,
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

        expect Some(State { verified_script_wits_root, .. }) =
          m_input_state_data

        // 2. With `Finalize` redeemer, the script witness set must be empty.
        expect verified_script_wits_root == empty_merkle_tree_root

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}
