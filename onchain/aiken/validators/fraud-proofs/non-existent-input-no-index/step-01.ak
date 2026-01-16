//// Fraud proof validator for the non-existing input violation.

use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, verify_bad_tx_in_state_queue_node_and_get_header,
}
use midgard/fraud_proofs/non_existent_input_no_index/step_01.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/non_existent_input_no_index/step_02

validator main(
  step_02_validator_script_hash: ScriptHash,
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
        bad_state_queue_node_ref_input_index,
        bad_tx_hash,
        bad_tx_info_hash,
        bad_tx_to_root_proof_path,
        bad_tx,
        bad_tx_body,
        bad_spend_inputs,
        bad_input,
        bad_input_tx_info_hash,
        bad_input_tx_to_root_proof_path,
        index_output,
        computation_thread_redeemer_index,
        previous_tx,
        previous_tx_body,
        real_outputs,
        hub_ref_input_index,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } =
          tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
          own_script_hash,
          computation_thread_token_asset_name,
          _fraud_prover,
          _m_input_state_data,
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

        // 2. The UTxO must be produced at next step's script hash with the
        //    verified bad transactions' inputs included in its datum.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State { verified_tx1_inputs_root: bad_tx_body.spend_inputs }
        expect output_state_data == expected_output_state

        // 3. The provided transaction must be valid, and must be included in
        //    the specified state queue node.
        expect _header =
          verify_bad_tx_in_state_queue_node_and_get_header(
            bad_tx_id,
            bad_tx,
            bad_tx_body,
            bad_tx_membership_proof,
            computation_thread_token_asset_name,
            hub_oracle,
            hub_ref_input_index,
            bad_state_queue_node_ref_input_index,
            inclusion_proof_script_withdraw_redeemer_index,
            reference_inputs,
            redeemers,
          )

        // Done.
        True

        let hub.Datum { state_queue, .. } =
          hub.get_datum(tx.reference_inputs, hub_oracle, hub_ref_input_index)
        let ct_token_asset_name =
          get_singleton_ct_token(own_input, ct_token_policy_id)
        let extracted_tx_root =
          get_tx_root_validate_block_hash(
            tx,
            bad_state_queue_node_ref_input_index,
            ct_token_asset_name,
            state_queue,
          )

        // -------- Non-existent input: validation-specific logic START --------
        // Membership proof for tx1
        //we have a transaction that is attemping to spend utxo HASH#INDEX
        //HASH is actually inside the current block however the INDEX is an invalid index
        expect
          validate_membership_proof(
            tx,
            plutarch_midgard_merkle_validator_inclusion,
            MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: bad_tx_hash,
              tx_info_hash: bad_tx_info_hash,
              tx_to_root_proof_path: bad_tx_to_root_proof_path,
            },
          )

        // Membership of the bad_input among the tx1 spent inputs
        //validation of the redeemer data
        verify_hash_32(bad_tx, bad_tx_hash)
        verify_hash_32(bad_tx_body, bad_tx.body)
        verify_hash_32(bad_spend_inputs, bad_tx_body.spend_inputs)

        //redeemer data is correct, but does it include the bad input?
        expect list.has(builtin.un_list_data(bad_spend_inputs), bad_input)

        // Inclusion proof that the transaction id of the bad input is in current block txs
        expect invalid_input: Input = bad_input
        expect
          validate_membership_proof(
            tx,
            plutarch_midgard_merkle_validator_input_hash_inclusion,
            MembershipProofInputs {
              tx_root: extracted_tx_root,
              tx_hash: invalid_input.output_reference.transaction_id,
              tx_info_hash: bad_input_tx_info_hash,
              tx_to_root_proof_path: bad_input_tx_to_root_proof_path,
            },
          )

        //now we know that we have an input that has a proper txHash because it's in a merkle tree
        //Validation of the redeemer info regardin the previous transaction
        verify_hash_32(
          previous_tx,
          invalid_input.output_reference.transaction_id,
        )
        verify_hash_32(previous_tx_body, previous_tx.body)
        verify_hash_32(real_outputs, previous_tx_body.outputs)
        //the previous transaction ouput size is smaller than the output index of the invalid utxo, doing -1 because size=1 means output index=0
        expect
          invalid_input.output_reference.output_index > list.length(
            builtin.un_list_data(real_outputs),
          ) - 1

        // -------- Non-existent input: validation-specific logic END --------
        expect
          validate_final_step_output(
            tx,
            index_output,
            fraud_proof_token_policy,
            ct_token_asset_name,
            step_datum,
            computation_thread_redeemer_index,
          )

        True
      }
    }
  }

  else(_) {
    fail
  }
}
