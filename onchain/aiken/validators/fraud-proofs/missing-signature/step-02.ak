use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/crypto.{ScriptHash, Signature, VerificationKey, VerificationKeyHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_signature/step_02.{Args, Datum, SpendRedeemer}
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

      ct.Continue(tx_inclusion_and_bad_addr_with_args) -> {
        let Args {
          tx_inclusion_args,
          bad_tx_wits,
          bad_required_signer_hash_index,
          bad_required_signer_hash,
          bad_required_signer_membership_proof,
          plutarch_phas_withdraw_redeemer_index,
        } = tx_inclusion_and_bad_addr_with_args

        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
          _own_script_hash,
          _computation_thread_token_asset_name,
          _fraud_prover,
          _m_input_state_data,
          output_script_hash,
          output_state_data,
          _header,
          _bad_tx_id,
          bad_tx,
          bad_tx_field_preimage,
        <-
          pass_tx_to_next_step(
            computation_thread_token_policy_id,
            hub_oracle,
            datum,
            tx_inclusion_args,
            own_out_ref,
            tx,
          )

        // 2. The redeemer must provide preimages of both the bad transaction's
        //    body and witness set.
        expect TxBodyAndWitsProvided(bad_tx_body, bad_tx_wits) =
          bad_tx_field_preimage

        let expected_output_state: Data =
          step_02.State {
            verified_required_signers_root: bad_tx_body.required_signer_hashes,
            bad_addr_tx_wits,
            bad_required_signer_hash,
          }

        // 4. The signer hash must be in the required signer hashes
        expect
          utils.plutarch_phas(
            bad_tx_body.required_signer_hashes,
            bad_required_signer_hash_index,
            bad_required_signer_hash,
            bad_required_signer_membership_proof,
            tx.redeemers,
            plutarch_phas_withdraw_redeemer_index,
          )

        // 4. The UTxO must be produced at next step's script hash
        let bad_addr_tx_wits = get_addr_tx_wits_merkle_tree_root(bad_tx_wits)
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State { bad_addr_tx_wits, bad_required_signer_hash }
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
