use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/crypto.{ScriptHash, Signature, VerificationKey, VerificationKeyHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/types.{MerkleRoot} as midgard_types
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/missing_signature/step_01.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/missing_signature/step_02
use midgard/ledger_state.{MidgardTxWitnessSetCompact}

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
          step_02.State {
            verified_required_signers_root: bad_required_signer_hash,
            verified_addr_tx_wits: bad_addr_tx_wits,
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
