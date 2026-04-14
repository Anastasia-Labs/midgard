use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/crypto.{ScriptHash, Signature, VerificationKey, VerificationKeyHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/types.{MerkleRoot} as midgard_types
use midgard/common/utils.{verify_hash_32}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_tx_to_next_step}
use midgard/fraud_proofs/invalid_signature/step_01.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/invalid_signature/step_02
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
      ct.Continue(tx_inclusion_args) -> {
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

        expect TxBodyAndWitsProvided(bad_tx_body, bod_tx_wits) =
          bad_tx_field_preimage

        // 2. The UTxO must be produced at next step's script hash with the bad
        //    transactin's body and address witness set root.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data =
          step_02.State {
            bad_tx_body,
            bad_addr_tx_wits_root: bod_tx_wits.addr_tx_wits,
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
