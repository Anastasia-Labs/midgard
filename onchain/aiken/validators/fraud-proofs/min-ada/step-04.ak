//// Q27 min-Ada UTxO predecessor step, separated so the maximum published MPF
//// evidence remains below the release execution reserve.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{
  published_chunk_non_membership_four_chunks,
}
use midgard/fraud_proofs/common.{
  PublishedChunkNonMembership, RedeemerCarriedNonMembership, cancel, continue,
  verify_non_membership_carried,
}
use midgard/fraud_proofs/min_ada/step_04.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/min_ada/step_05 as step_05

validator main(
  step_05_validator_script_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, predecessor_non_membership }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(state) = m_input_state_data
        expect output_script_hash == step_05_validator_script_hash
        let expected_output_state: Data = step_05.PredicateAndCulpabilityAuthenticated
        expect output_state_data == expected_output_state
        expect
          when predecessor_non_membership is {
            RedeemerCarriedNonMembership { .. } -> {
              let Transaction { redeemers, reference_inputs, .. } = tx
              verify_non_membership_carried(
                predecessor_non_membership,
                state.prev_utxos_root,
                state.out_ref_key,
                reference_inputs,
                redeemers,
              )
            }
            PublishedChunkNonMembership(published) -> {
              let Transaction { reference_inputs, .. } = tx
              published_chunk_non_membership_four_chunks(
                reference_inputs,
                published,
                state.prev_utxos_root,
                state.out_ref_key,
              )
            }
          }
        True
      }
    }
  }

  else(_) {
    fail
  }
}
