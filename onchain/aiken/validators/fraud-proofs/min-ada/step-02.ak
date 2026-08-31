//// Q27 min-Ada step 02: transaction predicate or post-root UTxO membership.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use env
use midgard/computation_thread as ct
use midgard/fraud_proofs/chunked_inclusion_v1.{
  published_chunk_membership_four_chunks,
}
use midgard/fraud_proofs/common.{
  PublishedChunkMembership, RedeemerCarriedMembership, cancel, continue,
  verify_membership_carried,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, outputs_field_index,
}
use midgard/fraud_proofs/min_ada/step_01.{MinAdaTx, MinAdaUtxo}
use midgard/fraud_proofs/min_ada/step_02.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/min_ada/step_03 as step_03
use midgard/fraud_proofs/min_ada/step_05 as step_05
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_descriptor_v1
use midgard/native_tx_field_access_v1.{field_item_at}
use midgard/validation_machine_v1

validator main(
  step_03_validator_script_hash: ScriptHash,
  step_05_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
        outputs_opening,
        post_membership,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
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
        when state.fault is {
          MinAdaTx { output_index: bad_output_index } -> {
            expect Some(opening) = outputs_opening
            expect None = post_membership
            expect None = state.post_utxo
            expect output_script_hash == step_05_validator_script_hash
            let expected_output_state: Data = step_05.PredicateAndCulpabilityAuthenticated
            expect output_state_data == expected_output_state
            let outputs_view = opened_field_view(
              opening,
              BodyAnchor { tx_id: state.bad_tx_id },
              outputs_field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            expect Some(built) = ledger_output_descriptor_v1.build_v1(
              bad_output_index,
              field_item_at(outputs_view, bad_output_index),
            )
            !validation_machine_v1.output_meets_min_ada_v1(
              env.coins_per_utxo_byte,
              built.total_length,
              built.lovelace,
            )
          }
          MinAdaUtxo -> {
            expect None = outputs_opening
            expect Some(membership) = post_membership
            expect Some(post) = state.post_utxo
            expect output_script_hash == step_03_validator_script_hash
            let expected_output_state: Data = step_03.State {
              descriptor_cbor: post.descriptor_cbor,
              out_ref_key: ledger_outref_key(post.out_ref),
              prev_utxos_root: post.prev_utxos_root,
            }
            expect output_state_data == expected_output_state
            expect
              when membership is {
                RedeemerCarriedMembership { .. } -> {
                  let Transaction { redeemers, .. } = tx
                  verify_membership_carried(
                    membership,
                    post.post_utxos_root,
                    ledger_outref_key(post.out_ref),
                    post.descriptor_cbor,
                    reference_inputs,
                    redeemers,
                  )
                }
                PublishedChunkMembership(published) ->
                  published_chunk_membership_four_chunks(
                    reference_inputs,
                    published,
                    post.post_utxos_root,
                    ledger_outref_key(post.out_ref),
                    post.descriptor_cbor,
                  )
              }
            // Predicate adjudication is deliberately deferred to step 03 so
            // maximum post-root membership and descriptor work never share a
            // transaction budget.
            True
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
