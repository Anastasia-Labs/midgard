//// Q27 min-Ada step 01: authenticate either the accepted transaction source
//// or the post-block descriptor membership and freeze the predecessor root.

use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/chunked_inclusion_v1.{PublishedProofCarriage}
use midgard/fraud_proofs/common.{
  PublishedChunkInclusion, PublishedChunkMembership, RedeemerCarriedInclusion,
  RedeemerCarriedMembership, cancel, continue, verify_membership_carried,
  verify_native_tx_in_state_queue_node_with,
}
use midgard/fraud_proofs/min_ada/step_01.{
  Args, Datum, MinAdaTx, MinAdaUtxo, PostUtxoStateV1, SpendRedeemer,
}
use midgard/fraud_proofs/min_ada/step_02 as step_02
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/hub_oracle as hub
use midgard/rejection_reason_v1.{OutputBelowMinAda}
use midgard/state_queue

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
        forced_source,
        tx_inclusion,
        post_utxo_membership,
        fault,
      }) -> {
        // All three claim routes authenticate the same thread and successor.
        // Share that work to keep the applied script inside the publication
        // budget, while preserving each route's independent source binding.
        let (input_index, output_index) = when forced_source is {
          Some(source) -> (source.input_index, source.output_index)
          None ->
            when tx_inclusion is {
              Some(RedeemerCarriedInclusion(source)) ->
                (source.input_index, source.output_index)
              Some(PublishedChunkInclusion(source)) ->
                (source.input_index, source.output_index)
              None -> {
                expect Some(source) = post_utxo_membership
                (source.input_index, source.output_index)
              }
            }
        }
        expect Some(step_datum) = datum
        let _, name, _, prior, output_hash, output_data <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect None = prior
        expect output_hash == step_02_validator_script_hash
        let (direction, bad_tx_id, post_utxo) = when forced_source is {
          Some(source) -> {
            expect None = tx_inclusion
            expect None = post_utxo_membership
            expect MinAdaTx { output_index: claimed_index } = fault
            let subject = substrate.bind_forced_subject_to_thread_v1(
              name,
              source.header,
              source.membership,
              source.direction,
            )
            expect subject.direction == substrate.direction_wrongful_rejection
            expect Some(OutputBelowMinAda { output_index }) = subject.rejection_reason
            expect output_index >= 0 && output_index == claimed_index
            (subject.direction, subject.transaction_id, None)
          }
          None ->
            when fault is {
              MinAdaTx { output_index } -> {
                expect output_index >= 0
                expect None = post_utxo_membership
                expect Some(inclusion) = tx_inclusion
                let (tx_id, source_cbor, root, hub_index, state_index, carriage)
                  = when inclusion is {
                  RedeemerCarriedInclusion(source) ->
                    (
                      source.native_tx_id,
                      source.l2_transaction_source_cbor,
                      source.transactions_phas_root,
                      source.hub_ref_input_index,
                      source.state_queue_node_ref_input_index,
                      RedeemerCarriedMembership {
                        membership_proof: source.tx_membership_proof,
                        membership_proof_script_redeemer_index: source.inclusion_proof_script_withdraw_redeemer_index,
                      },
                    )
                  PublishedChunkInclusion(source) ->
                    (
                      source.native_tx_id,
                      source.l2_transaction_source_cbor,
                      source.transactions_phas_root,
                      source.hub_ref_input_index,
                      source.state_queue_node_ref_input_index,
                      PublishedChunkMembership(
                        PublishedProofCarriage {
                          ordered_chunk_reference_input_indices: source.ordered_chunk_reference_input_indices,
                        },
                      ),
                    )
                }
                let _, _, verified <- verify_native_tx_in_state_queue_node_with(
                  tx_id,
                  source_cbor,
                  root,
                  name,
                  hub_oracle,
                  hub_index,
                  state_index,
                  tx.reference_inputs,
                  fn(authenticated_root, key, value) {
                    verify_membership_carried(
                      carriage,
                      authenticated_root,
                      key,
                      value,
                      tx.reference_inputs,
                      tx.redeemers,
                    )
                  },
                )
                expect verified.tx_compact.validity_code == Some(0)
                (0, tx_id, None)
              }
              MinAdaUtxo -> {
                expect None = tx_inclusion
                expect Some(membership) = post_utxo_membership
                let hub.Datum { state_queue: state_queue_policy, .. } = hub.get_datum(
                  tx.reference_inputs,
                  hub_oracle,
                  membership.hub_ref_input_index,
                )
                let header, state_queue_node_key <- state_queue.get_block_datum_v1(
                  tx.reference_inputs,
                  state_queue_policy,
                  membership.state_queue_node_ref_input_index,
                )
                expect
                  state_queue_node_key == bytearray.drop(
                    name,
                    fraud_proof_catalogue.id_byte_count,
                  )
                (
                  0,
                  membership.out_ref.transaction_id,
                  Some(
                    PostUtxoStateV1 {
                      out_ref: membership.out_ref,
                      descriptor_cbor: membership.descriptor_cbor,
                      post_utxos_root: header.utxos_root,
                      prev_utxos_root: header.prev_utxos_root,
                    },
                  ),
                )
              }
            }
        }
        let expected: Data = step_02.State {
          grammar_checkpoint_hash: #"",
          grammar_complete: False,
          walk_checkpoint_hash: #"",
          direction,
          bad_tx_id,
          fault,
          post_utxo,
        }
        output_data == expected
      }
    }
  }

  else(_) {
    fail
  }
}
