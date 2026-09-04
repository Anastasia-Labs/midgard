use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  NativeTxAnchorV1, WitnessAnchor, resume_opened_field_walk,
  script_witnesses_field_index,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_05.{
  SemanticScan, State, staged_batch_limit,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_07.{
  Datum, FinalizeSemanticScan, ResumeSemanticScan, SpendRedeemer,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/native_tx_machine_walk_v1.{
  field_walk_checkpoint_hash, walk_fold, walk_is_complete,
}
use midgard/script_proof_v1

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(action) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        when action is {
          ResumeSemanticScan {
            input_index,
            output_index,
            script_tx_wits_opening,
            checkpoint_bytes,
            item_budget,
          } -> {
            expect expect_valid_staged_budget(item_budget)
            let
                own_script_hash,
                _thread_asset_name,
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
            expect SemanticScan { checkpoint_hash, required_script_is_present }
              = state.phase
            let Pair(view, resumed) = resume_opened_field_walk(
              script_tx_wits_opening,
              anchor_of(state),
              script_witnesses_field_index,
              checkpoint_hash,
              checkpoint_bytes,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let Pair(found, next) = walk_fold(
              view,
              resumed,
              item_budget,
              required_script_is_present,
              fn(found, _index, item) {
                scan_script_witness(
                  state.expected_missing_script_hash,
                  found,
                  item,
                )
              },
            )
            expect !walk_is_complete(next)
            expect output_script_hash == own_script_hash
            expect_output_state(
              output_state_data,
              State {
                ..state,
                phase: SemanticScan {
                  checkpoint_hash: field_walk_checkpoint_hash(next),
                  required_script_is_present: found,
                },
              },
            )
          }
          FinalizeSemanticScan {
            input_index,
            output_index,
            fraud_proof_mint_redeemer_index,
            script_tx_wits_opening,
            checkpoint_bytes,
            item_budget,
          } -> {
            expect expect_valid_staged_budget(item_budget)
            let
                _own_script_hash,
                _thread_asset_name,
                _fraud_prover,
                m_input_state_data
              <- finalize(
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
            expect Some(state) = m_input_state_data
            expect SemanticScan { checkpoint_hash, required_script_is_present }
              = state.phase
            let Pair(view, resumed) = resume_opened_field_walk(
              script_tx_wits_opening,
              anchor_of(state),
              script_witnesses_field_index,
              checkpoint_hash,
              checkpoint_bytes,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let Pair(found, terminal) = walk_fold(
              view,
              resumed,
              item_budget,
              required_script_is_present,
              fn(found, _index, item) {
                scan_script_witness(
                  state.expected_missing_script_hash,
                  found,
                  item,
                )
              },
            )
            expect walk_is_complete(terminal)
            expect found == False
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

fn anchor_of(state: State) -> NativeTxAnchorV1 {
  WitnessAnchor {
    tx_id: state.bad_tx_id,
    witness_set_hash: state.bad_tx_witness_set_hash,
  }
}

fn expect_valid_staged_budget(item_budget: Int) -> Bool {
  expect item_budget > 0
  expect item_budget <= staged_batch_limit
  True
}

fn expect_output_state(output_state_data: Data, expected_state: State) -> Bool {
  let expected: Data = expected_state
  expect output_state_data == expected
  True
}

fn scan_script_witness(
  expected_missing_script_hash: ByteArray,
  found: Bool,
  item: ByteArray,
) -> Bool {
  let Pair(_offset, script_wit) = decode_midgard_versioned_script_at(item, 0)
  expect encode_midgard_versioned_script(script_wit) == item
  or {
    found,
    script_proof_v1.versioned_script_hash(script_wit) == expected_missing_script_hash,
  }
}
