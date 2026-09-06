use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/redeemer_item_proof_v1 as item
use midgard/script_sources_item_normalization as normalized
use midgard/script_sources_redeemer_normalization_v1 as envelope

pub type Datum =
  ct.StepDatum<Data>

pub type Action {
  NormalizeOuter { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

validator main(
  deployment_id: ByteArray,
  computation_thread_policy_id: PolicyId,
  source_authenticator_script_hash: ScriptHash,
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
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(action) -> {
        let NormalizeOuter { input_index, output_index } = action
        expect Some(step_datum) = datum
        continue(
          computation_thread_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(
            input_script_hash,
            _token_name,
            _prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(raw_state) = input_state
            expect state: normalized.TraversalChecked = raw_state
            expect state.envelope.deployment_id == deployment_id
            expect
              input_script_hash == state.envelope.outer_normalizer_script_hash
            let expected: Data = if state.checking_next {
              expect next: item.RedeemerItemProofControlV1 = state.claimed_next
              expect
                item.hash_outer_with_checked_optional_traversal(
                  next,
                  state.checked_optional_traversal_cbor,
                ) == state.envelope.expected_next_item_control_hash
              expect output_script_hash == source_authenticator_script_hash
              let result: Data = normalized.ControlsChecked {
                envelope: state.envelope,
                current_control: state.current_control,
                next_control: next,
                witness_hash: state.witness_hash,
              }
              result
            } else {
              expect state.current_control.mode == item.mode_data
              expect
                state.current_control.item_index == state.envelope.redeemer_count
              expect
                state.current_control.item_count == state.envelope.redeemer_total_count
              expect
                item.hash_outer_with_checked_optional_traversal(
                  state.current_control,
                  state.checked_optional_traversal_cbor,
                ) == state.envelope.current_pending_item_control_hash
              if state.envelope.action_family == envelope.invalid_family {
                expect state.envelope.carrier == envelope.script_sources_carrier
                expect state.claimed_next == builtin.constr_data(1, [])
                expect output_script_hash == source_authenticator_script_hash
                let result: Data = normalized.ControlsChecked {
                  envelope: state.envelope,
                  current_control: state.current_control,
                  next_control: state.current_control,
                  witness_hash: state.witness_hash,
                }
                result
              } else {
                expect
                  output_script_hash == state.envelope.traversal_normalizer_script_hash
                let result: Data = normalized.CurrentChecked {
                  envelope: state.envelope,
                  current_control: state.current_control,
                  claimed_next: state.claimed_next,
                  witness_hash: state.witness_hash,
                }
                result
              }
            }
            output_state == expected
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
