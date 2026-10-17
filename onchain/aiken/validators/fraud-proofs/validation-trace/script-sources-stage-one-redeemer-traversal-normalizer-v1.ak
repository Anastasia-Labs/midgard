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
  NormalizeCurrent {
    input_index: Int,
    output_index: Int,
    auxiliary: Data,
    claimed_next: Data,
  }
  NormalizeNext { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

validator main(
  deployment_id: ByteArray,
  computation_thread_policy_id: PolicyId,
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
        let Pair(input_index, output_index) = when action is {
          NormalizeCurrent { input_index, output_index, .. } ->
            Pair(input_index, output_index)
          NormalizeNext { input_index, output_index } ->
            Pair(input_index, output_index)
        }
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
            let checked = when action is {
              NormalizeCurrent { auxiliary, claimed_next, .. } -> {
                expect state: envelope.PreparedScriptSourcesRedeemerEnvelopeV1 = raw_state
                expect
                  envelope.envelope_state_is_bound_v1(
                    state,
                    deployment_id,
                    state.envelope_binder_script_hash,
                  )
                expect
                  input_script_hash == state.traversal_normalizer_script_hash
                expect
                  envelope.canonical_auxiliary_hash_v1(auxiliary) == state.canonical_auxiliary_hash
                expect
                  envelope.canonical_action_hash_v1(
                    auxiliary,
                    state.action_family,
                  ) == state.canonical_action_hash
                let Pair(current_data, witness_data) = normalized.auxiliary_item_fields(
                  auxiliary,
                )
                expect current: item.RedeemerItemProofControlV1 = current_data
                expect
                  state.carrier == envelope.script_sources_carrier || normalized.raw_data_hash(
                    claimed_next,
                  ) == state.expected_next_control_data_hash
                normalized.TraversalChecked {
                  checking_next: False,
                  envelope: state,
                  current_control: current,
                  claimed_next,
                  witness_hash: normalized.raw_data_hash(witness_data),
                  checked_optional_traversal_cbor: normalized.checked_optional_traversal_cbor(
                    current,
                  ),
                }
              }
              NormalizeNext { .. } -> {
                expect state: normalized.CurrentChecked = raw_state
                expect state.envelope.deployment_id == deployment_id
                expect
                  input_script_hash == state.envelope.traversal_normalizer_script_hash
                expect next: item.RedeemerItemProofControlV1 = state.claimed_next
                normalized.TraversalChecked {
                  checking_next: True,
                  envelope: state.envelope,
                  current_control: state.current_control,
                  claimed_next: state.claimed_next,
                  witness_hash: state.witness_hash,
                  checked_optional_traversal_cbor: normalized.checked_optional_traversal_cbor(
                    next,
                  ),
                }
              }
            }
            let expected: Data = checked
            and {
              output_script_hash == checked.envelope.outer_normalizer_script_hash,
              output_state == expected,
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
