use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as door
use midgard/fraud_proofs/first_fault_v1.{PurposeCoordinateV1}

validator main(
  source_lookup_script_hash: ScriptHash,
  spend_select_script_hash: ScriptHash,
  mint_grammar_script_hash: ScriptHash,
  receive_start_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<door.SelectionDatum>,
    redeemer: door.SelectionRedeemer,
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
        let Pair(input_index, output_index) = door.selection_indices_v1(action)
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        when action is {
          door.Spend { .. } -> {
            let expected: Data = state
            and {
              output_hash == spend_select_script_hash,
              output_state == expected,
            }
          }
          door.Observe { purpose_index, opening, .. } -> {
            let selected = purpose.open_observe_v1(
              state.bound,
              purpose_index,
              opening,
              tx.reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let expected: Data = door.selected_v1(
              state,
              PurposeCoordinateV1 {
                kind: purpose.observe,
                index: purpose_index,
              },
              Some(selected),
            )
            and {
              output_hash == source_lookup_script_hash,
              output_state == expected,
            }
          }
          door.Mint { purpose_index, opening, .. } -> {
            let expected: Data = door.initial_mint_v1(
              state,
              purpose_index,
              opening,
              tx.reference_inputs,
              field_preimage_certificate_policy_id,
            )
            and {
              output_hash == mint_grammar_script_hash,
              output_state == expected,
            }
          }
          door.Receive { .. } -> {
            let expected: Data = state
            and {
              output_hash == receive_start_script_hash,
              output_state == expected,
            }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
