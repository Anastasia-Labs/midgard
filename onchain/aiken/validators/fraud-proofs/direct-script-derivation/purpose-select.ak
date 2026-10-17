use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as door
use midgard/fraud_proofs/field_opening_v1.{spend_inputs_field_index}

validator main(
  source_lookup_script_hash: ScriptHash,
  spend_scan_script_hash: ScriptHash,
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
        let _, _, fraud_prover, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        expect list.has(tx.extra_signatories, fraud_prover)
        when action is {
          door.Spend {
            raw_item_index,
            opening,
            descriptor_cbor,
            membership,
            ..
          } -> {
            expect Some(selected) = purpose.open_spend_v1(
              state.bound,
              raw_item_index,
              opening,
              descriptor_cbor,
              membership,
              tx.reference_inputs,
              tx.redeemers,
              field_preimage_certificate_policy_id,
            )
            let (field_cursor, _, complete) = cursor.initial_with_completion_v1(
              state.bound,
              spend_inputs_field_index,
              opening,
              tx.reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let expected: Data = door.SpendStateV1 {
              source_index: state.source_index,
              cursor: field_cursor,
              raw_item_index,
              target: selected,
              rank: 0,
              complete,
            }
            and {
              output_hash == spend_scan_script_hash,
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
