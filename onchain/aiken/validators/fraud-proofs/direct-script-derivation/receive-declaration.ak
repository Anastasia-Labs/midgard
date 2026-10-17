use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  PurposeCoordinateV1,
}
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_declaration as door
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}

validator main(
  next_step_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<door.Datum>,
    redeemer: door.SpendRedeemer,
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
        expect Some(step_datum) = datum
        let Pair(input_index, output_index) = door.indices(action)
        let own_hash, _, fraud_prover, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        when action is {
          door.Scan { entries, .. } -> {
            // Each proposed leaf is a free declaration choice until the final
            // equality. Only its owner may commit that irreversible choice.
            expect list.has(tx.extra_signatories, fraud_prover)
            let expected: Data = receive.advance_declaration_v1(state, entries)
            and {
              output_hash == own_hash,
              output_state == expected,
            }
          }
          door.Finish { .. } -> {
            expect Some(index) = state.target_ordinal
            expect index >= 0
            let expected: Data = SelectionV1 {
              bound: state.bound,
              source_index: state.source_index,
              coordinate: PurposeCoordinateV1 { kind: 3, index },
              purpose: when receive.selected_purpose_v1(state) is {
                Some(selected) -> Some(selected.purpose)
                None -> None
              },
            }
            and {
              output_hash == next_step_script_hash,
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
