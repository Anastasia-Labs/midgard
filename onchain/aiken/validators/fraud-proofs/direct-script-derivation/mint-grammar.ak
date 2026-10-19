use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/mint_purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as door
use midgard/fraud_proofs/field_opening_v1.{mint_field_index}

validator main(
  mint_scan_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<door.MintDatum>,
    redeemer: door.MintGrammarRedeemer,
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
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        expect and {
            !state.scan.complete,
            state.scan.selected == None,
            state.scan.previous_policy == #"",
            args.budget <= door.scan_batch,
          }
        let (advanced, _, complete) = cursor.certify_with_completion_at_v1(
          state.scan.cursor,
          mint_field_index,
          args.opening,
          args.checkpoint_bytes,
          args.budget,
          tx.reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let expected: Data = door.MintStateV1 {
          ..state,
          scan: mint_purpose.StateV1 {
            ..state.scan,
            cursor: advanced,
            complete,
          },
        }
        and {
          output_hash == if advanced.phase == cursor.Walking {
            mint_scan_script_hash
          } else {
            own_hash
          },
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
