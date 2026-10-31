use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use env
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/receive.{OutputStateV1}
use midgard/fraud_proofs/direct_script_derivation/receive_grammar.{
  Datum, SpendRedeemer,
}
use midgard/fraud_proofs/field_opening_v1.{outputs_field_index}

validator main(
  receive_outputs_script_hash: ScriptHash,
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
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        expect and {
            !state.complete,
            state.credential_count == 0,
            state.credential_root == env.empty_merkle_tree_root,
          }
        let (advanced, _, complete) = cursor.certify_with_completion_at_v1(
          state.cursor,
          outputs_field_index,
          args.outputs_opening,
          args.checkpoint_bytes,
          args.budget,
          tx.reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let expected: Data = OutputStateV1 {
          ..state,
          cursor: advanced,
          complete,
        }
        and {
          output_hash == if advanced.phase == cursor.Walking {
            receive_outputs_script_hash
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
