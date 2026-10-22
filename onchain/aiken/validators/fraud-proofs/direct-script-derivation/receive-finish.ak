use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_finish as door

validator main(
  receive_declaration_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let _, _, fraud_prover, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        let finished = receive.finish_outputs_v1(
          state,
          args.outputs_opening,
          args.checkpoint_bytes,
          tx.reference_inputs,
          field_preimage_certificate_policy_id,
        )
        // Target choice and sorted declaration batches need their owner's
        // signature. Deterministic grammar/output progress stays public.
        expect Some(index) = args.target_ordinal
        expect index >= 0
        expect list.has(tx.extra_signatories, fraud_prover)
        let expected: Data = receive.initial_declaration_v1(
          finished,
          args.target_ordinal,
        )
        output_hash == receive_declaration_script_hash && output_state == expected
      }
    }
  }

  else(_) {
    fail
  }
}
