use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as door

/// Opens the spend the thread owner selected at the selection door, which
/// hands over its state unchanged, and starts the spends-only rank pass.
validator main(
  spend_scan_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<door.SelectionDatum>,
    redeemer: door.SpendSelectionRedeemer,
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
        let _, _, _, input_state, output_hash, output_state <- continue(
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
        let expected: Data = door.initial_spend_v1(
          state,
          args,
          tx.reference_inputs,
          tx.redeemers,
          field_preimage_certificate_policy_id,
        )
        and {
          output_hash == spend_scan_script_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
