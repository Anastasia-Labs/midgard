use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_redeemer/rule
use midgard/fraud_proofs/missing_redeemer/step_02.{Datum, SpendRedeemer}

validator main(
  next_step_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  field_preimage_certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          computation_thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        let Pair(initial, _) = rule.initial_scan_v1(
          state,
          args.opening,
          tx.reference_inputs,
          field_preimage_certificate_policy,
        )
        let expected: Data = initial
        and {
          output_hash == next_step_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
