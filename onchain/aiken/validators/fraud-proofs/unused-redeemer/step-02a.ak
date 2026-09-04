use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/unused_redeemer/rule.{authenticate_control_v1}
use midgard/fraud_proofs/unused_redeemer/step_02a.{Datum, SpendRedeemer}

validator main(step_02b_hash: ScriptHash, thread_policy: PolicyId) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(args) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          args.input_index,
          args.output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(authenticated) = input_state
        let expected: Data = authenticate_control_v1(
          authenticated,
          args.machine_state,
          args.trace_proof,
          args.control,
        )
        and {
          output_hash == step_02b_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
