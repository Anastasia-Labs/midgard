use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  ContinueDatum, FinishDatum, advance_datum_v1,
}
use midgard/fraud_proofs/transaction_output_non_canonical/step_03_datum.{
  Args, Datum, SpendRedeemer,
}

validator main(
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
      ct.Continue(Args {
        input_index,
        output_index,
        window,
        revealed,
        tail_root,
        node_budget,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        when
          advance_datum_v1(state, window, revealed, tail_root, node_budget)
        is {
          ContinueDatum(next) -> {
            let value: Data = next
            and {
              next_hash == own_hash,
              next_state == value,
            }
          }
          FinishDatum(scan) -> {
            let value: Data = scan
            and {
              next_hash == step_04_validator_script_hash,
              next_state == value,
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
