use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{
  ContinueDatum, FinishDatum, advance_datum_v1,
}
use midgard/fraud_proofs/resolved_output_non_canonical/step_04_datum.{
  Args, Datum, SpendRedeemer,
}

validator main(step_05_hash: ScriptHash, thread_policy: PolicyId) {
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
      ct.Continue(Args {
        input_index,
        output_index,
        chunk_proof,
        next_chunk_proof,
        revealed,
        tail_root,
        node_budget,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let own_hash, _, _, input_state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(state) = input_state
        when
          advance_datum_v1(
            state,
            chunk_proof,
            next_chunk_proof,
            revealed,
            tail_root,
            node_budget,
          )
        is {
          ContinueDatum { state: next } -> {
            let value: Data = next
            and {
              next_hash == own_hash,
              next_state == value,
            }
          }
          FinishDatum { verdict } -> {
            let value: Data = verdict
            and {
              next_hash == step_05_hash,
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
