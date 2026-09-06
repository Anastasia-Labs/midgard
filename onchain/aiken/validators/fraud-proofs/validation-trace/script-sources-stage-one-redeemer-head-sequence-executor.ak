use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/script_sources_item_normalization as normalized
use midgard/script_sources_item_semantics as semantics

pub type Datum =
  ct.StepDatum<normalized.TraversalExecution>

pub type Action {
  Execute { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

validator main(
  deployment_id: ByteArray,
  computation_thread_policy_id: PolicyId,
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
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Execute { input_index, output_index }) -> {
        expect Some(step_datum) = datum
        continue(
          computation_thread_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(
            input_script_hash,
            _token_name,
            _prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(state) = input_state
            and {
              normalized.execution_output_is_exact(
                state.output,
                deployment_id,
                4,
                input_script_hash,
                output_script_hash,
                output_state,
              ),
              semantics.head_sequence(state),
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
