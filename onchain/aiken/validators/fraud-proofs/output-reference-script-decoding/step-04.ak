use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, opened_field_view}
use midgard/fraud_proofs/output_reference_script_decoding/rule.{
  bind_reference_script_v1, output_canonical, outputs_field_index,
}
use midgard/fraud_proofs/output_reference_script_decoding/step_04.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{field_item_at}

validator main(
  step_05_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  certificate_policy: PolicyId,
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
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _own_hash, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(state) = input_state
        expect state.outcome == output_canonical
        let view = opened_field_view(
          opening,
          BodyAnchor { tx_id: state.bound.subject.transaction_id },
          outputs_field_index,
          reference_inputs,
          certificate_policy,
        )
        let scan = bind_reference_script_v1(
          state,
          field_item_at(view, state.bound.output_index),
          step_05_hash,
        )
        let expected: Data = scan
        and {
          next_hash == step_05_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
