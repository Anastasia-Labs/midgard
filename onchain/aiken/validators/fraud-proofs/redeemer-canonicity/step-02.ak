use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{WitnessAnchor, opened_field_view}
use midgard/fraud_proofs/redeemer_canonicity/rule.{
  authenticate_item_v1, redeemer_field_index,
}
use midgard/fraud_proofs/redeemer_canonicity/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{field_item_at}

validator main(
  step_03_hash: ScriptHash,
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
        let _, _, _, input_state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = input_state
        let view = opened_field_view(
          opening,
          WitnessAnchor {
            tx_id: bound.subject.transaction_id,
            witness_set_hash: bound.witness_set_hash,
          },
          redeemer_field_index,
          reference_inputs,
          certificate_policy,
        )
        let expected: Data = authenticate_item_v1(
          bound,
          field_item_at(view, bound.redeemer_index),
        )
        and {
          next_hash == step_03_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
