use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, opened_field_view}
use midgard/fraud_proofs/transaction_output_non_canonical/rule.{
  OutputScanStateV1, initial_scan_v1, outputs_field_index,
}
use midgard/fraud_proofs/transaction_output_non_canonical/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/native_tx_field_access_v1.{field_item_at}

fn output_state(state: OutputScanStateV1) -> Data {
  let value: Data = state
  value
}

validator main(
  step_03_validator_script_hash: ScriptHash,
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
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _own, _name, _prover, input_state, next_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        expect Some(bound) = input_state
        let view = opened_field_view(
          opening,
          BodyAnchor { tx_id: bound.subject.transaction_id },
          outputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let state = initial_scan_v1(
          bound,
          field_item_at(view, bound.output_index),
        )
        and {
          next_hash == step_03_validator_script_hash,
          next_state == output_state(state),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
