use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_item_width_illegal/rule.{
  AuthenticatedWidthV1, authenticate_item_width_v1,
}
use midgard/fraud_proofs/field_item_width_illegal/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/field_opening_v1.{BodyAnchor, opened_field_view}
use midgard/native_tx_field_access_v1.{field_item_at}

fn output_state(authenticated: AuthenticatedWidthV1) -> Data {
  let value: Data = authenticated
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
        let
            _own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            input_state,
            next_script_hash,
            next_state
          <- continue(
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
          bound.field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let authenticated = authenticate_item_width_v1(
          bound,
          field_item_at(view, bound.item_index),
        )
        and {
          next_script_hash == step_03_validator_script_hash,
          next_state == output_state(authenticated),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
